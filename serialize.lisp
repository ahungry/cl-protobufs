;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-IMPL")


;;; Protobuf serialization from Lisp objects

;;; Serialization

;; Serialize the object using the given protobuf "schema"
(defun serialize-object-to-stream (object protobuf &key (stream *standard-output*) visited)
  "Serializes the object 'object' as a protobuf object defined in the schema 'protobuf'
   onto the stream 'stream' using the wire format.
   'visited' is a hash table used to cache object sizes. If it is supplied, it will be
   cleared before it is used; otherwise, a fresh table will be created.
   The return value is the buffer containing the serialized object. If the stream is
   nil, the buffer is not actually written to anywhere."
  (let* ((visited (let ((v (or visited (make-hash-table))))
                    (clrhash v)
                    v))
         (size    (object-size object protobuf :visited visited))
         (buffer  (make-array size :element-type '(unsigned-byte 8))))
    (serialize-object object protobuf buffer 0 :visited visited)
    (when stream
      (write-sequence buffer stream))
    buffer))

;; Allow clients to add their own methods
;; This is how we address the problem of cycles, e.g. -- if you have an object
;; that may contain cycles, serialize the cyclic object using a "handle"
(defgeneric serialize-object (object protobuf buffer index &key visited)
  (:documentation
   "Serializes the object 'object' as a protobuf object defined in the schema 'protobuf'
    into the byte array given by 'buffer' starting at the fixnum index 'index' using
    the wire format.
    'visited' is a hash table used to cache object sizes.
    The return value is the buffer containing the serialized object."))

;; 'visited' is used to cache object sizes
;; If it's passed in explicitly, it is assumed to already have the sizes within it
;; The default method uses meta-data from the protobuf "schema"
(defmethod serialize-object ((object standard-object) protobuf buffer index &key visited)
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (check-type protobuf (or protobuf protobuf-message))
  (let* ((class   (class-of object))
         (message (find-message-for-class protobuf class))
         (visited (or visited (make-hash-table))))
    (assert message ()
            "There is no Protobuf message for the class ~S" class)
    (macrolet ((read-slot (object slot reader)
                 ;; Don't do a boundp check, we assume the object is fully populated
                 ;; Unpopulated slots should be "nullable" and should contain nil
                 `(if ,reader
                    (funcall ,reader ,object)
                    (slot-value ,object ,slot))))
      (labels ((do-field (object trace field)
                 ;; We don't do cycle detection here
                 ;; If the client needs it, he can define his own 'serialize-object'
                 ;; method to clean things up first
                 (let* ((cl     (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                        (slot   (proto-value field))
                        (reader (proto-reader field))
                        msg)
                   (when (or slot reader)
                     (cond ((eq (proto-required field) :repeated)
                            (cond ((and (proto-packed field) (packed-type-p cl))
                                   (let ((tag (make-tag cl (proto-index field))))
                                     (setq index (serialize-packed (read-slot object slot reader)
                                                                   cl tag buffer index))))
                                  ((keywordp cl)
                                   (let ((tag (make-tag cl (proto-index field))))
                                     (map () #'(lambda (v)
                                                 (setq index (serialize-prim v cl tag buffer index)))
                                             (read-slot object slot reader))))
                                  ((typep (setq msg (and cl (loop for p in trace
                                                                  thereis (or (find-message-for-class p cl)
                                                                              (find-enum-for-type p cl)))))
                                          'protobuf-message)
                                   (dolist (v (if slot (read-slot object slot reader) (list object)))
                                     ;; To serialize an embedded message, first say that it's
                                     ;; a string, then encode its size, then serialize its fields
                                     (let ((tag (make-tag $wire-type-string (proto-index field)))
                                           (len (object-size v protobuf :visited visited)))
                                       (setq index (encode-uint32 tag buffer index))
                                       (setq index (encode-uint32 len buffer index)))
                                     (map () (curry #'do-field v (cons msg trace))
                                             (proto-fields msg))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                     (map () #'(lambda (v)
                                                 (setq index (serialize-enum v (proto-values msg) tag buffer index)))
                                             (read-slot object slot reader))))))
                           (t
                            (cond ((keywordp cl)
                                   (let ((v (read-slot object slot reader)))
                                     (when (or v (eq cl :bool))
                                       (let ((tag (make-tag cl (proto-index field))))
                                         (setq index (serialize-prim v cl tag buffer index))))))
                                  ((typep (setq msg (and cl (loop for p in trace
                                                                  thereis (or (find-message-for-class p cl)
                                                                              (find-enum-for-type p cl)))))
                                          'protobuf-message)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (let ((tag (make-tag $wire-type-string (proto-index field)))
                                           (len (object-size v protobuf :visited visited)))
                                       (setq index (encode-uint32 tag buffer index))
                                       (setq index (encode-uint32 len buffer index)))
                                     (when v
                                       (map () (curry #'do-field v (cons msg trace))
                                               (proto-fields msg)))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((v (read-slot object slot reader)))
                                     (when v
                                       (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                         (setq index (serialize-enum v (proto-values msg) tag buffer index)))))))))))))
        (declare (dynamic-extent #'do-field))
        (map () (curry #'do-field object (list message protobuf)) (proto-fields message)))))
  (values buffer index))


;;; Deserialization

(defun deserialize-object-from-stream (class protobuf &key (stream *standard-input*))
  "Deserializes an object of the give class 'class' as a protobuf object defined
   in the schema 'protobuf' from the stream 'stream' using the wire format.
   The return value is the object."
  (let* ((size    (file-length stream))
         (buffer  (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    (deserialize-object class protobuf buffer 0)))

;; Allow clients to add their own methods
;; This is you might preserve object identity, e.g.
(defgeneric deserialize-object (class protobuf buffer &optional index length)
  (:documentation
   "Deserializes an object of the given class 'class' as a protobuf object defined
    in the schema 'protobuf' from the byte array given by 'buffer' starting at
    the fixnum index 'index' up to the length of the buffer, given by 'length'.
    The return value is the object."))

;; The default method uses meta-data from the protobuf "schema"
;; Note that 'class' is the Lisp name of the Protobufs message (class)
;; It is not the name of any overriding class ('proto-class-override')
(defmethod deserialize-object ((class symbol) protobuf buffer &optional (index 0) length)
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (check-type protobuf (or protobuf protobuf-message))
  (let ((length (or length (length buffer))))
    (declare (type fixnum length))
    (labels ((deserialize (class trace &optional (end length))
               (declare (type fixnum end))
               (let* ((message (loop for p in trace
                                     thereis (or (find-message-for-class p class)
                                                 (find-enum-for-type p class))))
                      (object  (make-instance (or (proto-class-override message) class))))
                 (assert (eql (proto-class message) class) ()
                         "The class in message ~S does not match the Lisp class ~S"
                         (proto-class message) class)
                 (assert message ()
                         "There is no Protobuf message for the class ~S" class)
                 (loop
                   (when (>= index end)
                     (return-from deserialize (values object index)))
                   (multiple-value-bind (val idx)
                       (decode-uint32 buffer index)
                     (setq index idx)
                     (let* ((type  (ilogand val #x7))
                            (fld   (ilogand (iash val -3) #x1FFFFFFF))
                            (field (find fld (proto-fields message) :key #'proto-index))
                            (cl    (and field (if (eq (proto-class field) 'boolean) :bool (proto-class field))))
                            ;; It's OK for this to be null
                            ;; That means we're parsing some version of a message
                            ;; that has the field, but our current message does not
                            ;; We still have to deserialize everything, though
                            (slot  (and field (proto-value field)))
                            msg)
                       (if (null field)
                         ;; If there's no field descriptor for this index,
                         ;; just skip the next element in the buffer
                         (setq index (skip-element buffer index type))
                         ;;--- Check for mismatched types, running past end of buffer, etc
                         (cond ((and field (eq (proto-required field) :repeated))
                                (cond ((and (proto-packed field) (packed-type-p cl))
                                       (multiple-value-bind (values idx)
                                           (deserialize-packed cl buffer index)
                                         (setq index idx)
                                         (when slot
                                           (setf (slot-value object slot) values))))
                                      ((keywordp cl)
                                       (multiple-value-bind (val idx)
                                           (deserialize-prim cl buffer index)
                                         (setq index idx)
                                         (when slot
                                           (setf (slot-value object slot)
                                                 (nconc (slot-value object slot) (list val))))))
                                      ((typep (setq msg (and cl (or (find-message-for-class protobuf cl)
                                                                    (find-enum-for-type protobuf cl))))
                                              'protobuf-message)
                                       (multiple-value-bind (len idx)
                                           (decode-uint32 buffer index)
                                         (setq index idx)
                                         (let ((obj (deserialize cl (cons msg trace) (+ index len))))
                                           (when slot
                                             (setf (slot-value object slot)
                                                   (nconc (slot-value object slot) (list obj)))))))
                                      ((typep msg 'protobuf-enum)
                                       (multiple-value-bind (val idx)
                                           (deserialize-enum (proto-values msg) buffer index)
                                         (setq index idx)
                                         (when slot
                                           (setf (slot-value object slot)
                                                 (nconc (slot-value object slot) (list val))))))))
                               (t
                                (cond ((keywordp cl)
                                       (multiple-value-bind (val idx)
                                           (deserialize-prim cl buffer index)
                                         (setq index idx)
                                         (when slot
                                           (setf (slot-value object slot) val))))
                                      ((typep (setq msg (and cl (or (find-message-for-class protobuf cl)
                                                                    (find-enum-for-type protobuf cl))))
                                              'protobuf-message)
                                       (multiple-value-bind (len idx)
                                           (decode-uint32 buffer index)
                                         (setq index idx)
                                         (let ((obj (deserialize cl (cons msg trace) (+ index len))))
                                           (when slot
                                             (setf (slot-value object slot) obj)))))
                                      ((typep msg 'protobuf-enum)
                                       (multiple-value-bind (val idx)
                                           (deserialize-enum (proto-values msg) buffer index)
                                         (setq index idx)
                                         (when slot
                                           (setf (slot-value object slot) val))))))))))))))
      (declare (dynamic-extent #'deserialize))
      (deserialize class (list protobuf)))))


;;; Object sizes

;; Allow clients to add their own methods
;; This is how we address the problem of cycles, e.g. -- if you have an object
;; that may contain cycles, return the size of the "handle" to the object
(defgeneric object-size (object protobuf &key visited)
  (:documentation
   "Computes the size in bytes of the object 'object' defined in the schema 'protobuf'.
    'visited' is a hash table used to cache object sizes.
    The return value is the size of the object in bytes."))

;; 'visited' is used to cache object sizes
;; The default method uses meta-data from the protobuf "schema"
(defmethod object-size ((object standard-object) protobuf &key visited)
  (check-type protobuf (or protobuf protobuf-message))
  (let ((size (and visited (gethash object visited))))
    (when size
      (return-from object-size size)))
  (let* ((class   (class-of object))
         (message (find-message-for-class protobuf class))
         (size    0))
    (declare (type fixnum size))
    (assert message ()
            "There is no Protobuf message for the class ~S" class)
    (macrolet ((read-slot (object slot reader)
                 ;; Don't do a boundp check, we assume the object is fully populated
                 ;; Unpopulated slots should be "nullable" and should contain nil
                 `(if ,reader
                    (funcall ,reader ,object)
                    (slot-value ,object ,slot))))
      (labels ((do-field (object trace field)
                 ;; We don't do cycle detection here
                 ;; If the client needs it, he can define his own 'object-size'
                 ;; method to clean things up first
                 (let* ((cl     (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                        (slot   (proto-value field))
                        (reader (proto-reader field))
                        msg)
                   (when (or slot reader)
                     (cond ((eq (proto-required field) :repeated)
                            (cond ((and (proto-packed field) (packed-type-p cl))
                                   (let ((tag (make-tag cl (proto-index field))))
                                     (iincf size (packed-size (read-slot object slot reader) cl tag))))
                                  ((keywordp cl)
                                   (let ((tag (make-tag cl (proto-index field))))
                                     (map () #'(lambda (v)
                                                 (iincf size (prim-size v cl tag)))
                                             (read-slot object slot reader))))
                                  ((typep (setq msg (and cl (loop for p in trace
                                                                  thereis (or (find-message-for-class p cl)
                                                                              (find-enum-for-type p cl)))))
                                          'protobuf-message)
                                   (dolist (v (if slot (read-slot object slot reader) (list object)))
                                     (let ((tag (make-tag $wire-type-string (proto-index field)))
                                           (len (object-size v protobuf :visited visited)))
                                       (iincf size (length32 tag))
                                       (iincf size (length32 len)))
                                     (map () (curry #'do-field v (cons msg trace))
                                             (proto-fields msg))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                     (map () #'(lambda (v)
                                                 (iincf size (enum-size v (proto-values msg) tag)))
                                             (read-slot object slot reader))))))
                           (t
                            (cond ((keywordp cl)
                                   (let ((v (read-slot object slot reader)))
                                     (when (or v (eq cl :bool))
                                       (let ((tag (make-tag cl (proto-index field))))
                                         (iincf size (prim-size v cl tag))))))
                                  ((typep (setq msg (and cl (loop for p in trace
                                                                  thereis (or (find-message-for-class p cl)
                                                                              (find-enum-for-type p cl)))))
                                          'protobuf-message)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (when v
                                       (let ((tag (make-tag $wire-type-string (proto-index field)))
                                             (len (object-size v protobuf :visited visited)))
                                         (iincf size (length32 tag))
                                         (iincf size (length32 len)))
                                       (map () (curry #'do-field v (cons msg trace))
                                               (proto-fields msg)))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((v (read-slot object slot reader)))
                                     (when v
                                       (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                         (iincf size (enum-size (read-slot object slot reader) (proto-values msg) tag)))))))))))))
        (declare (dynamic-extent #'do-field))
        (map () (curry #'do-field object (list message protobuf)) (proto-fields message))
        (when visited
          (setf (gethash object visited) size))   ;cache the size
        size))))


;;; Compile-time generation of serializers

(defun generate-serializer (protobuf message)
  "Generate a 'serialize-object' method for the given message."
  (with-gensyms (vobj vproto vbuf vidx vval)
    (with-collectors ((serializers collect-serializer))
      (dolist (field (proto-fields message))
        (let* ((class  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
               (msg    (and class (not (keywordp class))
                            (or (or (find-message-for-class message class)
                                    (find-enum-for-type message class))
                                (or (find-message-for-class protobuf class)
                                    (find-enum-for-type protobuf class)))))
               (reader (cond ((proto-reader field)
                              `(funcall #',(proto-reader field) ,vobj))
                             ((proto-value field)
                              `(slot-value ,vobj ',(proto-value field)))))
               (index  (proto-index field)))
          (when reader
            (cond ((eq (proto-required field) :repeated)
                   (cond ((and (proto-packed field) (packed-type-p class))
                          (collect-serializer
                           (let ((tag (make-tag class index)))
                             `(setq ,vidx (serialize-packed ,reader ,class ,tag ,vbuf ,vidx)))))
                         ((keywordp class)
                          (collect-serializer
                           (let ((tag (make-tag class index)))
                             `(dolist (,vval ,reader)
                                (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx))))))
                         ((typep msg 'protobuf-message)
                          (collect-serializer
                           (let ((tag (make-tag $wire-type-string index)))
                             `(dolist (,vval ,reader)
                                (let ((len (object-size ,vval ,vproto :visited visited)))
                                  (setq ,vidx (encode-uint32 ,tag ,vbuf ,vidx))
                                  (setq ,vidx (encode-uint32 len ,vbuf ,vidx))
                                  (serialize-object ,vval ,vproto ,vbuf ,vidx :visited visited)
                                  (incf ,vidx len))))))
                         ((typep msg 'protobuf-enum)
                          (collect-serializer
                           (let ((tag (make-tag $wire-type-varint index)))
                             `(dolist (,vval ,reader)
                                (setq ,vidx (serialize-enum ,vval '(,@(proto-values msg)) ,tag ,vbuf ,vidx))))))))
                  (t
                   (cond ((keywordp class)
                          (collect-serializer
                           (let ((tag (make-tag class index)))
                             (if (eq class :bool)
                               `(let ((,vval ,reader))
                                  (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx)))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx))))))))
                         ((typep msg 'protobuf-message)
                          (collect-serializer
                           (let ((tag (make-tag $wire-type-string index)))
                             `(let ((,vval ,reader))
                                (when ,vval
                                  (let ((len (object-size ,vval ,vproto :visited visited)))
                                    (setq ,vidx (encode-uint32 ,tag ,vbuf ,vidx))
                                    (setq ,vidx (encode-uint32 len ,vbuf ,vidx))
                                    (serialize-object ,vval ,vproto ,vbuf ,vidx :visited visited)
                                    (incf ,vidx len)))))))
                         ((typep msg 'protobuf-enum)
                          (collect-serializer
                           (let ((tag (make-tag $wire-type-varint index)))
                             `(let ((,vval ,reader))
                                (when ,vval
                                  (setq ,vidx (serialize-enum ,vval '(,@(proto-values msg)) ,tag ,vbuf ,vidx)))))))))))))
      `(defmethod serialize-object ((,vobj ,(proto-class message)) ,vproto ,vbuf ,vidx &key visited) 
         (declare (ignorable visited)
                  (type (simple-array (unsigned-byte 8)) ,vbuf)
                  (type fixnum ,vidx))
        ,@serializers
         (values ,vbuf ,vidx)))))

(defun generate-deserializer (protobuf message)
  "Generate a 'deserialize-object' method for the given message."
  (with-gensyms (vclass vproto vbuf vidx vlen vobj vval)
    (with-collectors ((deserializers collect-deserializer))
      (dolist (field (proto-fields message))
        (let* ((class  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
               (msg    (and class (not (keywordp class))
                            (or (or (find-message-for-class message class)
                                    (find-enum-for-type message class))
                                (or (find-message-for-class protobuf class)
                                    (find-enum-for-type protobuf class)))))
               (slot   (proto-value field))
               (index  (proto-index field)))
          (cond ((eq (proto-required field) :repeated)
                 (cond ((and (proto-packed field) (packed-type-p class))
                        (collect-deserializer
                         `((,(make-tag class index))
                           (multiple-value-bind (,vval idx)
                               (deserialize-packed ,class ,vbuf ,vidx)
                             (setq ,vidx idx)
                             ,(when slot
                                `(setf (slot-value ,vobj ',slot) ,vval))))))
                       ((keywordp class)
                        (collect-deserializer
                         `((,(make-tag class index))
                           (multiple-value-bind (,vval idx)
                               (deserialize-prim ,class ,vbuf ,vidx)
                             (setq ,vidx idx)
                             ,(when slot
                                `(setf (slot-value ,vobj ',slot)
                                       (nconc (slot-value ,vobj ',slot) (list ,vval))))))))
                       ((typep msg 'protobuf-message)
                        (collect-deserializer
                         `((,(make-tag $wire-type-string index))
                           (multiple-value-bind (len idx)
                               (decode-uint32 ,vbuf ,vidx)
                             (setq ,vidx idx)
                             (multiple-value-bind (,vval idx)
                                 (deserialize-object ',class ,vproto ,vbuf ,vidx (i+ ,vidx len))
                               (setq ,vidx idx)
                               ,(when slot
                                  `(setf (slot-value ,vobj ',slot)
                                         (nconc (slot-value ,vobj ',slot) (list ,vval)))))))))
                       ((typep msg 'protobuf-enum)
                        (collect-deserializer
                         `((,(make-tag $wire-type-varint index))
                           (multiple-value-bind (,vval idx)
                               (deserialize-enum '(,@(proto-values msg)) ,vbuf ,vidx)
                             (setq ,vidx idx)
                             ,(when slot
                                `(setf (slot-value ,vobj ',slot)
                                       (nconc (slot-value ,vobj ',slot) (list ,vval))))))))))
                (t
                 (cond ((keywordp class)
                        (collect-deserializer
                         `((,(make-tag class index))
                           (multiple-value-bind (,vval idx)
                               (deserialize-prim ,class ,vbuf ,vidx)
                             (setq ,vidx idx)
                             ,(when slot
                                `(setf (slot-value ,vobj ',slot) ,vval))))))
                       ((typep msg 'protobuf-message)
                        (collect-deserializer
                         `((,(make-tag $wire-type-string index))
                           (multiple-value-bind (len idx)
                               (decode-uint32 ,vbuf ,vidx)
                             (setq ,vidx idx)
                             (multiple-value-bind (,vval idx)
                                 (deserialize-object ',class ,vproto ,vbuf ,vidx (i+ ,vidx len))
                               (setq ,vidx idx)
                               ,(when slot
                                  `(setf (slot-value ,vobj ',slot) ,vval)))))))
                       ((typep msg 'protobuf-enum)
                        (collect-deserializer
                         `((,(make-tag $wire-type-varint index))
                           (multiple-value-bind (,vval idx)
                               (deserialize-enum '(,@(proto-values msg)) ,vbuf ,vidx)
                             (setq ,vidx idx)
                             ,(when slot
                                `(setf (slot-value ,vobj ',slot) ,vval)))))))))))
    `(defmethod deserialize-object ((,vclass (eql ',(proto-class message))) ,vproto ,vbuf
                                    &optional (,vidx 0) ,vlen)
       (let ((,vlen (or ,vlen (length ,vbuf))))
         (declare (type fixnum ,vlen))
         (let ((,vobj (make-instance ',(or (proto-class-override message) (proto-class message)))))
           (loop
              (when (>= ,vidx ,vlen)
                (return-from deserialize-object (values ,vobj ,vidx)))
              (multiple-value-bind (tag idx)
                  (decode-uint32 ,vbuf ,vidx)
                (setq ,vidx idx)
                (case tag
                  ,@deserializers
                  (otherwise
                   (setq ,vidx (skip-element ,vbuf ,vidx (ilogand tag #x7)))))))))))))

(defun generate-object-size (protobuf message)
  "Generate an 'object-size' method for the given message."
  (with-gensyms (vobj vproto vsize vval)
    (with-collectors ((sizers collect-sizer))
      (dolist (field (proto-fields message))
        (let* ((class  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
               (msg    (and class (not (keywordp class))
                            (or (or (find-message-for-class message class)
                                    (find-enum-for-type message class))
                                (or (find-message-for-class protobuf class)
                                    (find-enum-for-type protobuf class)))))
               (reader (cond ((proto-reader field)
                              `(funcall #',(proto-reader field) ,vobj))
                             ((proto-value field)
                              `(slot-value ,vobj ',(proto-value field)))))
               (index  (proto-index field)))
          (when reader
            (cond ((eq (proto-required field) :repeated)
                   (cond ((and (proto-packed field) (packed-type-p class))
                          (collect-sizer
                           (let ((tag (make-tag class index)))
                             `(iincf ,vsize (packed-size ,reader ,class ,tag)))))
                         ((keywordp class)
                          (collect-sizer
                           (let ((tag (make-tag class index)))
                             `(dolist (,vval ,reader)
                                (iincf ,vsize (prim-size ,vval ,class ,tag))))))
                         ((typep msg 'protobuf-message)
                          (collect-sizer
                           (let ((tag (make-tag $wire-type-string index)))
                             `(dolist (,vval ,reader)
                                (let ((len (object-size ,vval ,vproto :visited visited)))
                                  (iincf ,vsize (length32 ,tag))
                                  (iincf ,vsize (length32 len))
                                  (iincf ,vsize len))))))
                         ((typep msg 'protobuf-enum)
                          (let ((tag (make-tag $wire-type-varint index)))
                            (collect-sizer
                             `(dolist (,vval ,reader)
                                (iincf ,vsize (enum-size ,vval '(,@(proto-values msg)) ,tag))))))))
                  (t
                   (cond ((keywordp class)
                          (let ((tag (make-tag class index)))
                            (collect-sizer
                             (if (eq class :bool)
                               `(let ((,vval ,reader))
                                  (iincf ,vsize (prim-size ,vval ,class ,tag)))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (iincf ,vsize (prim-size ,vval ,class ,tag))))))))
                         ((typep msg 'protobuf-message)
                          (collect-sizer
                           (let ((tag (make-tag $wire-type-string index)))
                             `(let ((,vval ,reader))
                                (when ,vval
                                  (let ((len (object-size ,vval ,vproto :visited visited)))
                                    (iincf ,vsize (length32 ,tag))
                                    (iincf ,vsize (length32 len))
                                    (iincf ,vsize len)))))))
                         ((typep msg 'protobuf-enum)
                          (let ((tag (make-tag $wire-type-varint index)))
                            (collect-sizer
                             `(let ((,vval ,reader))
                                (when ,vval
                                  (iincf ,vsize (enum-size ,vval '(,@(proto-values msg)) ,tag)))))))))))))
      `(defmethod object-size ((,vobj ,(proto-class message)) ,vproto &key visited)
         (declare (ignorable visited))
         (let ((,vsize (and visited (gethash ,vobj visited))))
           (when ,vsize
             (return-from object-size ,vsize)))
         (let ((,vsize 0))
           (declare (type fixnum ,vsize))
           ,@sizers
           (when visited
             (setf (gethash ,vobj visited) ,vsize))
           ,vsize)))))
