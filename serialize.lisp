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

;; Serialize the object using the given protobuf type
(defun serialize-object-to-stream (object type &key (stream *standard-output*) visited)
  "Serializes the object 'object' of type 'type' onto the stream 'stream'
   using the wire format.
   'type' is the Lisp name of a Protobufs message (usually the name of a 
   Lisp class) or a 'protobuf-message'.
   'visited' is a hash table used to cache object sizes. If it is supplied, it will be
   cleared before it is used; otherwise, a fresh table will be created.
   The return value is the buffer containing the serialized object. If the stream is
   nil, the buffer is not actually written to anywhere."
  (let* ((visited (let ((v (or visited (make-hash-table))))
                    (clrhash v)
                    v))
         (size    (object-size object type visited))
         (buffer  (make-array size :element-type '(unsigned-byte 8))))
    (serialize-object object type buffer 0 visited)
    (when stream
      (write-sequence buffer stream))
    buffer))

;; Allow clients to add their own methods
;; This is how we address the problem of cycles, e.g. -- if you have an object
;; that may contain cycles, serialize the cyclic object using a "handle"
(defgeneric serialize-object (object type buffer &optional start visited)
  (:documentation
   "Serializes the object 'object' of type 'type' into the byte array 'buffer'
    using the wire format.
    'type' is the Lisp name of a Protobufs message (usually the name of a 
    Lisp class) or a 'protobuf-message'.
    The object is serialized into the byte array given by 'buffer' starting
    at the fixnum index 'index' using the wire format.
    'visited' is a hash table used to cache object sizes.
    The return value is the buffer containing the serialized object."))

(defmethod serialize-object (object (type symbol) buffer &optional start visited)
  (let ((message (find-message-for-class type)))
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
    (serialize-object object message buffer start visited)))

;; 'visited' is used to cache object sizes
;; If it's passed in explicitly, it is assumed to already have the sizes within it
;; The default method uses metadata from the protobuf "schema" for the message
(defmethod serialize-object (object (message protobuf-message) buffer &optional start visited)
  (declare (type (simple-array (unsigned-byte 8)) buffer))
  (let ((visited (or visited (make-hash-table)))
        (index   (or start 0)))
    (declare (type fixnum index))
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
                 (let* ((type   (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                        (slot   (proto-value field))
                        (reader (proto-reader field))
                        msg)
                   (when (or slot reader)
                     (cond ((eq (proto-required field) :repeated)
                            (cond ((and (proto-packed field) (packed-type-p type))
                                   (let ((tag (make-tag type (proto-index field))))
                                     (setq index (serialize-packed (read-slot object slot reader)
                                                                   type tag buffer index))))
                                  ((keywordp type)
                                   (let ((tag (make-tag type (proto-index field))))
                                     (map () #'(lambda (v)
                                                 (setq index (serialize-prim v type tag buffer index)))
                                             (read-slot object slot reader))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type))))
                                          'protobuf-message)
                                   (dolist (v (if slot (read-slot object slot reader) (list object)))
                                     ;; To serialize an embedded message, first say that it's
                                     ;; a string, then encode its size, then serialize its fields
                                     (let ((tag (make-tag $wire-type-string (proto-index field)))
                                           (len (object-size v msg visited)))
                                       (setq index (encode-uint32 tag buffer index))
                                       (setq index (encode-uint32 len buffer index)))
                                     (map () (curry #'do-field v msg)
                                             (proto-fields msg))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                     (map () #'(lambda (v)
                                                 (setq index (serialize-enum v (proto-values msg) tag buffer index)))
                                             (read-slot object slot reader))))))
                           (t
                            (cond ((keywordp type)
                                   (let ((v (read-slot object slot reader)))
                                     (when (or v (eq type :bool))
                                       (let ((tag (make-tag type (proto-index field))))
                                         (setq index (serialize-prim v type tag buffer index))))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type))))
                                          'protobuf-message)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (when v
                                       (let ((tag (make-tag $wire-type-string (proto-index field)))
                                             (len (object-size v msg visited)))
                                         (setq index (encode-uint32 tag buffer index))
                                         (setq index (encode-uint32 len buffer index))
                                         (map () (curry #'do-field v msg)
                                                 (proto-fields msg))))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((v (read-slot object slot reader)))
                                     (when v
                                       (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                         (setq index (serialize-enum v (proto-values msg) tag buffer index)))))))))))))
        (declare (dynamic-extent #'do-field))
        (map () (curry #'do-field object message) (proto-fields message))))
    (values buffer index)))


;;; Deserialization

(defun deserialize-object-from-stream (type &key (stream *standard-input*))
  "Deserializes an object of the given type 'type' as a Protobuf object.
   'type' is the Lisp name of a Protobufs message (usually the name of a 
   Lisp class) or a 'protobuf-message'.
   The return value is the object."
  (let* ((size    (file-length stream))
         (buffer  (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    (deserialize-object type buffer 0 size)))

;; Allow clients to add their own methods
;; This is you might preserve object identity, e.g.
(defgeneric deserialize-object (type buffer &optional start end)
  (:documentation
   "Deserializes an object of the given type 'type' as a Protobufs object.
    'type' is the Lisp name of a Protobufs message (usually the name of a 
    Lisp class) or a 'protobuf-message'.
    The encoded bytes are in the byte array given by 'buffer' starting at
    the fixnum index 'index' up to the length of the buffer, given by 'length'.
    The return values are the object and the index at which deserialization stopped.."))

(defmethod deserialize-object ((type symbol) buffer &optional start end)
  (let ((message (find-message-for-class type)))
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
    (deserialize-object message buffer start end)))

;; The default method uses metadata from the protobuf "schema" for the message
(defmethod deserialize-object ((message protobuf-message) buffer &optional start end)
  (declare (type (simple-array (unsigned-byte 8)) buffer))
  (let ((index   (or start 0))
        (length  (or end (length buffer))))
    (declare (type fixnum index length))
    (macrolet ((read-slot (object slot reader)
                 `(if ,reader
                    (funcall ,reader ,object)
                    (slot-value ,object ,slot)))
               (write-slot (object slot writer value)
                 `(if ,writer
                    (funcall ,writer ,object ,value)
                    (setf (slot-value ,object ,slot) ,value))))
      (labels ((deserialize (type trace end)
                 (declare (type fixnum end))
                 (let* ((message (find-message trace type))
                        (object  (and message
                                      (make-instance (or (proto-alias-for message) (proto-class message)))))
                        ;; All the slots into which we store a repeated element
                        (rslots ()))
                   (loop
                     (multiple-value-bind (tag idx)
                         (if (i< index end) (decode-uint32 buffer index) (values 0 index))
                       ;; We're done if we've gotten to the end index or
                       ;; we see a null byte (there can never be null tags
                       ;; because field indices start at 1)
                       (when (i= tag 0)
                         ;; Now set the repeated slots
                         ;; If we do this element by element, we get killed by type checking
                         ;; in the slot setters
                         (dolist (field rslots)
                           (let ((slot   (proto-value field))
                                 (reader (proto-reader field))
                                 (writer (proto-writer field)))
                             (write-slot object slot writer
                                         (nreverse (read-slot object slot reader)))))
                         (return-from deserialize
                           (values object index)))
                       (setq index idx)
                       (let* ((wtype (ilogand tag #x7))
                              (fidx  (ilogand (iash tag -3) #x1FFFFFFF))
                              (field (find fidx (proto-fields message) :key #'proto-index))
                              (type  (and field (if (eq (proto-class field) 'boolean) :bool (proto-class field))))
                              ;; It's OK for this to be null
                              ;; That means we're parsing some version of a message
                              ;; that has the field, but our current message does not
                              ;; We still have to deserialize everything, though
                              (slot   (and field (proto-value field)))
                              (reader (and field (proto-reader field)))
                              (writer (and field (proto-writer field)))
                              msg)
                         (if (null field)
                           ;; If there's no field descriptor for this index, just skip
                           ;; the next element in the buffer having the given wire type
                           (setq index (skip-element buffer index wtype))
                           ;;--- Check for mismatched wire type, running past end of buffer, etc
                           (cond ((and field (eq (proto-required field) :repeated))
                                  (cond ((and (proto-packed field) (packed-type-p type))
                                         (multiple-value-bind (values idx)
                                             (deserialize-packed type buffer index)
                                           (setq index idx)
                                           (when slot
                                             (write-slot object slot writer values))))
                                        ((keywordp type)
                                         (multiple-value-bind (val idx)
                                             (deserialize-prim type buffer index)
                                           (setq index idx)
                                           (when slot
                                             (pushnew field rslots)
                                             ;; This "push" will type-check the entire list for
                                             ;; 'quux:list-of', so avoid using that type in classes
                                             ;; in Protobufs if performance is an issue
                                             (write-slot object slot writer
                                                         (cons val (read-slot object slot reader))))))
                                        ((typep (setq msg (and type (or (find-message trace type)
                                                                        (find-enum trace type))))
                                                'protobuf-message)
                                         (multiple-value-bind (len idx)
                                             (decode-uint32 buffer index)
                                           (setq index idx)
                                           (let ((obj (deserialize type msg (+ index len))))
                                             (when slot
                                               (pushnew field rslots)
                                               (write-slot object slot writer
                                                           (cons obj (read-slot object slot reader)))))))
                                        ((typep msg 'protobuf-enum)
                                         (multiple-value-bind (val idx)
                                             (deserialize-enum (proto-values msg) buffer index)
                                           (setq index idx)
                                           (when slot
                                             (pushnew field rslots)
                                             (write-slot object slot writer
                                                         (cons val (read-slot object slot reader))))))))
                                 (t
                                  (cond ((keywordp type)
                                         (multiple-value-bind (val idx)
                                             (deserialize-prim type buffer index)
                                           (setq index idx)
                                           (when slot
                                             (write-slot object slot writer val))))
                                        ((typep (setq msg (and type (or (find-message trace type)
                                                                        (find-enum trace type))))
                                                'protobuf-message)
                                         (multiple-value-bind (len idx)
                                             (decode-uint32 buffer index)
                                           (setq index idx)
                                           (let ((obj (deserialize type msg (+ index len))))
                                             (when slot
                                               (write-slot object slot writer obj)))))
                                        ((typep msg 'protobuf-enum)
                                         (multiple-value-bind (val idx)
                                             (deserialize-enum (proto-values msg) buffer index)
                                           (setq index idx)
                                           (when slot
                                             (write-slot object slot writer val))))))))))))))
        (declare (dynamic-extent #'deserialize))
        (deserialize (proto-class message) message length)))))

;;; Object sizes

;; Allow clients to add their own methods
;; This is how we address the problem of cycles, e.g. -- if you have an object
;; that may contain cycles, return the size of the "handle" to the object
(defgeneric object-size (object type &optional visited)
  (:documentation
   "Computes the size in bytes of the object 'object' of type 'type'.
    'type' is the Lisp name of a Protobufs message (usually the name of a 
    Lisp class) or a 'protobuf-message'.
    'visited' is a hash table used to cache object sizes.
    The return value is the size of the object in bytes."))

(defmethod object-size (object (type symbol) &optional visited)
  (let ((message (find-message-for-class type)))
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
    (object-size object message visited)))

;; 'visited' is used to cache object sizes
;; The default method uses metadata from the protobuf "schema" for the message
(defmethod object-size (object (message protobuf-message) &optional visited)
  (let ((size (and visited (gethash object visited))))
    (when size
      (return-from object-size size)))
  (let ((size 0))
    (declare (type fixnum size))
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
                 (let* ((type   (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                        (slot   (proto-value field))
                        (reader (proto-reader field))
                        msg)
                   (when (or slot reader)
                     (cond ((eq (proto-required field) :repeated)
                            (cond ((and (proto-packed field) (packed-type-p type))
                                   (let ((tag (make-tag type (proto-index field))))
                                     (iincf size (packed-size (read-slot object slot reader) type tag))))
                                  ((keywordp type)
                                   (let ((tag (make-tag type (proto-index field))))
                                     (map () #'(lambda (v)
                                                 (iincf size (prim-size v type tag)))
                                             (read-slot object slot reader))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type))))
                                          'protobuf-message)
                                   (dolist (v (if slot (read-slot object slot reader) (list object)))
                                     (let ((tag (make-tag $wire-type-string (proto-index field)))
                                           (len (object-size v msg visited)))
                                       (iincf size (length32 tag))
                                       (iincf size (length32 len)))
                                     (map () (curry #'do-field v msg)
                                             (proto-fields msg))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                     (map () #'(lambda (v)
                                                 (iincf size (enum-size v (proto-values msg) tag)))
                                             (read-slot object slot reader))))))
                           (t
                            (cond ((keywordp type)
                                   (let ((v (read-slot object slot reader)))
                                     (when (or v (eq type :bool))
                                       (let ((tag (make-tag type (proto-index field))))
                                         (iincf size (prim-size v type tag))))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type))))
                                          'protobuf-message)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (when v
                                       (let ((tag (make-tag $wire-type-string (proto-index field)))
                                             (len (object-size v msg visited)))
                                         (iincf size (length32 tag))
                                         (iincf size (length32 len)))
                                       (map () (curry #'do-field v msg)
                                               (proto-fields msg)))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((v (read-slot object slot reader)))
                                     (when v
                                       (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                         (iincf size (enum-size (read-slot object slot reader) (proto-values msg) tag)))))))))))))
        (declare (dynamic-extent #'do-field))
        (map () (curry #'do-field object message) (proto-fields message))
        (when visited
          (setf (gethash object visited) size))   ;cache the size
        size))))


;;; Compile-time generation of serializers

;; Note well: keep this in sync with the main 'serialize-object' method above
(defun generate-serializer (message)
  "Generate a 'serialize-object' method for the given message."
  (with-gensyms (vobj vbuf vidx vval vclass)
    (with-collectors ((serializers collect-serializer))
      (dolist (field (proto-fields message))
        (let* ((class  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
               (msg    (and class (not (keywordp class))
                            (or (find-message message class)
                                (find-enum message class))))
               (reader (cond ((proto-reader field)
                              `(,(proto-reader field) ,vobj))
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
                                (let ((len (or (and visited (gethash ,vval visited))
                                               (object-size ,vval ,msg visited))))
                                  (setq ,vidx (encode-uint32 ,tag ,vbuf ,vidx))
                                  (setq ,vidx (encode-uint32 len ,vbuf ,vidx))
                                  (serialize-object ,vval ,msg ,vbuf ,vidx visited)
                                  (iincf ,vidx len))))))
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
                                  (let ((len (or (and visited (gethash ,vval visited))
                                                 (object-size ,vval ,msg visited))))
                                    (setq ,vidx (encode-uint32 ,tag ,vbuf ,vidx))
                                    (setq ,vidx (encode-uint32 len ,vbuf ,vidx))
                                    (serialize-object ,vval ,msg ,vbuf ,vidx visited)
                                    (iincf ,vidx len)))))))
                         ((typep msg 'protobuf-enum)
                          (collect-serializer
                           (let ((tag (make-tag $wire-type-varint index)))
                             `(let ((,vval ,reader))
                                (when ,vval
                                  (setq ,vidx (serialize-enum ,vval '(,@(proto-values msg)) ,tag ,vbuf ,vidx)))))))))))))
      `(defmethod serialize-object
           (,vobj (,vclass (eql ,message)) ,vbuf &optional (,vidx 0) visited)
         (declare (ignorable visited)
                  (type (simple-array (unsigned-byte 8)) ,vbuf)
                  (type fixnum ,vidx))
         (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
          ,@serializers
           (values ,vbuf ,vidx))))))

;; Note well: keep this in sync with the main 'deserialize-object' method above
(defun generate-deserializer (message)
  "Generate a 'deserialize-object' method for the given message."
  (with-gensyms (vclass vbuf vidx vlen vobj vval)
    (with-collectors ((deserializers collect-deserializer)
                      (rslots collect-rslot))
      (flet ((read-slot (object field)
               (cond ((proto-reader field)
                      `(,(proto-reader field) ,object))
                     ((proto-value field)
                      `(slot-value ,object ',(proto-value field)))))
             (write-slot (object field value)
               (cond ((proto-writer field)
                      `(,(proto-writer field) ,object ,value))
                     ((proto-value field)
                      `(setf (slot-value ,object ',(proto-value field)) ,value)))))
        (dolist (field (proto-fields message))
          (let* ((class  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                 (msg    (and class (not (keywordp class))
                              (or (find-message message class)
                                  (find-enum message class))))
                 (index  (proto-index field)))
            (cond ((eq (proto-required field) :repeated)
                   (cond ((and (proto-packed field) (packed-type-p class))
                          (collect-deserializer
                           `((,(make-tag class index))
                             (multiple-value-bind (,vval idx)
                                 (deserialize-packed ,class ,vbuf ,vidx)
                               (setq ,vidx idx)
                               ,(write-slot vobj field vval)))))
                         ((keywordp class)
                          (collect-rslot field)
                          (collect-deserializer
                           `((,(make-tag class index))
                             (multiple-value-bind (,vval idx)
                                 (deserialize-prim ,class ,vbuf ,vidx)
                               (setq ,vidx idx)
                               (let ((val ,(read-slot vobj field)))
                                 ,(write-slot vobj field `(cons ,vval val)))))))
                         ((typep msg 'protobuf-message)
                          (collect-rslot field)
                          (collect-deserializer
                           `((,(make-tag $wire-type-string index))
                             ;; Call 'deserialize-object' with the name of the message
                             ;; class so that we preferentially get any optimized version
                             ;; of the method
                             (multiple-value-bind (len idx)
                                 (decode-uint32 ,vbuf ,vidx)
                               (setq ,vidx idx)
                               (multiple-value-bind (,vval idx)
                                   (deserialize-object ',class ,vbuf ,vidx (i+ ,vidx len))
                                 (setq ,vidx idx)
                                 (let ((val ,(read-slot vobj field)))
                                   ,(write-slot vobj field `(cons ,vval val))))))))
                         ((typep msg 'protobuf-enum)
                          (collect-rslot field)
                          (collect-deserializer
                           `((,(make-tag $wire-type-varint index))
                             (multiple-value-bind (,vval idx)
                                 (deserialize-enum '(,@(proto-values msg)) ,vbuf ,vidx)
                               (setq ,vidx idx)
                               (let ((val ,(read-slot vobj field)))
                                 ,(write-slot vobj field `(cons ,vval val)))))))))
                  (t
                   (cond ((keywordp class)
                          (collect-deserializer
                           `((,(make-tag class index))
                             (multiple-value-bind (,vval idx)
                                 (deserialize-prim ,class ,vbuf ,vidx)
                               (setq ,vidx idx)
                               ,(write-slot vobj field vval)))))
                         ((typep msg 'protobuf-message)
                          (collect-deserializer
                           `((,(make-tag $wire-type-string index))
                             (multiple-value-bind (len idx)
                                 (decode-uint32 ,vbuf ,vidx)
                               (setq ,vidx idx)
                               (multiple-value-bind (,vval idx)
                                   (deserialize-object ',class ,vbuf ,vidx (i+ ,vidx len))
                                 (setq ,vidx idx)
                                 ,(write-slot vobj field vval))))))
                         ((typep msg 'protobuf-enum)
                          (collect-deserializer
                           `((,(make-tag $wire-type-varint index))
                             (multiple-value-bind (,vval idx)
                                 (deserialize-enum '(,@(proto-values msg)) ,vbuf ,vidx)
                               (setq ,vidx idx)
                               ,(write-slot vobj field vval)))))))))))
    `(defmethod deserialize-object
         ((,vclass (eql ,message)) ,vbuf &optional ,vidx ,vlen)
       (declare (type (simple-array (unsigned-byte 8)) ,vbuf))
       (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let ((,vidx (or ,vidx 0))
               (,vlen (or ,vlen (length ,vbuf))))
           (declare (type fixnum ,vidx ,vlen))
           (let ((,vobj (make-instance ',(or (proto-alias-for message) (proto-class message)))))
             (loop
               (multiple-value-bind (tag idx)
                   (if (i< ,vidx ,vlen) (decode-uint32 ,vbuf ,vidx) (values 0 ,vidx))
                 (when (i= tag 0)
                   (dolist (field ',(delete-duplicates rslots))
                     (let* ((slot   (proto-value field))
                            (reader (proto-reader field))
                            (writer (proto-writer field))
                            (value  (nreverse (if reader
                                                (funcall reader ,vobj)
                                                (slot-value ,vobj slot)))))
                       (if writer
                         (funcall writer ,vobj value)
                         (setf (slot-value ,vobj slot) value))))
                   (return-from deserialize-object
                                (values ,vobj ,vidx)))
                 (setq ,vidx idx)
                 (case tag
                   ,@deserializers
                   (otherwise
                    (setq ,vidx (skip-element ,vbuf ,vidx (ilogand tag #x7))))))))))))))

;; Note well: keep this in sync with the main 'object-size' method above
(defun generate-object-size (message)
  "Generate an 'object-size' method for the given message."
  (with-gensyms (vobj vsize vval vclass)
    (with-collectors ((sizers collect-sizer))
      (dolist (field (proto-fields message))
        (let* ((class  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
               (msg    (and class (not (keywordp class))
                            (or (find-message message class)
                                (find-enum message class))))
               (reader (cond ((proto-reader field)
                              `(,(proto-reader field) ,vobj))
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
                                ;; Call 'object-size' with the name of the message
                                ;; class so that we preferentially get any optimized version
                                ;; of the method
                                (let ((len (or (and visited (gethash ,vval visited))
                                               (object-size ,vval ,msg visited))))
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
                                  (let ((len (or (and visited (gethash ,vval visited))
                                                 (object-size ,vval ,msg visited))))
                                    (iincf ,vsize (length32 ,tag))
                                    (iincf ,vsize (length32 len))
                                    (iincf ,vsize len)))))))
                         ((typep msg 'protobuf-enum)
                          (let ((tag (make-tag $wire-type-varint index)))
                            (collect-sizer
                             `(let ((,vval ,reader))
                                (when ,vval
                                  (iincf ,vsize (enum-size ,vval '(,@(proto-values msg)) ,tag)))))))))))))
      `(defmethod object-size
           (,vobj (,vclass (eql ,message)) &optional visited)
         (declare (ignorable visited))
         (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
           (let ((,vsize (and visited (gethash ,vobj visited))))
             (when ,vsize
               (return-from object-size ,vsize)))
           (let ((,vsize 0))
             (declare (type fixnum ,vsize))
             ,@sizers
             (when visited
               (setf (gethash ,vobj visited) ,vsize))
             ,vsize))))))
