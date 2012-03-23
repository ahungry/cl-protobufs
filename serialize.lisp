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

(defconstant $wire-type-varint 0)
(defconstant $wire-type-64bit  1)
(defconstant $wire-type-string 2)
(defconstant $wire-type-32bit  5)


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
  (check-type protobuf (or protobuf protobuf-message))
  (check-type index fixnum)
  (check-type buffer (simple-array (unsigned-byte 8)))
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
                   (cond ((eq (proto-required field) :repeated)
                          (cond ((and slot (proto-packed field) (packed-type-p cl))
                                 (setq index (serialize-packed (read-slot object slot reader)
                                                               cl field buffer index)))
                                ((and slot (keywordp cl))
                                 (map () #'(lambda (v)
                                             (when (or v (eq cl :bool))
                                               (setq index (serialize-prim v cl field buffer index))))
                                         (read-slot object slot reader)))
                                ((typep (setq msg (and cl (loop for p in trace
                                                                thereis (or (find-message-for-class p cl)
                                                                            (find-enum-for-type p cl)))))
                                        'protobuf-message)
                                 (dolist (v (if slot (read-slot object slot reader) (list object)))
                                   ;; To serialize an embedded message, first say that it's
                                   ;; a string, then encode its size, then serialize its fields
                                   (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                         (len (object-size v protobuf :visited visited)))
                                     (setq index (encode-uint32 tag buffer index))
                                     (setq index (encode-uint32 len buffer index)))
                                   (map () (curry #'do-field v (cons msg trace))
                                           (proto-fields msg))))
                                ((and slot (typep msg 'protobuf-enum))
                                 (map () #'(lambda (v)
                                             (when v
                                               (setq index (serialize-enum v msg field buffer index))))
                                         (read-slot object slot reader)))))
                         (t
                          (cond ((and slot (keywordp cl))
                                 (let ((v (read-slot object slot reader)))
                                   (when (or v (eq cl :bool))
                                     (setq index (serialize-prim v cl field buffer index)))))
                                ((typep (setq msg (and cl (loop for p in trace
                                                                thereis (or (find-message-for-class p cl)
                                                                            (find-enum-for-type p cl)))))
                                        'protobuf-message)
                                 (let ((v (if slot (read-slot object slot reader) object)))
                                   (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                         (len (object-size v protobuf :visited visited)))
                                     (setq index (encode-uint32 tag buffer index))
                                     (setq index (encode-uint32 len buffer index)))
                                   (when v
                                     (map () (curry #'do-field v (cons msg trace))
                                             (proto-fields msg)))))
                                ((and slot (typep msg 'protobuf-enum))
                                 (let ((v (read-slot object slot reader)))
                                   (when v
                                     (setq index (serialize-enum v msg field buffer index)))))))))))
        (declare (dynamic-extent #'do-field))
        (map () (curry #'do-field object (list message protobuf)) (proto-fields message))))))


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
(defgeneric deserialize-object (class protobuf buffer index)
  (:documentation
   "Deserializes an object of the given class 'class' as a protobuf object defined
    in the schema 'protobuf' from the byte array given by 'buffer' starting at
    the fixnum index 'index' using the wire format.
    The return value is the object."))

;; The default method uses meta-data from the protobuf "schema"
;; Note that 'class' is the Lisp name of the Protobufs message (class)
;; It is not the name of any overriding class ('proto-class-override')
(defmethod deserialize-object ((class symbol) protobuf buffer index)
  (check-type protobuf (or protobuf protobuf-message))
  (check-type index fixnum)
  (check-type buffer (simple-array (unsigned-byte 8)))
  (let ((length (length buffer)))
    (labels ((deserialize (class trace &optional (end length))
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
                     (return-from deserialize object))
                   (multiple-value-bind (val idx)
                       (decode-uint32 buffer index)
                     (setq index idx)
                     (let* ((type  (logand val #x7))
                            (fld   (logand (ash val -3) #x1FFFFFFF))
                            (field (find fld (proto-fields message) :key #'proto-index))
                            (cl    (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                            ;; It's OK for this to be null
                            ;; That means we're parsing some version of a message
                            ;; that has the field, but our current message does not
                            ;; We still have to deserialize everything, though
                            (slot  (proto-value field))
                            msg)
                       ;;--- Check for mismatched types, running past end of buffer, etc
                       (declare (ignore type))
                       (cond ((eq (proto-required field) :repeated)
                              (cond ((and (proto-packed field) (packed-type-p cl))
                                     (multiple-value-bind (values idx)
                                         (deserialize-packed cl field buffer index)
                                       (setq index idx)
                                       (when slot
                                         (setf (slot-value object slot) values))))
                                    ((keywordp cl)
                                     (multiple-value-bind (val idx)
                                         (deserialize-prim cl field buffer index)
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
                                         (deserialize-enum msg field buffer index)
                                       (setq index idx)
                                       (when slot
                                         (setf (slot-value object slot)
                                               (nconc (slot-value object slot) (list val))))))))
                             (t
                              (cond ((keywordp cl)
                                     (multiple-value-bind (val idx)
                                         (deserialize-prim cl field buffer index)
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
                                         (deserialize-enum msg field buffer index)
                                       (setq index idx)
                                       (when slot
                                         (setf (slot-value object slot) val)))))))))))))
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
                   (cond ((eq (proto-required field) :repeated)
                          (cond ((and slot (proto-packed field) (packed-type-p cl))
                                 (iincf size (packed-size (read-slot object slot reader) cl field)))
                                ((and slot (keywordp cl))
                                 (map () #'(lambda (v)
                                             (when (or v (eq cl :bool))
                                               (iincf size (prim-size v cl field))))
                                         (read-slot object slot reader)))
                                ((typep (setq msg (and cl (loop for p in trace
                                                                thereis (or (find-message-for-class p cl)
                                                                            (find-enum-for-type p cl)))))
                                        'protobuf-message)
                                 (dolist (v (if slot (read-slot object slot reader) (list object)))
                                   (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                         (len (object-size v protobuf :visited visited)))
                                     (iincf size (length32 tag))
                                     (iincf size (length32 len)))
                                   (map () (curry #'do-field v (cons msg trace))
                                           (proto-fields msg))))
                                ((and slot (typep msg 'protobuf-enum))
                                 (map () #'(lambda (v)
                                             (when v
                                               (iincf size (enum-size v msg field))))
                                         (read-slot object slot reader)))))
                         (t
                          (cond ((and slot (keywordp cl))
                                 (let ((v (read-slot object slot reader)))
                                   (when (or v (eq cl :bool))
                                     (iincf size (prim-size v cl field)))))
                                ((typep (setq msg (and cl (loop for p in trace
                                                                thereis (or (find-message-for-class p cl)
                                                                            (find-enum-for-type p cl)))))
                                        'protobuf-message)
                                 (let ((v (if slot (read-slot object slot reader) object)))
                                   (when v
                                     (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                           (len (object-size v protobuf :visited visited)))
                                       (iincf size (length32 tag))
                                       (iincf size (length32 len)))
                                     (map () (curry #'do-field v (cons msg trace))
                                             (proto-fields msg)))))
                                ((and slot (typep msg 'protobuf-enum))
                                 (let ((v (read-slot object slot reader)))
                                   (when v
                                     (iincf size (enum-size (read-slot object slot reader) msg field)))))))))))
        (declare (dynamic-extent #'do-field))
        (map () (curry #'do-field object (list message protobuf)) (proto-fields message))
        (when visited
          (setf (gethash object visited) size))   ;cache the size
        size))))
