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
    (labels ((safe-slot-value (object slot reader)
               (if reader                               ;use a reader if it's supplied
                 (funcall reader object)
                 (if (slot-boundp object slot)
                   (slot-value object slot)
                   nil)))
             (do-field (object trace field)
               ;; We don't do cycle detection here
               ;; If the client needs it, he can define his own 'serialize-object'
               ;; method to clean things up first
               (let* ((cl  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                      (msg (and cl (loop for p in trace
                                         thereis (or (find-message-for-class p cl)
                                                     (find-enum-for-type p cl)))))
                      (slot   (proto-value field))
                      (reader (proto-reader field)))
                 (cond ((eq (proto-required field) :repeated)
                        (cond ((and slot (proto-packed field) (packed-type-p cl))
                               (setq index (serialize-packed (safe-slot-value object slot reader)
                                                             cl field buffer index)))
                              ((and slot (keywordp cl))
                               (map () #'(lambda (v)
                                           (when (or (eq cl :bool) (not (null v)))
                                             (setq index (serialize-prim v cl field buffer index))))
                                       (safe-slot-value object slot reader)))
                              ((and slot (typep msg 'protobuf-enum))
                               (map () #'(lambda (v)
                                           (when (not (null v))
                                             (setq index (serialize-enum v msg field buffer index))))
                                       (safe-slot-value object slot reader)))
                              ((typep msg 'protobuf-message)
                               (dolist (v (if slot
                                            (safe-slot-value object slot reader)
                                            (list object)))
                                 ;; To serialize an embedded message, first say that it's
                                 ;; a string, then encode its size, then serialize its fields
                                 (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                       (len (object-size v protobuf :visited visited)))
                                   (setq index (encode-uint32 tag buffer index))
                                   (setq index (encode-uint32 len buffer index)))
                                 (map () (curry #'do-field v (cons msg trace))
                                         (proto-fields msg))))))
                       (t
                        (cond ((and slot (keywordp cl))
                               (let ((v (safe-slot-value object slot reader)))
                                 (when (or (eq cl :bool) (not (null v)))
                                   (setq index (serialize-prim v cl field buffer index)))))
                              ((and slot (typep msg 'protobuf-enum))
                               (let ((v (safe-slot-value object slot reader)))
                                 (when (not (null v))
                                   (setq index (serialize-enum v msg field buffer index)))))
                              ((typep msg 'protobuf-message)
                               (let ((v (if slot (safe-slot-value object slot reader) object)))
                                 (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                       (len (object-size v protobuf :visited visited)))
                                   (setq index (encode-uint32 tag buffer index))
                                   (setq index (encode-uint32 len buffer index)))
                                 (when (not (null v))
                                   (map () (curry #'do-field v (cons msg trace))
                                           (proto-fields msg)))))))))))
      (declare (dynamic-extent #'safe-slot-value #'do-field))
      (map () (curry #'do-field object (list message protobuf)) (proto-fields message)))))


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
                            (msg   (and cl (or (find-message-for-class protobuf cl)
                                               (find-enum-for-type protobuf cl))))
                            ;; It's OK for this to be null
                            ;; That means we're parsing some version of a message
                            ;; that has the field, but our current message does not
                            ;; We still have to deserialize everything, though
                            (slot  (proto-value field)))
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
                                    ((typep msg 'protobuf-enum)
                                     (multiple-value-bind (val idx)
                                         (deserialize-enum msg field buffer index)
                                       (setq index idx)
                                       (when slot
                                         (setf (slot-value object slot)
                                               (nconc (slot-value object slot) (list val))))))
                                    ((typep msg 'protobuf-message)
                                     (multiple-value-bind (len idx)
                                         (decode-uint32 buffer index)
                                       (setq index idx)
                                       (let ((obj (deserialize cl (cons msg trace) (+ index len))))
                                         (when slot
                                           (setf (slot-value object slot)
                                                 (nconc (slot-value object slot) (list obj)))))))))
                             (t
                              (cond ((keywordp cl)
                                     (multiple-value-bind (val idx)
                                         (deserialize-prim cl field buffer index)
                                       (setq index idx)
                                       (when slot
                                         (setf (slot-value object slot) val))))
                                    ((typep msg 'protobuf-enum)
                                     (multiple-value-bind (val idx)
                                         (deserialize-enum msg field buffer index)
                                       (setq index idx)
                                       (when slot
                                         (setf (slot-value object slot) val))))
                                    ((typep msg 'protobuf-message)
                                     (multiple-value-bind (len idx)
                                         (decode-uint32 buffer index)
                                       (setq index idx)
                                       (let ((obj (deserialize cl (cons msg trace) (+ index len))))
                                         (when slot
                                           (setf (slot-value object slot) obj))))))))))))))
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
    (labels ((safe-slot-value (object slot reader)
               (if reader                               ;use a reader if it's supplied
                 (funcall reader object)
                 (if (slot-boundp object slot)
                   (slot-value object slot)
                   nil)))
             (do-field (object trace field)
               ;; We don't do cycle detection here
               ;; If the client needs it, he can define his own 'object-size'
               ;; method to clean things up first
               (let* ((cl  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                      (msg (and cl (loop for p in trace
                                         thereis (or (find-message-for-class p cl)
                                                     (find-enum-for-type p cl)))))
                      (slot   (proto-value field))
                      (reader (proto-reader field)))
                 (cond ((eq (proto-required field) :repeated)
                        (cond ((and slot (proto-packed field) (packed-type-p cl))
                               (iincf size (packed-size (safe-slot-value object slot reader) cl field)))
                              ((and slot (keywordp cl))
                               (map () #'(lambda (v)
                                           (when (or (eq cl :bool) (not (null v)))
                                             (iincf size (prim-size v cl field))))
                                       (safe-slot-value object slot reader)))
                              ((and slot (typep msg 'protobuf-enum))
                               (map () #'(lambda (v)
                                           (when (not (null v))
                                             (iincf size (enum-size v msg field))))
                                       (safe-slot-value object slot reader)))
                              ((typep msg 'protobuf-message)
                               (dolist (v (if slot (safe-slot-value object slot reader) (list object)))
                                 (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                       (len (object-size v protobuf :visited visited)))
                                   (iincf size (length32 tag))
                                   (iincf size (length32 len)))
                                 (map () (curry #'do-field v (cons msg trace))
                                         (proto-fields msg))))))
                       (t
                        (cond ((and slot (keywordp cl))
                               (let ((v (safe-slot-value object slot reader)))
                                 (when (or (eq cl :bool) (not (null v)))
                                   (iincf size (prim-size v cl field)))))
                              ((and slot (typep msg 'protobuf-enum))
                               (let ((v (safe-slot-value object slot reader)))
                                 (when (not (null v))
                                   (iincf size (enum-size (safe-slot-value object slot reader) msg field)))))
                              ((typep msg 'protobuf-message)
                               (let ((v (if slot (safe-slot-value object slot reader) object)))
                                 (when (not (null v))
                                   (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                         (len (object-size v protobuf :visited visited)))
                                     (iincf size (length32 tag))
                                     (iincf size (length32 len)))
                                   (map () (curry #'do-field v (cons msg trace))
                                           (proto-fields msg)))))))))))
      (declare (dynamic-extent #'safe-slot-value #'do-field))
      (map () (curry #'do-field object (list message protobuf)) (proto-fields message))
      (when visited
        (setf (gethash object visited) size))   ;cache the size
      size)))
