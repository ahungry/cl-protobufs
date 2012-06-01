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
         (buffer  (make-byte-vector size)))
    (serialize-object object type buffer 0 visited)
    (when stream
      (write-sequence buffer stream))
    buffer))

(defun serialize-object-to-file (filename object type &key visited)
  (with-open-file (stream filename
                   :direction :output
                   :element-type '(unsigned-byte 8))
    (serialize-object-to-stream object type :stream stream :visited visited)))

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

(defmethod serialize-object (object type buffer &optional start visited)
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
                                   ;; This is where we handle packed primitive types
                                   ;; Packed enums get handled below
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
                                   (if (eq (proto-message-type msg) :group)
                                     (map () #'(lambda (v)
                                                 ;; To serialize a group, we encode a start tag,
                                                 ;; serialize the fields, then encode an end tag
                                                 (let ((tag1 (make-tag $wire-type-start-group (proto-index field)))
                                                       (tag2 (make-tag $wire-type-end-group   (proto-index field))))
                                                   (setq index (encode-uint32 tag1 buffer index))
                                                   (map () (curry #'do-field v msg)
                                                           (proto-fields msg))
                                                   (setq index (encode-uint32 tag2 buffer index))))
                                             (if slot (read-slot object slot reader) (list object)))
                                     (map () #'(lambda (v)
                                                 ;; To serialize an embedded message, first say that it's
                                                 ;; a string, then encode its size, then serialize its fields
                                                 (let ((tag (make-tag $wire-type-string (proto-index field)))
                                                       (len (object-size v msg visited)))
                                                   (setq index (encode-uint32 tag buffer index))
                                                   (setq index (encode-uint32 len buffer index)))
                                                 (map () (curry #'do-field v msg)
                                                         (proto-fields msg)))
                                             (if slot (read-slot object slot reader) (list object)))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                     ;; 'proto-packed-p' of enum types returns nil,
                                     ;; so packed enum fields won't be handled above
                                     (if (proto-packed field)
                                       (setq index (serialize-packed-enum (read-slot object slot reader)
                                                                          (proto-values msg) tag buffer index))
                                       (map () #'(lambda (v)
                                                   (setq index (serialize-enum v (proto-values msg) tag buffer index)))
                                               (read-slot object slot reader)))))))
                           (t
                            (cond ((eq type :bool)
                                   ;; We have to handle optional boolean fields specially
                                   ;; because "false" and nil are the same value in Lisp
                                   (let ((v (cond ((or (eq (proto-required field) :required)
                                                       (null slot))
                                                   (read-slot object slot reader))
                                                  ((slot-boundp object slot)
                                                   (read-slot object slot reader))
                                                  (t :unbound))))
                                     (unless (eq v :unbound)
                                       (let ((tag (make-tag :bool (proto-index field))))
                                         (setq index (serialize-prim v type tag buffer index))))))
                                  ((keywordp type)
                                   (let ((v (read-slot object slot reader)))
                                     (when v
                                       (let ((tag (make-tag type (proto-index field))))
                                         (setq index (serialize-prim v type tag buffer index))))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type))))
                                          'protobuf-message)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (when v
                                       (if (eq (proto-message-type msg) :group)
                                         (let ((tag1 (make-tag $wire-type-start-group (proto-index field)))
                                               (tag2 (make-tag $wire-type-end-group   (proto-index field))))
                                           (setq index (encode-uint32 tag1 buffer index))
                                           (map () (curry #'do-field v msg)
                                                   (proto-fields msg))
                                           (setq index (encode-uint32 tag2 buffer index)))
                                         (let ((tag (make-tag $wire-type-string (proto-index field)))
                                               (len (object-size v msg visited)))
                                           (setq index (encode-uint32 tag buffer index))
                                           (setq index (encode-uint32 len buffer index))
                                           (map () (curry #'do-field v msg)
                                                   (proto-fields msg)))))))
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
         (buffer  (make-byte-vector size)))
    (read-sequence buffer stream)
    (deserialize-object type buffer 0 size)))

(defun deserialize-object-from-file (type filename)
  (with-open-file (stream filename
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (deserialize-object-from-stream type :stream stream)))

;; Allow clients to add their own methods
;; This is you might preserve object identity, e.g.
(defgeneric deserialize-object (type buffer &optional start end end-tag)
  (:documentation
   "Deserializes an object of the given type 'type' as a Protobufs object.
    'type' is the Lisp name of a Protobufs message (usually the name of a 
    Lisp class) or a 'protobuf-message'.
    The encoded bytes are in the byte array given by 'buffer' starting at
    the fixnum index 'start' up to the end of the buffer, given by 'end'.
    'start' defaults to 0, 'end' defaults to the length of the buffer.
    'end-tag' is used internally to handle the (deprecated) \"group\" feature.
    The return values are the object and the index at which deserialization stopped.."))

(defmethod deserialize-object (type buffer &optional start end (end-tag 0))
  (let ((message (find-message-for-class type)))
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
    (deserialize-object message buffer start end end-tag)))

;; The default method uses metadata from the protobuf "schema" for the message
(defmethod deserialize-object ((message protobuf-message) buffer &optional start end (end-tag 0))
  (declare (type (simple-array (unsigned-byte 8)) buffer))
  (let ((index   (or start 0))
        (length  (or end (length buffer))))
    (declare (type fixnum index length))
    (macrolet ((read-slot (object slot reader)
                 `(if ,reader
                    (funcall ,reader ,object)
                    (slot-value ,object ,slot)))
               (write-slot (object slot writer value)
                 (with-gensyms (vval)
                   `(let ((,vval ,value))
                      (if ,writer
                        (funcall ,writer ,object ,vval)
                        (setf (slot-value ,object ,slot) ,vval))))))
      (labels ((deserialize (type trace end end-tag)
                 (declare (type fixnum end end-tag))
                 (let* ((message (find-message trace type))
                        (object  (and message
                                      (make-instance (or (proto-alias-for message) (proto-class message)))))
                        ;; All the slots into which we store a repeated element
                        ;; These will be reversed at the end of deserialization
                        (rslots ()))
                   (loop
                     (multiple-value-bind (tag idx)
                         (if (i< index end) (decode-uint32 buffer index) (values 0 index))
                       ;; We're done if we've gotten to the end index or
                       ;; we see an end tag that matches a previous group's start tag
                       ;; Note that the default end tag is 0, which is also an end of
                       ;; message marker (there can never be "real" zero tags because
                       ;; field indices start at 1)
                       (setq index idx)
                       (when (i= tag end-tag)
                         ;; Reverse the repeated slots
                         (dolist (field rslots)
                           (let ((slot   (proto-value field))
                                 (reader (proto-reader field))
                                 (writer (proto-writer field)))
                             (write-slot object slot writer
                                         (nreverse (read-slot object slot reader)))))
                         (return-from deserialize
                           (values object index)))
                       (let* ((fidx  (ilogand (iash tag -3) #x1FFFFFFF))
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
                           (setq index (skip-element buffer index tag))
                           ;;--- Check for mismatched wire type, running past end of buffer, etc
                           (cond ((and field (eq (proto-required field) :repeated))
                                  (let ((vectorp (vector-field-p field)))
                                    (cond ((and (proto-packed field) (packed-type-p type))
                                           (multiple-value-bind (values idx)
                                               (deserialize-packed type buffer index)
                                             (setq index idx)
                                             (if vectorp
                                               (let ((values (make-array (length values)
                                                               :fill-pointer t :adjustable t
                                                               :initial-contents values)))
                                                 (write-slot object slot writer values))
                                               (write-slot object slot writer values))))
                                          ((keywordp type)
                                           (multiple-value-bind (val idx)
                                               (deserialize-prim type buffer index)
                                             (setq index idx)
                                             (cond (vectorp
                                                    (vector-push-extend val (read-slot object slot reader)))
                                                   (t
                                                    (pushnew field rslots)
                                                    ;; This "push" could type-check the entire list if
                                                    ;; there's a parameterized list type in effect,
                                                    ;; so you'll want to avoid using such types
                                                    ;; We'll reverse the slots at the last minute
                                                    (write-slot object slot writer
                                                                (cons val (read-slot object slot reader)))))))
                                          ((typep (setq msg (and type (or (find-message trace type)
                                                                          (find-enum trace type))))
                                                  'protobuf-message)
                                           (if (eq (proto-message-type msg) :group)
                                             (let* ((etag (make-tag $wire-type-end-group fidx))
                                                    (obj  (deserialize type msg length etag)))
                                               (cond (vectorp
                                                      (vector-push-extend obj (read-slot object slot reader)))
                                                     (t
                                                      (pushnew field rslots)
                                                      (write-slot object slot writer
                                                                  (cons obj (read-slot object slot reader))))))
                                             (multiple-value-bind (len idx)
                                                 (decode-uint32 buffer index)
                                               (setq index idx)
                                               (let ((obj (deserialize type msg (+ index len) 0)))
                                                 (cond (vectorp
                                                        (vector-push-extend obj (read-slot object slot reader)))
                                                       (t
                                                        (pushnew field rslots)
                                                        (write-slot object slot writer
                                                                    (cons obj (read-slot object slot reader)))))))))
                                          ((typep msg 'protobuf-enum)
                                           (if (proto-packed field)
                                             (multiple-value-bind (values idx)
                                                 (deserialize-packed-enum (proto-values msg) buffer index)
                                               (setq index idx)
                                               (if vectorp
                                                 (let ((values (make-array (length values)
                                                                 :fill-pointer t :adjustable t
                                                                 :initial-contents values)))
                                                   (write-slot object slot writer values))
                                                 (write-slot object slot writer values)))
                                             (multiple-value-bind (val idx)
                                                 (deserialize-enum (proto-values msg) buffer index)
                                               (setq index idx)
                                               (cond (vectorp
                                                      (vector-push-extend val (read-slot object slot reader)))
                                                     (t
                                                      (pushnew field rslots)
                                                      (write-slot object slot writer
                                                                  (cons val (read-slot object slot reader)))))))))))
                                 (t
                                  (cond ((keywordp type)
                                         (multiple-value-bind (val idx)
                                             (deserialize-prim type buffer index)
                                           (setq index idx)
                                           (write-slot object slot writer val)))
                                        ((typep (setq msg (and type (or (find-message trace type)
                                                                        (find-enum trace type))))
                                                'protobuf-message)
                                         ;;--- If there's already a value in the slot, merge messages
                                         (if (eq (proto-message-type msg) :group)
                                           (let* ((etag (make-tag $wire-type-end-group fidx))
                                                  (obj  (deserialize type msg length etag)))
                                             (write-slot object slot writer obj))
                                           (multiple-value-bind (len idx)
                                               (decode-uint32 buffer index)
                                             (setq index idx)
                                             (let ((obj (deserialize type msg (+ index len) 0)))
                                               (write-slot object slot writer obj)))))
                                        ((typep msg 'protobuf-enum)
                                         (multiple-value-bind (val idx)
                                             (deserialize-enum (proto-values msg) buffer index)
                                           (setq index idx)
                                           (write-slot object slot writer val)))))))))))))
        (declare (dynamic-extent #'deserialize))
        (deserialize (proto-class message) message length end-tag)))))

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

(defmethod object-size (object type &optional visited)
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
                                   (if (eq (proto-message-type msg) :group)
                                     (map () #'(lambda (v)
                                                 (let ((tag1 (make-tag $wire-type-start-group (proto-index field)))
                                                       (tag2 (make-tag $wire-type-end-group   (proto-index field))))
                                                   (iincf size (length32 tag1))
                                                   (map () (curry #'do-field v msg)
                                                           (proto-fields msg))
                                                   (iincf size (length32 tag2))))
                                             (if slot (read-slot object slot reader) (list object)))
                                     (map () #'(lambda (v)
                                                 (let ((tag (make-tag $wire-type-string (proto-index field)))
                                                       (len (object-size v msg visited)))
                                                   (iincf size (length32 tag))
                                                   (iincf size (length32 len))
                                                   (map () (curry #'do-field v msg)
                                                           (proto-fields msg))))
                                             (if slot (read-slot object slot reader) (list object)))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                     (if (proto-packed field)
                                       (iincf size (packed-enum-size (read-slot object slot reader) type tag))
                                       (map () #'(lambda (v)
                                                   (iincf size (enum-size v (proto-values msg) tag)))
                                               (read-slot object slot reader)))))))
                           (t
                            (cond ((eq type :bool)
                                   (let ((v (cond ((or (eq (proto-required field) :required)
                                                       (null slot))
                                                   (read-slot object slot reader))
                                                  ((slot-boundp object slot)
                                                   (read-slot object slot reader))
                                                  (t :unbound))))
                                     (unless (eq v :unbound)
                                       (let ((tag (make-tag :bool (proto-index field))))
                                         (iincf size (prim-size v type tag))))))
                                  ((keywordp type)
                                   (let ((v (read-slot object slot reader)))
                                     (when v
                                       (let ((tag (make-tag type (proto-index field))))
                                         (iincf size (prim-size v type tag))))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type))))
                                          'protobuf-message)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (when v
                                       (if (eq (proto-message-type msg) :group)
                                         (let ((tag1 (make-tag $wire-type-start-group (proto-index field)))
                                             (tag2 (make-tag $wire-type-end-group   (proto-index field))))
                                           (iincf size (length32 tag1))
                                           (map () (curry #'do-field v msg)
                                                   (proto-fields msg))
                                           (iincf size (length32 tag2)))
                                         (let ((tag (make-tag $wire-type-string (proto-index field)))
                                               (len (object-size v msg visited)))
                                           (iincf size (length32 tag))
                                           (iincf size (length32 len))
                                           (map () (curry #'do-field v msg)
                                                (proto-fields msg)))))))
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
;;; Type-checking is done at the top-level methods specialized on 'symbol',
;;; so we turn off all type checking at the level of these functions

;; Note well: keep this in sync with the main 'serialize-object' method above
(defun generate-serializer (message)
  "Generate a 'serialize-object' method for the given message."
  (with-gensyms (vobj vbuf vidx vval vclass)
    (when (null (proto-fields message))
      (return-from generate-serializer
        `(defmethod serialize-object
           (,vobj (,vclass (eql ,message)) ,vbuf &optional (,vidx 0) visited)
         (declare #.$optimize-serialization)
         (declare (ignorable ,vobj ,vclass visited)
                  (type (simple-array (unsigned-byte 8)) ,vbuf)
                  (type fixnum ,vidx))
         (values ,vbuf ,vidx))))
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
                   (let ((iterator (if (vector-field-p field) 'dovector 'dolist)))
                     (cond ((and (proto-packed field) (packed-type-p class))
                            (collect-serializer
                             (let ((tag (make-tag class index)))
                               `(setq ,vidx (serialize-packed ,reader ,class ,tag ,vbuf ,vidx)))))
                           ((keywordp class)
                            (collect-serializer
                             (let ((tag (make-tag class index)))
                               `(,iterator (,vval ,reader)
                                  (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx))))))
                           ((typep msg 'protobuf-message)
                            (collect-serializer
                             (if (eq (proto-message-type msg) :group)
                               (let ((tag1 (make-tag $wire-type-start-group index))
                                     (tag2 (make-tag $wire-type-end-group   index)))
                                 `(,iterator (,vval ,reader)
                                    (let ((len (or (and visited (gethash ,vval visited))
                                                   (object-size ,vval ,msg visited))))
                                      (setq ,vidx (encode-uint32 ,tag1 ,vbuf ,vidx))
                                      (serialize-object ,vval ,msg ,vbuf ,vidx visited)
                                      (iincf ,vidx len)
                                      (setq ,vidx (encode-uint32 ,tag2 ,vbuf ,vidx)))))
                               (let ((tag (make-tag $wire-type-string index)))
                                 `(,iterator (,vval ,reader)
                                    (let ((len (or (and visited (gethash ,vval visited))
                                                   (object-size ,vval ,msg visited))))
                                      (setq ,vidx (encode-uint32 ,tag ,vbuf ,vidx))
                                      (setq ,vidx (encode-uint32 len ,vbuf ,vidx))
                                      (serialize-object ,vval ,msg ,vbuf ,vidx visited)
                                      (iincf ,vidx len)))))))
                           ((typep msg 'protobuf-enum)
                            (collect-serializer
                             (let ((tag (make-tag $wire-type-varint index)))
                               (if (proto-packed field)
                                 `(setq ,vidx (serialize-packed-enum ,reader '(,@(proto-values msg)) ,tag ,vbuf ,vidx))

                                 `(,iterator (,vval ,reader)
                                    (setq ,vidx (serialize-enum ,vval '(,@(proto-values msg)) ,tag ,vbuf ,vidx))))))))))
                  (t
                   (cond ((keywordp class)
                          (collect-serializer
                           (let ((tag (make-tag class index)))
                             (if (eq class :bool)
                               (if (or (eq (proto-required field) :required)
                                       reader)
                                 `(let ((,vval ,reader))
                                    (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx)))
                                 `(let ((,vval (cond ((slot-boundp ,vobj ',(proto-value field))
                                                      ,reader)
                                                     (t :unbound))))
                                    (unless (eq ,vval :unbound)
                                      (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx)))))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx))))))))
                         ((typep msg 'protobuf-message)
                          (collect-serializer
                           (if (eq (proto-message-type msg) :group)
                             (let ((tag1 (make-tag $wire-type-start-group index))
                                   (tag2 (make-tag $wire-type-end-group   index)))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (let ((len (or (and visited (gethash ,vval visited))
                                                   (object-size ,vval ,msg visited))))
                                      (setq ,vidx (encode-uint32 ,tag1 ,vbuf ,vidx))
                                      (serialize-object ,vval ,msg ,vbuf ,vidx visited)
                                      (iincf ,vidx len)
                                      (setq ,vidx (encode-uint32 ,tag2 ,vbuf ,vidx))))))
                             (let ((tag (make-tag $wire-type-string index)))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (let ((len (or (and visited (gethash ,vval visited))
                                                   (object-size ,vval ,msg visited))))
                                      (setq ,vidx (encode-uint32 ,tag ,vbuf ,vidx))
                                      (setq ,vidx (encode-uint32 len ,vbuf ,vidx))
                                      (serialize-object ,vval ,msg ,vbuf ,vidx visited)
                                      (iincf ,vidx len))))))))
                         ((typep msg 'protobuf-enum)
                          (collect-serializer
                           (let ((tag (make-tag $wire-type-varint index)))
                             `(let ((,vval ,reader))
                                (when ,vval
                                  (setq ,vidx (serialize-enum ,vval '(,@(proto-values msg)) ,tag ,vbuf ,vidx)))))))))))))
      `(defmethod serialize-object
           (,vobj (,vclass (eql ,message)) ,vbuf &optional (,vidx 0) visited)
         (declare #.$optimize-serialization)
         (declare (ignorable visited)
                  (type (simple-array (unsigned-byte 8)) ,vbuf)
                  (type fixnum ,vidx))
         ,@serializers
         (values ,vbuf ,vidx)))))

;; Note well: keep this in sync with the main 'deserialize-object' method above
(defun generate-deserializer (message)
  "Generate a 'deserialize-object' method for the given message."
  (with-gensyms (vclass vbuf vidx vlen vendtag vobj vval)
    (when (null (proto-fields message))
      (return-from generate-deserializer
        `(defmethod deserialize-object
             ((,vclass (eql ,message)) ,vbuf &optional ,vidx ,vlen (,vendtag 0))
           (declare #.$optimize-serialization)
           (declare (ignorable ,vclass ,vbuf ,vlen ,vendtag)
                    (type (simple-array (unsigned-byte 8)) ,vbuf))
           (let ((,vidx (or ,vidx 0)))
             (declare (type fixnum ,vidx))
             (let ((,vobj (make-instance ',(or (proto-alias-for message) (proto-class message)))))
               (values ,vobj ,vidx))))))
    (with-collectors ((deserializers collect-deserializer)
                      ;; For tracking repeated slots that will need to be reversed
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
                               ,@(when (vector-field-p field)
                                   `((setq ,vval (make-array (length ,vval)
                                                   :fill-pointer t :adjustable t
                                                   :initial-contents ,vval))))
                               ,(write-slot vobj field vval)))))
                         ((keywordp class)
                          (let ((temp (gensym (string (proto-value field)))))
                            (collect-rslot (list field temp))
                            (collect-deserializer
                             `((,(make-tag class index))
                               (multiple-value-bind (,vval idx)
                                   (deserialize-prim ,class ,vbuf ,vidx)
                                 (setq ,vidx idx)
                                 (push ,vval ,temp))))))
                         ((typep msg 'protobuf-message)
                          (let ((temp (gensym (string (proto-value field)))))
                            (collect-rslot (list field temp))
                            (collect-deserializer
                             (if (eq (proto-message-type msg) :group)
                               `((,(make-tag $wire-type-start-group index))
                                 (multiple-value-bind (,vval idx)
                                     (deserialize-object ,msg ,vbuf ,vidx ,vlen
                                                         ,(make-tag $wire-type-end-group index))
                                   (setq ,vidx idx)
                                   (push ,vval ,temp)))
                               `((,(make-tag $wire-type-string index))
                                 (multiple-value-bind (len idx)
                                     (decode-uint32 ,vbuf ,vidx)
                                   (setq ,vidx idx)
                                   (multiple-value-bind (,vval idx)
                                       (deserialize-object ,msg ,vbuf ,vidx (i+ ,vidx len) 0)
                                     (setq ,vidx idx)
                                     (push ,vval ,temp))))))))
                         ((typep msg 'protobuf-enum)
                          (if (proto-packed field)
                            (collect-deserializer
                             `((,(make-tag $wire-type-varint index))
                               (multiple-value-bind (,vval idx)
                                   (deserialize-packed-enum '(,@(proto-values msg)) ,vbuf ,vidx)
                                 (setq ,vidx idx)
                                 ,(write-slot vobj field vval))))
                            (let ((temp (gensym (string (proto-value field)))))
                              (collect-rslot (list field temp))
                              (collect-deserializer
                               `((,(make-tag $wire-type-varint index))
                                 (multiple-value-bind (,vval idx)
                                     (deserialize-enum '(,@(proto-values msg)) ,vbuf ,vidx)
                                   (setq ,vidx idx)
                                   (push ,vval ,temp)))))))))
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
                           (if (eq (proto-message-type msg) :group)
                             `((,(make-tag $wire-type-start-group index))
                               (multiple-value-bind (,vval idx)
                                   (deserialize-object ,msg ,vbuf ,vidx  ,vlen
                                                       ,(make-tag $wire-type-end-group index))
                                 (setq ,vidx idx)
                                 ,(write-slot vobj field vval)))
                             `((,(make-tag $wire-type-string index))
                               (multiple-value-bind (len idx)
                                   (decode-uint32 ,vbuf ,vidx)
                                 (setq ,vidx idx)
                                 (multiple-value-bind (,vval idx)
                                     (deserialize-object ,msg ,vbuf ,vidx (i+ ,vidx len) 0)
                                   (setq ,vidx idx)
                                   ,(write-slot vobj field vval)))))))
                         ((typep msg 'protobuf-enum)
                          (collect-deserializer
                           `((,(make-tag $wire-type-varint index))
                             (multiple-value-bind (,vval idx)
                                 (deserialize-enum '(,@(proto-values msg)) ,vbuf ,vidx)
                               (setq ,vidx idx)
                               ,(write-slot vobj field vval)))))))))))
      (let* ((rslots  (delete-duplicates rslots :key #'first))
             (rfields (mapcar #'first  rslots))
             (rtemps  (mapcar #'second rslots)))
        `(defmethod deserialize-object
             ((,vclass (eql ,message)) ,vbuf &optional ,vidx ,vlen (,vendtag 0))
           (declare #.$optimize-serialization)
           (declare (type (simple-array (unsigned-byte 8)) ,vbuf))
           (let ((,vidx (or ,vidx 0))
                 (,vlen (or ,vlen (length ,vbuf))))
             (declare (type fixnum ,vidx ,vlen))
             (let ((,vobj (make-instance ',(or (proto-alias-for message) (proto-class message))))
                   ;; Bind the temporary variables that hold repeated slots
                   ,@rtemps)
               (loop
                 (multiple-value-bind (tag idx)
                     (if (i< ,vidx ,vlen) (decode-uint32 ,vbuf ,vidx) (values 0 ,vidx))
                   (setq ,vidx idx)
                   (when (i= tag ,vendtag)
                     ;; Set the (un)reversed values of the repeated slots
                     ,@(loop for field in rfields
                             for temp in rtemps
                             as slot = (proto-value field)
                             as writer = (proto-writer field)
                             collect (cond ((vector-field-p field)
                                            (if writer
                                              `(funcall ,writer ,vobj (make-array (length ,temp)
                                                                        :fill-pointer t :adjustable t
                                                                        :initial-contents (nreverse ,temp)))
                                              `(setf (slot-value ,vobj ',slot) (make-array (length ,temp)
                                                                                 :fill-pointer t :adjustable t
                                                                                 :initial-contents (nreverse ,temp)))))
                                           (t
                                            (if writer
                                              `(funcall ,writer ,vobj (nreverse ,temp))
                                              `(setf (slot-value ,vobj ',slot) (nreverse ,temp))))))
                     (return-from deserialize-object
                       (values ,vobj ,vidx)))
                   (case tag
                     ,@deserializers
                     (otherwise
                      (setq ,vidx (skip-element ,vbuf ,vidx tag)))))))))))))

;; Note well: keep this in sync with the main 'object-size' method above
(defun generate-object-size (message)
  "Generate an 'object-size' method for the given message."
  (with-gensyms (vobj vsize vval vclass)
    (when (null (proto-fields message))
      (return-from generate-object-size
        `(defmethod object-size
             (,vobj (,vclass (eql ,message)) &optional visited)
         (declare #.$optimize-serialization)
         (declare (ignorable ,vobj visited))
         0)))
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
                   (let ((iterator (if (vector-field-p field) 'dovector 'dolist)))
                     (cond ((and (proto-packed field) (packed-type-p class))
                            (collect-sizer
                             (let ((tag (make-tag class index)))
                               `(iincf ,vsize (packed-size ,reader ,class ,tag)))))
                           ((keywordp class)
                            (collect-sizer
                             (let ((tag (make-tag class index)))
                               `(,iterator (,vval ,reader)
                                  (iincf ,vsize (prim-size ,vval ,class ,tag))))))
                           ((typep msg 'protobuf-message)
                            (collect-sizer
                             (if (eq (proto-message-type msg) :group)
                               (let ((tag1 (make-tag $wire-type-start-group index))
                                     (tag2 (make-tag $wire-type-end-group   index)))
                                 `(,iterator (,vval ,reader)
                                    (let ((len (or (and visited (gethash ,vval visited))
                                                   (object-size ,vval ,msg visited))))
                                      (iincf ,vsize (length32 ,tag1))
                                      (iincf ,vsize len)
                                      (iincf ,vsize ,tag2))))
                               (let ((tag (make-tag $wire-type-string index)))
                                 `(,iterator (,vval ,reader)
                                    (let ((len (or (and visited (gethash ,vval visited))
                                                   (object-size ,vval ,msg visited))))
                                      (iincf ,vsize (length32 ,tag))
                                      (iincf ,vsize (length32 len))
                                      (iincf ,vsize len)))))))
                           ((typep msg 'protobuf-enum)
                            (let ((tag (make-tag $wire-type-varint index)))
                              (collect-sizer
                               (if (proto-packed field)
                                 `(iincf ,vsize (packed-enum-size ,reader '(,@(proto-values msg)) ,tag))
                                 `(,iterator (,vval ,reader)
                                    (iincf ,vsize (enum-size ,vval '(,@(proto-values msg)) ,tag))))))))))
                  (t
                   (cond ((keywordp class)
                          (let ((tag (make-tag class index)))
                            (collect-sizer
                             (if (eq class :bool)
                               (if (or (eq (proto-required field) :required)
                                       reader)
                                 `(let ((,vval ,reader))
                                    (declare (ignorable ,vval))
                                    (iincf ,vsize (prim-size ,vval ,class ,tag)))
                                 `(let ((,vval (cond ((slot-boundp ,vobj ',(proto-value field))
                                                      ,reader)
                                                     (t :unbound))))
                                    (unless (eq ,vval :unbound)
                                      (iincf ,vsize (prim-size ,vval ,class ,tag)))))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (iincf ,vsize (prim-size ,vval ,class ,tag))))))))
                         ((typep msg 'protobuf-message)
                          (collect-sizer
                           (if (eq (proto-message-type msg) :group)
                             (let ((tag1 (make-tag $wire-type-start-group index))
                                   (tag2 (make-tag $wire-type-end-group   index)))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (let ((len (or (and visited (gethash ,vval visited))
                                                   (object-size ,vval ,msg visited))))
                                      (iincf ,vsize (length32 ,tag1))
                                      (iincf ,vsize len)
                                      (iincf ,vsize (length32 ,tag2))))))
                             (let ((tag (make-tag $wire-type-string index)))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (let ((len (or (and visited (gethash ,vval visited))
                                                   (object-size ,vval ,msg visited))))
                                      (iincf ,vsize (length32 ,tag))
                                      (iincf ,vsize (length32 len))
                                      (iincf ,vsize len))))))))
                         ((typep msg 'protobuf-enum)
                          (let ((tag (make-tag $wire-type-varint index)))
                            (collect-sizer
                             `(let ((,vval ,reader))
                                (when ,vval
                                  (iincf ,vsize (enum-size ,vval '(,@(proto-values msg)) ,tag)))))))))))))
      `(defmethod object-size
           (,vobj (,vclass (eql ,message)) &optional visited)
         (declare #.$optimize-serialization)
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
