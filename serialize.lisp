;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012-2013 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-IMPL")


;;; Protobuf serialization from Lisp objects

;;; Size caches

(defgeneric make-size-cache (object type)
  (:documentation
   "Make an object size cache for 'object'."))

;; Note that this gets called on the top-level object being serialized
;; This means that either all the objects in the tree should be subclasses
;; of 'base-protobuf-message', or none of them should be. If the root is
;; not a 'base-protobuf-message', then things will work but be slower; if
;; the root is a 'base-protobuf-message', but some children are not, then
;; serialization will fail.
(defmethod make-size-cache ((object standard-object) type)
  (declare (ignore type))
  ;; No '%cached-size' slot in standard objects, create a "visited" table
  (make-hash-table))

(defmethod make-size-cache ((object base-protobuf-message) type)
  ;; In classes defined by Protobufs, we will use the '%cached-size' slot
  ;; This clears the size cache for the tree of objects
  (clear-size-cache object type)
  nil)


(declaim (inline cached-object-size))
(defun cached-object-size (object visited)
  (declare #.$optimize-fast-unsafe)
  (if visited
    (gethash object visited)
    ;; Warning, Will Robinson! Danger!
    ;; If there's no 'visited' table, assume there's a cached size slot.
    ;; We should use methods speciaized on 'base-protobuf-message',
    ;; but we're trying to max out the speed.
    (slot-value object '%cached-size)))

(defun (setf cached-object-size) (size object visited)
  (declare #.$optimize-fast-unsafe)
  (if visited
    (setf (gethash object visited) size)
    (setf (slot-value object '%cached-size) size)))


(defgeneric clear-size-cache (object type)
  (:documentation
   "Clear the size cache for a tree of objects."))

(defmethod clear-size-cache ((object standard-object) type)
  (declare (ignore type))
  nil)

(defmethod clear-size-cache ((object base-protobuf-message) type)
  (let ((message (find-message-for-class type)))
    (unless message
      (serialization-error "There is no Protobuf message having the type ~S" type))
    (macrolet ((read-slot (object slot reader)
                 `(if ,reader
                    (funcall ,reader ,object)
                    (slot-value ,object ,slot))))
      (labels ((do-field (object trace field)
                 (let* ((type   (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                        (slot   (proto-value field))
                        (reader (proto-reader field))
                        msg)
                   (when (or slot reader)
                     (cond ((eq (proto-required field) :repeated)
                            (cond ((or (and (proto-packed field) (packed-type-p type))
                                       (keywordp type)))
                                  ((typep (setq msg (and type (find-message trace type))) 'protobuf-message)
                                   (setf (slot-value object '%cached-size) nil)
                                   (doseq (v (if slot (read-slot object slot reader) (list object)))
                                     (dolist (f (proto-fields msg))
                                       (do-field v msg f))))))
                           (t
                            (cond ((keywordp type))
                                  ((typep (setq msg (and type (find-message trace type))) 'protobuf-message)
                                   (setf (slot-value object '%cached-size) nil)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (when v
                                       (dolist (f (proto-fields msg))
                                         (do-field v msg f))))))))))))
          (declare (dynamic-extent #'do-field))
          (setf (slot-value object '%cached-size) nil)
          (dolist (field (proto-fields message))
            (do-field object message field))
          nil))))


;;; Serialization

(defun serialize-object-to-file (filename object type &key visited)
  "Serializes the object 'object' of type 'type' into the file 'filename'
   using the wire format.
   'object' and 'type' are the same as for 'serialize-object-to-bytes'."
  (with-open-file (stream filename
                   :direction :output
                   :element-type '(unsigned-byte 8))
    (serialize-object-to-stream object type :stream stream :visited visited)))

(defun serialize-object-to-stream (object type &key (stream *standard-output*) visited)
  "Serializes the object 'object' of type 'type' onto the stream 'stream'
   using the wire format.
   'object' and 'type' are the same as for 'serialize-object-to-bytes'."
  (let ((buffer (serialize-object-to-bytes object type :visited visited)))
    (write-sequence buffer stream)
    buffer))

(defun serialize-object-to-bytes (object type &key visited)
  "Serializes the object 'object' of type 'type' into a new byte vector
   using the wire format.
   'type' is the Lisp name of a Protobufs message (usually the name of a 
   Lisp class) or a 'protobuf-message'.
   'visited' is a hash table used to cache object sizes. If it is supplied, it will be
   cleared before it is used; otherwise, a fresh table will be created if necessary.
   The return value is the buffer containing the serialized object. If the stream is
   nil, the buffer is not actually written to anywhere."
  (let* ((visited (let ((v (or visited (make-size-cache object type))))
                    (when v (clrhash v))
                    v))
         ;; Use 'object-size' to forcibly recompute all the sizes
         (size    (object-size object type visited))
         (buffer  (make-byte-vector size)))
    (serialize-object object type buffer 0 visited)
    buffer))

;; Serialize the object using the given protobuf type


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
    'visited' is a hash table used to cache object sizes; if this is nil, then
    the object caches its size itself in a '%cached-size' slot.
    The return value is the buffer containing the serialized object."))

(defmethod serialize-object (object type buffer &optional start visited)
  (let ((message (find-message-for-class type)))
    (unless message
      (serialization-error "There is no Protobuf message having the type ~S" type))
    (handler-case        
       (serialize-object object message buffer start visited)
     (error (e)
      (serialization-error "Error serializing object ~S: ~A" object (princ-to-string e))))))

;; 'visited' is used to cache object sizes
;; If it's non-nil. it must to be a table with the sizes already in it
;; If it's nil, then the objects must have a '%cached-size' slot
;; The default method uses metadata from the protobuf "schema" for the message
(defmethod serialize-object (object (message protobuf-message) buffer &optional start visited)
  (declare (type (simple-array (unsigned-byte 8)) buffer))
  (let ((index (or start 0)))
    (declare (type fixnum index))
    (macrolet ((read-slot (object slot reader)
                 ;; Don't do a boundp check, we assume the object is fully populated
                 ;; Unpopulated slots should be "nullable" and will contain nil when empty
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
                                     (doseq (v (read-slot object slot reader))
                                       (setq index (serialize-prim v type tag buffer index)))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type)
                                                                  (find-type-alias trace type))))
                                          'protobuf-message)
                                   (if (eq (proto-message-type msg) :group)
                                     (doseq (v (if slot (read-slot object slot reader) (list object)))
                                       ;; To serialize a group, we encode a start tag,
                                       ;; serialize the fields, then encode an end tag
                                       (let ((tag1 (make-tag $wire-type-start-group (proto-index field)))
                                             (tag2 (make-tag $wire-type-end-group   (proto-index field))))
                                         (setq index (encode-uint32 tag1 buffer index))
                                         (dolist (f (proto-fields msg))
                                           (do-field v msg f))
                                         (setq index (encode-uint32 tag2 buffer index))))
                                     (doseq (v (if slot (read-slot object slot reader) (list object)))
                                       ;; To serialize an embedded message, first say that it's
                                       ;; a string, then encode its size, then serialize its fields
                                       (let ((tag (make-tag $wire-type-string (proto-index field)))
                                             (len (cached-object-size v visited)))
                                         (setq index (encode-uint32 tag buffer index))
                                         (setq index (encode-uint32 len buffer index)))
                                       (dolist (f (proto-fields msg))
                                         (do-field v msg f)))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                     ;; 'proto-packed-p' of enum types returns nil,
                                     ;; so packed enum fields won't be handled above
                                     (if (proto-packed field)
                                       (setq index (serialize-packed-enum (read-slot object slot reader)
                                                                          (proto-values msg) tag buffer index))
                                       (doseq (v (read-slot object slot reader))
                                         (setq index (serialize-enum v (proto-values msg) tag buffer index))))))
                                  ((typep msg 'protobuf-type-alias)
                                   (let* ((type (proto-proto-type msg))
                                          (tag  (make-tag type (proto-index field))))
                                     (doseq (v (read-slot object slot reader))
                                       (let ((v (funcall (proto-serializer msg) v)))
                                         (setq index (serialize-prim v type tag buffer index))))))
                                  (t
                                   (undefined-field-type "While serializing ~S,"
                                                         object type field))))
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
                                     (when (and v (not (equal v (proto-default field))))
                                       (let ((tag (make-tag type (proto-index field))))
                                         (setq index (serialize-prim v type tag buffer index))))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type)
                                                                  (find-type-alias trace type))))
                                          'protobuf-message)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (when v
                                       (if (eq (proto-message-type msg) :group)
                                         (let ((tag1 (make-tag $wire-type-start-group (proto-index field)))
                                               (tag2 (make-tag $wire-type-end-group   (proto-index field))))
                                           (setq index (encode-uint32 tag1 buffer index))
                                           (dolist (f (proto-fields msg))
                                             (do-field v msg f))
                                           (setq index (encode-uint32 tag2 buffer index)))
                                         (let ((tag (make-tag $wire-type-string (proto-index field)))
                                               (len (cached-object-size v visited)))
                                           (setq index (encode-uint32 tag buffer index))
                                           (setq index (encode-uint32 len buffer index))
                                           (dolist (f (proto-fields msg))
                                             (do-field v msg f)))))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((v (read-slot object slot reader)))
                                     (when (and v (not (eql v (proto-default field))))
                                       (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                         (setq index (serialize-enum v (proto-values msg) tag buffer index))))))
                                  ((typep msg 'protobuf-type-alias)
                                   (let ((v (read-slot object slot reader)))
                                     (when v
                                       (let* ((v    (funcall (proto-serializer msg) v))
                                              (type (proto-proto-type msg))
                                              (tag  (make-tag type (proto-index field))))
                                         (setq index (serialize-prim v type tag buffer index))))))
                                  (t
                                   (undefined-field-type "While serializing ~S,"
                                                         object type field)))))))))
        (declare (dynamic-extent #'do-field))
        (dolist (field (proto-fields message))
          (do-field object message field))))
    (values buffer index)))


;;; Deserialization

(defun deserialize-object-from-file (type filename)
  "Deserializes an object of the given type 'type' from the given file
   as a Protobuf object."
  (with-open-file (stream filename
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (deserialize-object-from-stream type :stream stream)))

(defun deserialize-object-from-stream (type &key (stream *standard-input*))
  "Deserializes an object of the given type 'type' from the given stream
   as a Protobuf object."
  (let* ((size    (file-length stream))
         (buffer  (make-byte-vector size)))
    (read-sequence buffer stream)
    (deserialize-object type buffer 0 size)))

(defun deserialize-object-from-bytes (type buffer)
  "Deserializes an object of the given type 'type' from the given stream
   as a Protobuf object.
   'type' is the Lisp name of a Protobufs message (usually the name of a 
   Lisp class) or a 'protobuf-message'.
   The return value is the object."
  (deserialize-object type buffer))

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
    (unless message
      (serialization-error "There is no Protobuf message having the type ~S" type))
    (handler-case        
       (deserialize-object message buffer start end end-tag)
     (error (e)
      (serialization-error "Error deserializing buffer ~S: ~A" buffer (princ-to-string e))))))

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
                        (setf (slot-value ,object ,slot) ,vval)))))
               (push-slot (object slot reader writer value)
                 (with-gensyms (vvals)
                   `(let ((,vvals (read-slot ,object ,slot ,reader)))
                      (if (i= (length ,vvals) 0)
                        ;; We need the initial value to be a stretchy vector,
                        ;; so scribble over it just to make sure
                        (let ((,vvals (make-array 1
                                        :fill-pointer t :adjustable t
                                        :initial-contents (list ,value))))
                          (write-slot ,object ,slot ,writer ,vvals))
                        (vector-push-extend ,value ,vvals))))))
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
                           ;; We don't explicitly check for mismatched wire type, running past the
                           ;; end of the buffer, etc; instead, we'll count on the high likelihood
                           ;; of some kind of an error getting signalled (e.g., array out of bounds)
                           ;; and catch it at a higher level. Yay, Lisp!
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
                                                    (push-slot object slot reader writer val))
                                                   (t
                                                    (pushnew field rslots)
                                                    ;; This "push" could type-check the entire list if
                                                    ;; there's a parameterized list type in effect,
                                                    ;; so you'll want to avoid using such types
                                                    ;; We'll reverse the slots at the last minute
                                                    (write-slot object slot writer
                                                                (cons val (read-slot object slot reader)))))))
                                          ((typep (setq msg (and type (or (find-message trace type)
                                                                          (find-enum trace type)
                                                                          (find-type-alias trace type))))
                                                  'protobuf-message)
                                           (if (eq (proto-message-type msg) :group)
                                             (let* ((etag (make-tag $wire-type-end-group fidx))
                                                    (obj  (deserialize type msg length etag)))
                                               (cond (vectorp
                                                      (push-slot object slot reader writer obj))
                                                     (t
                                                      (pushnew field rslots)
                                                      (write-slot object slot writer
                                                                  (cons obj (read-slot object slot reader))))))
                                             (multiple-value-bind (len idx)
                                                 (decode-uint32 buffer index)
                                               (setq index idx)
                                               (let ((obj (deserialize type msg (+ index len) 0)))
                                                 (cond (vectorp
                                                        (push-slot object slot reader writer obj))
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
                                                      (push-slot object slot reader writer val))
                                                     (t
                                                      (pushnew field rslots)
                                                      (write-slot object slot writer
                                                                  (cons val (read-slot object slot reader))))))))
                                          ((typep msg 'protobuf-type-alias)
                                           (let ((type (proto-proto-type msg)))
                                             (multiple-value-bind (val idx)
                                                 (deserialize-prim type buffer index)
                                               (setq index idx)
                                               (cond (vectorp
                                                      (push-slot object slot reader writer
                                                                 (funcall (proto-deserializer msg) val)))
                                                     (t
                                                      (pushnew field rslots)
                                                      (write-slot object slot writer
                                                                  (cons (funcall (proto-deserializer msg) val)
                                                                        (read-slot object slot reader)))))))))))
                                 (t
                                  (cond ((keywordp type)
                                         (multiple-value-bind (val idx)
                                             (deserialize-prim type buffer index)
                                           (setq index idx)
                                           (write-slot object slot writer val)))
                                        ((typep (setq msg (and type (or (find-message trace type)
                                                                        (find-enum trace type)
                                                                        (find-type-alias trace type))))
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
                                           (write-slot object slot writer val)))
                                        ((typep msg 'protobuf-type-alias)
                                         (let ((type (proto-proto-type msg)))
                                           (multiple-value-bind (val idx)
                                               (deserialize-prim type buffer index)
                                             (setq index idx)
                                             (write-slot object slot writer
                                                         (funcall (proto-deserializer msg) val)))))))))))))))
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
    'visited' is either a hash table used to cache object sizes,
    or is nil, in which case the objects must have a '%cached-size' slot in them.
    The return value is the size of the object in bytes."))

(defmethod object-size (object type &optional visited)
  (let ((message (find-message-for-class type)))
    (unless message
      (serialization-error "There is no Protobuf message having the type ~S" type))
    (object-size object message visited)))

;; 'visited' is used to cache object sizes
;; The default method uses metadata from the protobuf "schema" for the message
(defmethod object-size (object (message protobuf-message) &optional visited)
  (let ((size 0))
    (declare (type fixnum size))
    (macrolet ((read-slot (object slot reader)
                 ;; Don't do a boundp check, we assume the object is fully populated
                 ;; Unpopulated slots should be "nullable" and will contain nil when empty
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
                                     (doseq (v (read-slot object slot reader))
                                       (iincf size (prim-size v type tag)))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type)
                                                                  (find-type-alias trace type))))
                                          'protobuf-message)
                                   (if (eq (proto-message-type msg) :group)
                                     (doseq (v (if slot (read-slot object slot reader) (list object)))
                                       (let ((tag1 (make-tag $wire-type-start-group (proto-index field)))
                                             (tag2 (make-tag $wire-type-end-group   (proto-index field))))
                                         (iincf size (varint-length tag1))
                                         (dolist (f (proto-fields msg))
                                           (do-field v msg f))
                                         (iincf size (varint-length tag2))))
                                     (doseq (v (if slot (read-slot object slot reader) (list object)))
                                       (let ((tag (make-tag $wire-type-string (proto-index field)))
                                             (len (or (cached-object-size v visited)
                                                      (object-size v msg visited))))
                                         (iincf size (varint-length tag))
                                         (iincf size (varint-length len))
                                         (dolist (f (proto-fields msg))
                                           (do-field v msg f))))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                     (if (proto-packed field)
                                       (iincf size (packed-enum-size (read-slot object slot reader) type tag))
                                       (doseq (v (read-slot object slot reader))
                                         (iincf size (enum-size v (proto-values msg) tag))))))
                                  ((typep msg 'protobuf-type-alias)
                                   (let* ((type (proto-proto-type msg))
                                          (tag  (make-tag type (proto-index field))))
                                     (doseq (v (read-slot object slot reader))
                                       (let ((v (funcall (proto-serializer msg) v)))
                                         (iincf size (prim-size v type tag))))))
                                  (t
                                   (undefined-field-type "While computing the size of ~S,"
                                                         object type field))))
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
                                     (when (and v (not (equal v (proto-default field))))
                                       (let ((tag (make-tag type (proto-index field))))
                                         (iincf size (prim-size v type tag))))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type)
                                                                  (find-type-alias trace type))))
                                          'protobuf-message)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (when v
                                       (if (eq (proto-message-type msg) :group)
                                         (let ((tag1 (make-tag $wire-type-start-group (proto-index field)))
                                               (tag2 (make-tag $wire-type-end-group   (proto-index field))))
                                           (iincf size (varint-length tag1))
                                           (dolist (f (proto-fields msg))
                                             (do-field v msg f))
                                           (iincf size (varint-length tag2)))
                                         (let ((tag (make-tag $wire-type-string (proto-index field)))
                                               (len (or (cached-object-size v visited)
                                                        (object-size v msg visited))))
                                           (iincf size (varint-length tag))
                                           (iincf size (varint-length len))
                                           (dolist (f (proto-fields msg))
                                             (do-field v msg f)))))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((v (read-slot object slot reader)))
                                     (when (and v (not (eql v (proto-default field))))
                                       (let ((tag (make-tag $wire-type-varint (proto-index field))))
                                         (iincf size (enum-size (read-slot object slot reader) (proto-values msg) tag))))))
                                  ((typep msg 'protobuf-type-alias)
                                   (let ((v (read-slot object slot reader)))
                                     (when v
                                       (let* ((v    (funcall (proto-serializer msg) v))
                                              (type (proto-proto-type msg))
                                              (tag  (make-tag type (proto-index field))))
                                         (iincf size (prim-size v type tag))))))
                                  (t
                                   (undefined-field-type "While computing the size of ~S,"
                                                         object type field)))))))))
        (declare (dynamic-extent #'do-field))
        (dolist (field (proto-fields message))
          (do-field object message field))
        (setf (cached-object-size object visited) size)          ;cache the size
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
                                (find-enum message class)
                                (find-type-alias message class))))
               (reader (cond ((proto-reader field)
                              `(,(proto-reader field) ,vobj))
                             ((proto-value field)
                              `(slot-value ,vobj ',(proto-value field)))))
               (index  (proto-index field)))
          (when reader
            (cond ((eq (proto-required field) :repeated)
                   (let* ((vectorp  (vector-field-p field))
                          (iterator (if vectorp 'dovector 'dolist)))
                     (cond ((and (proto-packed field) (packed-type-p class))
                            (collect-serializer
                             (let ((tag (make-tag class index)))
                               `(setq ,vidx (serialize-packed ,reader ,class ,tag ,vbuf ,vidx
                                                              ,vectorp)))))
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
                                    (let ((len (cached-object-size ,vval visited)))
                                      (setq ,vidx (encode-uint32 ,tag1 ,vbuf ,vidx))
                                      (serialize-object ,vval ,msg ,vbuf ,vidx visited)
                                      (iincf ,vidx len)
                                      (setq ,vidx (encode-uint32 ,tag2 ,vbuf ,vidx)))))
                               (let ((tag (make-tag $wire-type-string index)))
                                 `(,iterator (,vval ,reader)
                                    (let ((len (cached-object-size ,vval visited)))
                                      (setq ,vidx (encode-uint32 ,tag ,vbuf ,vidx))
                                      (setq ,vidx (encode-uint32 len ,vbuf ,vidx))
                                      (serialize-object ,vval ,msg ,vbuf ,vidx visited)
                                      (iincf ,vidx len)))))))
                           ((typep msg 'protobuf-enum)
                            (collect-serializer
                             (let ((tag (make-tag $wire-type-varint index)))
                               (if (proto-packed field)
                                 `(setq ,vidx (serialize-packed-enum ,reader '(,@(proto-values msg)) ,tag ,vbuf ,vidx
                                                                     ,vectorp))

                                 `(,iterator (,vval ,reader)
                                    (setq ,vidx (serialize-enum ,vval '(,@(proto-values msg)) ,tag ,vbuf ,vidx)))))))
                           ((typep msg 'protobuf-type-alias)
                            (collect-serializer
                             (let* ((class (proto-proto-type msg))
                                    (tag   (make-tag class (proto-index field))))
                               `(,iterator (,vval ,reader)
                                  (let ((,vval (funcall #',(proto-serializer msg) ,vval)))
                                    (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx)))))))
                           (t
                            (undefined-field-type "While generating 'serialize-object' for ~S,"
                                                  message class field)))))
                  (t
                   (cond ((keywordp class)
                          (collect-serializer
                           (let ((tag (make-tag class index)))
                             (if (eq class :bool)
                               (if (or (eq (proto-required field) :required)
                                       (null (proto-value field)))
                                 `(let ((,vval ,reader))
                                    (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx)))
                                 `(let ((,vval (cond ((slot-boundp ,vobj ',(proto-value field))
                                                      ,reader)
                                                     (t :unbound))))
                                    (unless (eq ,vval :unbound)
                                      (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx)))))
                               (if (empty-default-p field)
                                 `(let ((,vval ,reader))
                                    (when ,vval
                                      (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx))))
                                 `(let ((,vval ,reader))
                                    (when (and ,vval (not (equal ,vval ',(proto-default field))))
                                      (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx)))))))))
                         ((typep msg 'protobuf-message)
                          (collect-serializer
                           (if (eq (proto-message-type msg) :group)
                             (let ((tag1 (make-tag $wire-type-start-group index))
                                   (tag2 (make-tag $wire-type-end-group   index)))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (let ((len (cached-object-size ,vval visited)))
                                      (setq ,vidx (encode-uint32 ,tag1 ,vbuf ,vidx))
                                      (serialize-object ,vval ,msg ,vbuf ,vidx visited)
                                      (iincf ,vidx len)
                                      (setq ,vidx (encode-uint32 ,tag2 ,vbuf ,vidx))))))
                             (let ((tag (make-tag $wire-type-string index)))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (let ((len (cached-object-size ,vval visited)))
                                      (setq ,vidx (encode-uint32 ,tag ,vbuf ,vidx))
                                      (setq ,vidx (encode-uint32 len ,vbuf ,vidx))
                                      (serialize-object ,vval ,msg ,vbuf ,vidx visited)
                                      (iincf ,vidx len))))))))
                         ((typep msg 'protobuf-enum)
                          (collect-serializer
                           (let ((tag (make-tag $wire-type-varint index)))
                             (if (empty-default-p field)
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (setq ,vidx (serialize-enum ,vval '(,@(proto-values msg)) ,tag ,vbuf ,vidx))))
                               `(let ((,vval ,reader))
                                  (when (and ,vval (not (eql ,vval ',(proto-default field))))
                                    (setq ,vidx (serialize-enum ,vval '(,@(proto-values msg)) ,tag ,vbuf ,vidx))))))))
                         ((typep msg 'protobuf-type-alias)
                          (collect-serializer
                           (let* ((class (proto-proto-type msg))
                                  (tag   (make-tag class (proto-index field))))
                             `(let ((,vval ,reader))
                                (when ,vval
                                  (let ((,vval (funcall #',(proto-serializer msg) ,vval)))
                                    (setq ,vidx (serialize-prim ,vval ,class ,tag ,vbuf ,vidx))))))))
                         (t
                          (undefined-field-type "While generating 'serialize-object' for ~S,"
                                                message class field))))))))
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
                                  (find-enum message class)
                                  (find-type-alias message class))))
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
                                   (push ,vval ,temp)))))))
                         ((typep msg 'protobuf-type-alias)
                          (let ((class (proto-proto-type msg))
                                (temp  (gensym (string (proto-value field)))))
                            (collect-rslot (list field temp))
                            (collect-deserializer
                             `((,(make-tag class index))
                               (multiple-value-bind (,vval idx)
                                   (deserialize-prim ,class ,vbuf ,vidx)
                                 (setq ,vidx idx)
                                 (push (funcall #',(proto-deserializer msg) ,vval) ,temp))))))
                         (t
                          (undefined-field-type "While generating 'deserialize-object' for ~S,"
                                                message class field))))
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
                               ,(write-slot vobj field vval)))))
                         ((typep msg 'protobuf-type-alias)
                          (let ((class (proto-proto-type msg)))
                            (collect-deserializer
                             `((,(make-tag class index))
                               (multiple-value-bind (,vval idx)
                                     (deserialize-prim ,class ,vbuf ,vidx)
                                   (let ((,vval (funcall #',(proto-deserializer msg) ,vval)))
                                     (setq ,vidx idx)
                                     ,(write-slot vobj field vval)))))))
                         (t
                          (undefined-field-type "While generating 'deserialize-object' for ~S,"
                                                message class field))))))))
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
           (setf (cached-object-size ,vobj visited) 0)
           0)))
    (with-collectors ((sizers collect-sizer))
      (dolist (field (proto-fields message))
        (let* ((class  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
               (msg    (and class (not (keywordp class))
                            (or (find-message message class)
                                (find-enum message class)
                                (find-type-alias message class))))
               (reader (cond ((proto-reader field)
                              `(,(proto-reader field) ,vobj))
                             ((proto-value field)
                              `(slot-value ,vobj ',(proto-value field)))))
               (index  (proto-index field)))
          (when reader
            (cond ((eq (proto-required field) :repeated)
                   (let* ((vectorp  (vector-field-p field))
                          (iterator (if vectorp 'dovector 'dolist)))
                     (cond ((and (proto-packed field) (packed-type-p class))
                            (collect-sizer
                             (let ((tag (make-tag class index)))
                               `(iincf ,vsize (packed-size ,reader ,class ,tag ,vectorp)))))
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
                                    (let ((len (or (cached-object-size ,vval visited)
                                                   (object-size ,vval ,msg visited))))
                                      (iincf ,vsize (varint-length ,tag1))
                                      (iincf ,vsize len)
                                      (iincf ,vsize ,tag2))))
                               (let ((tag (make-tag $wire-type-string index)))
                                 `(,iterator (,vval ,reader)
                                    (let ((len (or (cached-object-size ,vval visited)
                                                   (object-size ,vval ,msg visited))))
                                      (iincf ,vsize (varint-length ,tag))
                                      (iincf ,vsize (varint-length len))
                                      (iincf ,vsize len)))))))
                           ((typep msg 'protobuf-enum)
                            (let ((tag (make-tag $wire-type-varint index)))
                              (collect-sizer
                               (if (proto-packed field)
                                 `(iincf ,vsize (packed-enum-size ,reader '(,@(proto-values msg)) ,tag))
                                 `(,iterator (,vval ,reader)
                                    (iincf ,vsize (enum-size ,vval '(,@(proto-values msg)) ,tag)))))))
                           ((typep msg 'protobuf-type-alias)
                            (collect-sizer
                             (let* ((class (proto-proto-type msg))
                                    (tag   (make-tag class index)))
                               `(,iterator (,vval ,reader)
                                  (let ((,vval (funcall #',(proto-serializer msg) ,vval)))
                                    (iincf ,vsize (prim-size ,vval ,class ,tag)))))))
                           (t
                            (undefined-field-type "While generating 'object-size' for ~S,"
                                                  message class field)))))
                  (t
                   (cond ((keywordp class)
                          (let ((tag (make-tag class index)))
                            (collect-sizer
                             (if (eq class :bool)
                               (if (or (eq (proto-required field) :required)
                                       (null (proto-value field)))
                                 `(let ((,vval ,reader))
                                    (declare (ignorable ,vval))
                                    (iincf ,vsize (prim-size ,vval ,class ,tag)))
                                 `(let ((,vval (cond ((slot-boundp ,vobj ',(proto-value field))
                                                      ,reader)
                                                     (t :unbound))))
                                    (unless (eq ,vval :unbound)
                                      (iincf ,vsize (prim-size ,vval ,class ,tag)))))
                               (if (empty-default-p field)
                                 `(let ((,vval ,reader))
                                    (when ,vval
                                      (iincf ,vsize (prim-size ,vval ,class ,tag))))
                                 `(let ((,vval ,reader))
                                    (when (and ,vval (not (equal ,vval ',(proto-default field))))
                                      (iincf ,vsize (prim-size ,vval ,class ,tag)))))))))
                         ((typep msg 'protobuf-message)
                          (collect-sizer
                           (if (eq (proto-message-type msg) :group)
                             (let ((tag1 (make-tag $wire-type-start-group index))
                                   (tag2 (make-tag $wire-type-end-group   index)))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (let ((len (or (cached-object-size ,vval visited)
                                                   (object-size ,vval ,msg visited))))
                                      (iincf ,vsize (varint-length ,tag1))
                                      (iincf ,vsize len)
                                      (iincf ,vsize (varint-length ,tag2))))))
                             (let ((tag (make-tag $wire-type-string index)))
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (let ((len (or (cached-object-size ,vval visited)
                                                   (object-size ,vval ,msg visited))))
                                      (iincf ,vsize (varint-length ,tag))
                                      (iincf ,vsize (varint-length len))
                                      (iincf ,vsize len))))))))
                         ((typep msg 'protobuf-enum)
                          (let ((tag (make-tag $wire-type-varint index)))
                            (collect-sizer
                             (if (empty-default-p field)
                               `(let ((,vval ,reader))
                                  (when ,vval
                                    (iincf ,vsize (enum-size ,vval '(,@(proto-values msg)) ,tag))))
                               `(let ((,vval ,reader))
                                  (when (and ,vval (not (eql ,vval ',(proto-default field))))
                                    (iincf ,vsize (enum-size ,vval '(,@(proto-values msg)) ,tag))))))))
                         ((typep msg 'protobuf-type-alias)
                          (collect-sizer
                           (let* ((class (proto-proto-type msg))
                                  (tag   (make-tag class index)))
                             `(let ((,vval ,reader))
                                (when ,vval
                                  (iincf ,vsize (prim-size (funcall #',(proto-serializer msg) ,vval)
                                                           ,class ,tag)))))))
                         (t
                          (undefined-field-type "While generating 'object-size' for ~S,"
                                                message class field))))))))
      `(defmethod object-size
           (,vobj (,vclass (eql ,message)) &optional visited)
         (declare #.$optimize-serialization)
         (declare (ignorable visited))
         (let ((,vsize 0))
           (declare (type fixnum ,vsize))
           ,@sizers
           (setf (cached-object-size ,vobj visited) ,vsize)
           ,vsize)))))
