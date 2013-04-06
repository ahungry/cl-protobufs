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


;;; Other API functions

(defgeneric object-initialized-p (object type)
  (:documentation
   "Returns true iff all of the fields of 'object' are initialized."))  

(defmethod object-initialized-p (object (type symbol))
  (let ((message (find-message-for-class type)))
    (unless message
      (serialization-error "There is no Protobuf message having the type ~S" type))
    (object-initialized-p object message)))

(defmethod object-initialized-p (object (message protobuf-message))
  (macrolet ((read-slot (object slot reader)
               `(if ,reader
                  (funcall ,reader ,object)
                  (slot-value ,object ,slot))))
    (labels ((initialized-p (object trace field)
               (let* ((type   (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                      (slot   (proto-value field))
                      (reader (proto-reader field))
                      msg)
                 (when slot
                   (cond ((eq (proto-required field) :repeated)
                          ;; We're claiming that empty repeated fields are initialized,
                          ;; which might not be correct
                          (cond ((keywordp type) t)
                                ((typep (setq msg (and type (or (find-message trace type)
                                                                (find-enum trace type))))
                                        'protobuf-message)
                                 (let ((values (read-slot object slot reader)))
                                   (dolist (value values t)
                                     (unless (every #'(lambda (field)
                                                        (initialized-p value msg field))
                                                    (proto-fields msg))
                                       (return-from object-initialized-p nil)))))
                                ((typep msg 'protobuf-enum) t)))
                         (t
                          (cond ((keywordp type)
                                 (or (slot-initialized-p object message slot)
                                     (return-from object-initialized-p nil)))
                                ((typep (setq msg (and type (or (find-message trace type)
                                                                (find-enum trace type))))
                                        'protobuf-message)
                                 (unless (slot-initialized-p object message slot)
                                   (return-from object-initialized-p nil))
                                 (let ((value (read-slot object slot reader)))
                                   (unless (every #'(lambda (field)
                                                      (initialized-p value msg field))
                                                  (proto-fields msg))
                                     (return-from object-initialized-p nil))))
                                ((typep msg 'protobuf-enum)
                                 (or (slot-initialized-p object message slot)
                                     (return-from object-initialized-p nil))))))))))
            (declare (dynamic-extent #'initialized-p))
            (every #'(lambda (field)
                       (initialized-p object message field))
                   (proto-fields message)))))


(defgeneric slot-initialized-p (object type slot)
  (:documentation
   "Returns true iff the field 'slot' in 'object' is initialized."))

(defmethod slot-initialized-p (object (type symbol) slot)
  (let ((message (find-message-for-class type)))
    (unless message
      (serialization-error "There is no Protobuf message having the type ~S" type))
    (slot-initialized-p object message slot)))

(defmethod slot-initialized-p (object (message protobuf-message) slot)
  (macrolet ((read-slot (object slot reader)
               `(if ,reader
                  (funcall ,reader ,object)
                  (slot-value ,object ,slot))))
    (let ((field (find-field message slot)))
      (when field
        (let ((type   (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
              (slot   (proto-value field))
              (reader (proto-reader field)))
          (cond ((null slot) nil)
                ((or (eq (proto-required field) :required)
                     (eq type :bool))
                 (slot-boundp object slot))
                (t (not (null (read-slot object slot reader))))))))))


(defgeneric reinitialize-object (object type)
  (:documentation
   "Reset all the fields of 'object' to their initial values."))

(defmethod reinitialize-object (object (type symbol))
  (let ((message (find-message-for-class type)))
    (unless message
      (serialization-error "There is no Protobuf message having the type ~S" type))
    (reinitialize-object object message)))

(defmethod reinitialize-object (object (message protobuf-message))
  (dolist (field (proto-fields message))
    (reinitialize-field object message field))
  object)

(defgeneric reinitialize-field (object message field)
  (:method (object (message protobuf-message) field)
    (macrolet ((write-slot (object slot writer value)
                 `(if ,writer
                    (funcall ,writer ,object ,value)
                    (setf (slot-value ,object ,slot) ,value))))
      (let ((default (proto-default field))
            (slot    (proto-value field))
            (writer  (proto-writer field)))
        (cond ((null slot)
               (unless (empty-default-p field)
                 (write-slot object slot writer default)))
              (t
               (if (empty-default-p field)
                 (slot-makunbound object slot)
                 (write-slot object slot writer default))))))))

(defgeneric reinitialize-slot (object message slot)
  (:method (object (message protobuf-message) slot)
    (let ((field (find slot (proto-fields message) :key #'proto-value)))
      (reinitialize-field object message field))))


;;; A Python-like, Protobufs2-compatible API

(defgeneric is-initialized (object)
  (:documentation
   "Returns true iff all of the fields of 'object' are initialized.")
  (:method ((object standard-object))
    (let* ((class   (type-of object))
           (message (find-message-for-class class)))
      (unless message
        (serialization-error "There is no Protobufs message for the class ~S" class))
      (object-initialized-p object message))))

(defgeneric clear (object)
  (:documentation
   "Initialize all of the fields of 'object' to their default values.")
  (:method ((object standard-object))
    (let* ((class   (type-of object))
           (message (find-message-for-class class)))
      (unless message
        (serialization-error "There is no Protobufs message for the class ~S" class))
      (reinitialize-object object message))))

(defgeneric has-field (object slot)
  (:documentation
   "Returns true iff the field 'slot' in 'object' is initialized.")
  (:method ((object standard-object) slot)
    (let* ((class   (type-of object))
           (message (find-message-for-class class)))
      (unless message
        (serialization-error "There is no Protobufs message for the class ~S" class))
      (slot-initialized-p object message slot))))

(defgeneric clear-field (object slot)
  (:documentation
   "Initialize the field 'slot' of 'object' to its default value.")
  (:method ((object standard-object) slot)
    (let* ((class   (type-of object))
           (message (find-message-for-class class)))
      (unless message
        (serialization-error "There is no Protobufs message for the class ~S" class))
      (reinitialize-slot object message slot))))

;; This is simpler than 'object-size', but doesn't fully support aliasing
(defgeneric octet-size (object)
  (:documentation
   "Returns the number of octets required to encode 'object' using the wire format.
    'object' is an object whose Lisp class corresponds to a Protobufs message.")
  (:method ((object standard-object))
    (let* ((class   (type-of object))
           (message (find-message-for-class class))
           (type    (and message (proto-class message))))
      (unless message
        (serialization-error "There is no Protobufs message for the class ~S" class))
      (let ((visited (make-size-cache object type)))
        (object-size object type visited)))))

;; This is simpler than 'serialize-object', but doesn't fully support aliasing
(defgeneric serialize (object &optional buffer start end)
  (:documentation
   "Serialize 'object' into 'buffer' using the wire format, starting at the index
   'start' and going no farther than 'end'. 'object' is an object whose Lisp class
   corresponds to a Protobufs message.
   Returns two values, the final index and the buffer.")
  (:method ((object standard-object) &optional buffer (start 0) end)
    (declare (ignore end))
    (let* ((class   (type-of object))
           (message (find-message-for-class class))
           (type    (and message (proto-class message))))
      (unless message
        (serialization-error "There is no Protobufs message for the class ~S" class))
      (let* ((visited (make-size-cache object type))
             (size    (object-size object type visited))
             (start   (or start 0))
             (buffer  (or buffer (make-byte-vector size))))
        (unless (>= (length buffer) size)
          (serialization-error "The buffer ~S is not large enough to hold ~S" buffer))
        (multiple-value-bind (nbuf nend)
            (serialize-object object type buffer start visited)
          (declare (ignore nbuf))
          (values nend buffer))))))

(defgeneric merge-from-array (object buffer &optional start end)
  (:documentation
   "Deserialize the object encoded in 'buffer' and merge it into 'object'.
    Deserialization starts at the index 'start' and ends at 'end'.
    'object' must an object whose Lisp class corresponds to the message
    being deserialized.
    The return value is the updated object.")
  (:method ((object standard-object) buffer &optional (start 0) (end (length buffer)))
    (let* ((class   (type-of object))
           (message (find-message-for-class class))
           (type    (and message (proto-class message))))
      (unless message
        (serialization-error "There is no Protobufs message for the class ~S" class))
      (let* ((start  (or start 0))
             (end    (or end (length buffer))))
        (merge-from-message object (deserialize-object type buffer start end))))))

(defgeneric merge-from-message (object source)
  (:documentation
   "Merge the fields from the source object 'source' into 'object'.
    The two objects must be of the same type.
    Singular fields will be overwritten, with embedded messages being be merged.
    Repeated fields will be concatenated.
    The return value is the updated object 'object'.")
  (:method ((object standard-object) (source standard-object))
    (let* ((class   (type-of object))
           (message (find-message-for-class class))
           (type    (and message (proto-class message))))
      (unless message
        (serialization-error "There is no Protobufs message for the class ~S" class))
      (unless (eq class (type-of source))
        (serialization-error "The objects ~S and ~S are of not of the same class" object source))
      ;;--- Do this (should return side-effected 'object', not 'source')
      type
      source)))
