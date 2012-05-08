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


;;; Other API functions

(defgeneric object-initialized-p (object type)
  (:documentation
   "Returns true iff all of the fields of 'object' are initialized."))  

(defmethod object-initialized-p (object (type symbol))
  (let ((message (find-message-for-class type)))
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
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
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
    (slot-initialized-p object message slot)))

(defmethod slot-initialized-p (object (message protobuf-message) slot)
  (let ((field (find-field message slot)))
    (when field
      (macrolet ((read-slot (object slot reader)
                   `(if ,reader
                      (funcall ,reader ,object)
                      (slot-value ,object ,slot))))
        (let ((type   (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
              (slot   (proto-value field))
              (reader (proto-reader field)))
          (cond ((null slot) nil)
                ((or (eq (proto-required field) :required)
                     (eq type :bool))
                 (slot-boundp object slot))
                (t (not (null (read-slot object slot reader))))))))))


;;; A Python-like, Protobufs2-compatible API

(defgeneric is-initialized (object)
  (:documentation
   "Returns true iff all of the fields of 'object' are initialized.")
  (:method ((object standard-object))
    (let* ((class   (class-of object))
           (message (find-message-for-class class)))
      (assert message ()
              "There is no Protobufs message for the class ~S" class)
      (object-initialized-p object message))))

(defgeneric has-field (object slot)
  (:documentation
   "Returns true iff the field 'slot' in 'object' is initialized.")
  (:method ((object standard-object) slot)
    (let* ((class   (class-of object))
           (message (find-message-for-class class)))
      (assert message ()
              "There is no Protobufs message for the class ~S" class)
      (slot-initialized-p object message slot))))

(defgeneric clear (object)
  (:documentation
   "Initialize all of the fields of 'object' to their default values.")
  (:method ((object standard-object))
    (let* ((class   (class-of object))
           (message (find-message-for-class class)))
      (assert message ()
              "There is no Protobufs message for the class ~S" class)
      ;;--- Do this: set everything either to the default value or "unbound"
      message)))

;; This is simpler than 'object-size', but doesn't fully support aliasing
(defgeneric octet-size (object)
  (:documentation
   "Returns the number of octets required to encode 'object' using the wire format.
    'object' is an object whose Lisp class corresponds to a Protobufs message.")
  (:method ((object standard-object))
    (let* ((class   (class-of object))
           (message (find-message-for-class class))
           (type    (and message (proto-class message))))
      (assert message ()
              "There is no Protobufs message for the class ~S" class)
      (let ((visited (make-hash-table)))
        (object-size object type visited)))))

;; This is simpler than 'serialize-object', but doesn't fully support aliasing
(defgeneric serialize (object &optional buffer start end)
  (:documentation
   "Serialize 'object' into 'buffer' using the wire format, starting at the index
   'start' and going no farther than 'end'. 'object' is an object whose Lisp class
   corresponds to a Protobufs message.")
  (:method ((object standard-object) &optional buffer (start 0) end)
    (declare (ignore end))
    (let* ((class   (class-of object))
           (message (find-message-for-class class))
           (type    (and message (proto-class message))))
      (assert message ()
              "There is no Protobufs message for the class ~S" class)
      (let* ((visited (make-hash-table))
             (size    (object-size object type visited))
             (start   (or start 0))
             (buffer  (or buffer (make-array size :element-type '(unsigned-byte 8)))))
        (assert (>= (length buffer) size) ()
                "The buffer ~S is not large enough to hold ~S" buffer object)
        (serialize-object object type buffer start visited)
        buffer))))

;; This is simpler than 'deserialize-object', but doesn't fully support aliasing
(defgeneric merge-from-array (object buffer &optional start end)
  (:documentation
   "Deserialize the object encoded in 'buffer' into 'object', starting at the index
    'start' and ending at 'end'. 'object' is an object whose Lisp class corresponds
    to a Protobufs message.")
  (:method ((object standard-object) buffer &optional (start 0) (end (length buffer)))
    (let* ((class   (class-of object))
           (message (find-message-for-class class))
           (type    (and message (proto-class message))))
      (assert message ()
              "There is no Protobufs message for the class ~S" class)
      (let* ((start  (or start 0))
             (end    (or end (length buffer))))
        (deserialize-object type buffer start end)))))

(defgeneric merge-from-message (object source-object)
  (:documentation
   "")
  (:method ((object standard-object) (source-object standard-object))
    (let* ((class   (class-of object))
           (message (find-message-for-class class))
           (type    (and message (proto-class message))))
      (assert (eq class (class-of source-object)) ()
              "The objects ~S and ~S are of not of the same class" object source-object)
      (assert message ()
              "There is no Protobufs message for the class ~S" class)
      ;;--- Do this
      type)))
