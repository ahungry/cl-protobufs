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


;;; A Protobufs2-compatible API, whose names are taken from the Python API

(defgeneric clear (object)
  (:documentation
   "Initialize all of the fields of 'object' to their default values.")
  (:method ((object standard-object))
    (let* ((message (find-message-for-class (class-of object)))
           (proto   '---need-to-have-messages-store-the-parent---))
      (assert message ()
              "There is no Protobufs message for the class ~S" (class-of object))
      message proto)))

(defgeneric is-initialized (object)
  (:documentation
   "Returns true iff all of the fields of 'object' are initialized.")
  (:method ((object standard-object))
    (let* ((message (find-message-for-class (class-of object)))
           (proto   '---need-to-have-messages-store-the-parent---))
      (assert message ()
              "There is no Protobufs message for the class ~S" (class-of object))
      message proto)))

;; This is simpler than 'object-size', but doesn't fully support aliasing
(defgeneric octet-size (object)
  (:documentation
   "Returns the number of octets required to encode 'object' using the wire format.
    'object' is an object whose Lisp class corresponds to a Protobufs message.")
  (:method ((object standard-object))
    (let* ((message (find-message-for-class (class-of object)))
           (type    (and message (proto-class message)))
           (proto   '---need-to-have-messages-store-the-parent---))
      (assert message ()
              "There is no Protobufs message for the class ~S" (class-of object))
      (let ((visited (make-hash-table)))
        (object-size object type proto :visited visited)))))

;; This is simpler than 'serialize-object', but doesn't fully support aliasing
(defgeneric serialize (object buffer &optional start end)
  (:documentation
   "Serialize 'object' into 'buffer' using the wire format, starting at the index
   'start' and going no farther than 'end'. 'object' is an object whose Lisp class
   corresponds to a Protobufs message.")
  (:method ((object standard-object) buffer &optional start end)
    (declare (ignore end))
    (let* ((message (find-message-for-class (class-of object)))
           (type    (and message (proto-class message)))
           (proto   '---need-to-have-messages-store-the-parent---))
      (assert message ()
              "There is no Protobufs message for the class ~S" (class-of object))
      (let* ((visited (make-hash-table))
	     (size    (object-size object type proto :visited visited))
             (start   (or start 0))
             (buffer  (make-array size :element-type '(unsigned-byte 8))))
        (serialize-object object type proto buffer start :visited visited)
        buffer))))

;; This is simpler than 'deserialize-object', but doesn't fully support aliasing
(defgeneric merge-from-array (object buffer &optional start end)
  (:documentation
   "Deserialize the object encoded in 'buffer' into 'object', starting at the index
    'start' and ending at 'end'. 'object' is an object whose Lisp class corresponds
    to a Protobufs message.")
  (:method ((object standard-object) buffer &optional start end)
    (let* ((message (find-message-for-class (class-of object)))
           (type    (and message (proto-class message)))
           (proto   '---need-to-have-messages-store-the-parent---))
      (assert message ()
              "There is no Protobufs message for the class ~S" (class-of object))
      (let* ((start  (or start 0))
             (end    (or end (length buffer))))
        (deserialize-object type proto buffer start end)))))

(defgeneric merge-from-message (object source-object)
  (:documentation
   "")
  (:method ((object standard-object) (source-object standard-object))
    (assert (eq (class-of object) (class-of source-object)) ()
            "The objects ~S and ~S are of not of the same class" object source-object)
    (let* ((message (find-message-for-class (class-of object)))
           (type    (and message (proto-class message)))
           (proto   '---need-to-have-messages-store-the-parent---))
      (assert message ()
              "There is no Protobufs message for the class ~S" (class-of object))
      message type proto)))
