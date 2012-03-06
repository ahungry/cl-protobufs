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


;; Serialize the object using the given protobuf "schema"
;; 'visited' is used to cache object sizes
(defun serialize-object-to-stream (object protobuf &key (stream *standard-output*) visited)
  (let* ((visited (let ((v (or visited (make-hash-table))))
                    (clrhash v)
                    v))
         (size    (object-size object protobuf :visited visited))
         (buffer  (make-array size :element-type '(unsigned-byte 8))))
    (serialize-object object protobuf buffer :visited visited)
    (when stream
      (write-sequence buffer stream))
    buffer))

;; Allow specialized methods
(defgeneric serialize-object (object protobuf buffer &key visited))

;; 'visited' is used to cache object sizes
;; If it's passed in explicitly, it is assumed to already have the sizes within it
;; The default method uses meta-data from the protobuf "schema"
(defmethod serialize-object ((object standard-object) protobuf buffer &key visited)
  (check-type protobuf (or protobuf protobuf-message))
  (let* ((class   (class-of object))
         (message (find-message-for-class protobuf class))
         (visited (or visited (make-hash-table)))
         (index   0))
    (assert message ()
            "There is no Protobuf message for the class ~S" class)
    (labels ((safe-slot-value (object slot)
               (if (slot-boundp object slot)
                 (slot-value object slot)
                 nil))
             (do-field (object trace field)
               ;;---*** How can we detect cycles?
               (let* ((cl  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                      (msg (and cl (loop for p in trace
                                         thereis (or (find-message-for-class p cl)
                                                     (find-enum-for-type p cl)))))
                      (slot (proto-value field)))
                 (cond ((eq (proto-required field) :repeated)
                        (cond ((and slot (proto-packed field) (packed-type-p cl))
                               (setq index (serialize-packed (safe-slot-value object slot) cl field buffer index)))
                              ((and slot (keywordp cl))
                               (map () #'(lambda (v)
                                           (when (or (eq cl :bool) (not (null v)))
                                             (setq index (serialize-prim v cl field buffer index))))
                                       (safe-slot-value object slot)))
                              ((and slot (typep msg 'protobuf-enum))
                               (map () #'(lambda (v)
                                           (when (not (null v))
                                             (setq index (serialize-enum v msg field buffer index))))
                                       (safe-slot-value object slot)))
                              ((typep msg 'protobuf-message)
                               (dolist (v (if slot (safe-slot-value object slot) (list object)))
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
                               (let ((v (safe-slot-value object slot)))
                                 (when (or (eq cl :bool) (not (null v)))
                                   (setq index (serialize-prim v cl field buffer index)))))
                              ((and slot (typep msg 'protobuf-enum))
                               (let ((v (safe-slot-value object slot)))
                                 (when (not (null v))
                                   (setq index (serialize-enum v msg field buffer index)))))
                              ((typep msg 'protobuf-message)
                               (let ((v (if slot (safe-slot-value object slot) object)))
                                 (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                       (len (object-size v protobuf :visited visited)))
                                   (setq index (encode-uint32 tag buffer index))
                                   (setq index (encode-uint32 len buffer index)))
                                 (when (not (null v))
                                   (map () (curry #'do-field v (cons msg trace))
                                           (proto-fields msg)))))))))))
      (map () (curry #'do-field object (list message protobuf)) (proto-fields message)))))


(defun deserialize-object-from-stream (class protobuf &key (stream *standard-input*))
  (let* ((size    (file-length stream))
         (buffer  (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    (deserialize-object class protobuf buffer)))

;; Allow specialized methods
(defgeneric deserialize-object (class protobuf buffer))

;; The default method uses meta-data from the protobuf "schema"
(defmethod deserialize-object ((class symbol) protobuf buffer)
  (check-type protobuf (or protobuf protobuf-message))
  (let ((length (length buffer))
        (index  0))
    (labels ((deserialize (class trace &optional (end length))
               (let ((object  (make-instance class))
                     (message (loop for p in trace
                                    thereis (or (find-message-for-class p class)
                                                (find-enum-for-type p class)))))
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
                            (slot  (proto-value field)))
                       ;;---*** Check for mismatched types, running past end of buffer, etc
                       (declare (ignore type))
                       (cond ((eq (proto-required field) :repeated)
                              (cond ((and (proto-packed field) (packed-type-p cl))
                                     (multiple-value-bind (values idx)
                                         (deserialize-packed cl field buffer index)
                                       (setq index idx)
                                       (setf (slot-value object slot) values)))
                                    ((keywordp cl)
                                     (multiple-value-bind (val idx)
                                         (deserialize-prim cl field buffer index)
                                       (setq index idx)
                                       (setf (slot-value object slot) (nconc (slot-value object slot) (list val)))))
                                    ((typep msg 'protobuf-enum)
                                     (multiple-value-bind (val idx)
                                         (deserialize-enum msg field buffer index)
                                       (setq index idx)
                                       (setf (slot-value object slot) (nconc (slot-value object slot) (list val)))))
                                    ((typep msg 'protobuf-message)
                                     (multiple-value-bind (len idx)
                                         (decode-uint32 buffer index)
                                       (setq index idx)
                                       (let ((obj (deserialize cl (cons msg trace) (+ index len))))
                                         (setf (slot-value object slot) (nconc (slot-value object slot) (list obj))))))))
                             (t
                              (cond ((keywordp cl)
                                     (multiple-value-bind (val idx)
                                         (deserialize-prim cl field buffer index)
                                       (setq index idx)
                                       (setf (slot-value object slot) val)))
                                    ((typep msg 'protobuf-enum)
                                     (multiple-value-bind (val idx)
                                         (deserialize-enum msg field buffer index)
                                       (setq index idx)
                                       (setf (slot-value object slot) val)))
                                    ((typep msg 'protobuf-message)
                                     (multiple-value-bind (len idx)
                                         (decode-uint32 buffer index)
                                       (setq index idx)
                                       (let ((obj (deserialize cl (cons msg trace) (+ index len))))
                                         (setf (slot-value object slot) obj)))))))))))))
      (deserialize class (list protobuf)))))


;; Allow specialized methods
(defgeneric object-size (object protobuf &key visited))

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
    (labels ((safe-slot-value (object slot)
               (if (slot-boundp object slot)
                 (slot-value object slot)
                 nil))
             (do-field (object trace field)
               ;;---*** How can we detect cycles?
               (let* ((cl  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                      (msg (and cl (loop for p in trace
                                         thereis (or (find-message-for-class p cl)
                                                     (find-enum-for-type p cl)))))
                      (slot (proto-value field)))
                 (cond ((eq (proto-required field) :repeated)
                        (cond ((and slot (proto-packed field) (packed-type-p cl))
                               (iincf size (packed-size (safe-slot-value object slot) cl field)))
                              ((and slot (keywordp cl))
                               (map () #'(lambda (v)
                                           (when (or (eq cl :bool) (not (null v)))
                                             (iincf size (prim-size v cl field))))
                                       (safe-slot-value object slot)))
                              ((and slot (typep msg 'protobuf-enum))
                               (map () #'(lambda (v)
                                           (when (not (null v))
                                             (iincf size (enum-size v msg field))))
                                       (safe-slot-value object slot)))
                              ((typep msg 'protobuf-message)
                               (dolist (v (if slot (safe-slot-value object slot) (list object)))
                                 (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                       (len (object-size v protobuf :visited visited)))
                                   (iincf size (length32 tag))
                                   (iincf size (length32 len)))
                                 (map () (curry #'do-field v (cons msg trace))
                                      (proto-fields msg))))))
                       (t
                        (cond ((and slot (keywordp cl))
                               (let ((v (safe-slot-value object slot)))
                                 (when (or (eq cl :bool) (not (null v)))
                                   (iincf size (prim-size v cl field)))))
                              ((and slot (typep msg 'protobuf-enum))
                               (let ((v (safe-slot-value object slot)))
                                 (when (not (null v))
                                   (iincf size (enum-size (safe-slot-value object slot) msg field)))))
                              ((typep msg 'protobuf-message)
                               (let ((v (if slot (safe-slot-value object slot) object)))
                                 (when (not (null v))
                                   (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
                                         (len (object-size v protobuf :visited visited)))
                                     (iincf size (length32 tag))
                                     (iincf size (length32 len)))
                                   (map () (curry #'do-field v (cons msg trace))
                                           (proto-fields msg)))))))))))
      (map () (curry #'do-field object (list message protobuf)) (proto-fields message))
      (when visited
        (setf (gethash object visited) size))   ;cache the size
      size)))
