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


;;; Print objects using Protobufs text format

(defgeneric print-text-format (object protobuf &key stream)
  (:documentation
   "Prints the object 'object' as a protobuf object defined in the schema 'protobuf'
    onto the stream 'stream' using the textual format."))

(defmethod print-text-format ((object standard-object) protobuf &key (stream *standard-output*))
  (check-type protobuf (or protobuf protobuf-message))
  (let* ((class   (class-of object))
         (message (find-message-for-class protobuf class)))
    (assert message ()
            "There is no Protobuf message for the class ~S" class)
    (labels ((safe-slot-value (object slot)
               (if (slot-boundp object slot)
                 (slot-value object slot)
                 nil))
             (do-field (object trace indent field)
               ;; We don't do cycle detection here
               ;; If the client needs it, he can define his own 'print-text-format'
               ;; method to clean things up first
               (let* ((cl  (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                      (msg (and cl (loop for p in trace
                                         thereis (or (find-message-for-class p cl)
                                                     (find-enum-for-type p cl)))))
                      (slot (proto-value field)))
                 (cond ((eq (proto-required field) :repeated)
                        (cond ((and slot (keywordp cl))
                               (map () #'(lambda (v)
                                           (when (or (eq cl :bool) (not (null v)))
                                             (print-prim v cl field stream indent)))
                                       (safe-slot-value object slot)))
                              ((and slot (typep msg 'protobuf-enum))
                               (map () #'(lambda (v)
                                           (when (not (null v))
                                             (print-enum v msg field stream indent)))
                                       (safe-slot-value object slot)))
                              ((typep msg 'protobuf-message)
                               (let ((values (if slot (safe-slot-value object slot) (list object))))
                                 (when values
                                   (format stream "~&~VT~A:~%" (+ indent 2) (proto-name field))
                                   (let ((indent (+ indent 4)))
                                     (dolist (v values)
                                       (format stream "~&~VT~A {~%" indent (proto-name msg))
                                       (map () (curry #'do-field v (cons msg trace) indent)
                                               (proto-fields msg))
                                       (format stream "~&~VT}~%" indent))))))))
                       (t
                        (cond ((and slot (keywordp cl))
                               (let ((v (safe-slot-value object slot)))
                                 (when (or (eq cl :bool) (not (null v)))
                                   (print-prim v cl field stream indent))))
                              ((and slot (typep msg 'protobuf-enum))
                               (let ((v (safe-slot-value object slot)))
                                 (when (not (null v))
                                   (print-enum v msg field stream indent))))
                              ((typep msg 'protobuf-message)
                               (let ((v (if slot (safe-slot-value object slot) object)))
                                 (when v
                                   (format stream "~&~VT~A:~%" (+ indent 2) (proto-name field))
                                   (let ((indent (+ indent 4)))
                                     (format stream "~&~VT~A {~%" indent (proto-name msg))
                                     (map () (curry #'do-field v (cons msg trace) indent)
                                             (proto-fields msg))
                                     (format stream "~&~VT}~%" indent)))))))))))
      (format stream "~&~A {~%" (proto-name message))
      (map () (curry #'do-field object (list message protobuf) 0) (proto-fields message))
      (format stream "~&}~%")
      nil)))

(defun print-prim (val type field stream &optional (indent 0))
  (when val
    (format stream "~&~VT~A: " (+ indent 2) (proto-name field))
    (ecase type
      ((:int32 :uint32 :int64 :uint64 :sint32 :sint64
        :fixed32 :sfixed32 :fixed64 :sfixed64)
       (format stream "~D~%" val))
      ((:string)
       (format stream "\"~A\"~%" val))
      ((:bytes)
       (format stream "~S~%" val))
      ((:bool)
       (format stream "~A~%" (if (zerop val) "false" "true")))
      ((:float :double)
       (format stream "~D~%" val))
      ;; A few of our homegrown types
      ((:symbol)
       (format stream "~A~%" val))
      ((:date :time :datetime :timestamp)
       (format stream "~D~%" val)))))

(defun print-enum (val enum field stream &optional (indent 0))
  (when val
    (format stream "~&~VT~A: " (+ indent 2) (proto-name field))
    (let ((name (let ((e (find val (proto-values enum) :key #'proto-value)))
                  (and e (proto-name e)))))
      (format stream "~A~%" name))))
