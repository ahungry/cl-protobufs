;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Ben Wagner                                      ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-IMPL")

;;; Protocol buffers conditions

(define-condition undefined-type (simple-error)
  ((type-name :type string
              :reader error-type-name
              :initarg :type-name
              :documentation "The name of the type which can not be found."))
  (:documentation "Indicates that a schema references a type which has not been defined.")
  (:default-initargs :format-control "Undefined type:")
  (:report (lambda (condition stream)
             (format stream "~? ~s"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (error-type-name condition)))))

(define-condition undefined-field-type (undefined-type)
  ((field :type protobuf-field
          :reader error-field
          :initarg :field
          :documentation "The field whose type is TYPE-NAME."))
  (:documentation "Indicates that a schema contains a message with a field whose type is not a
                   primitive type and is not a known message (or extend) or enum.")
  (:report (lambda (condition stream)
             (format stream "~? Field ~s in message ~s has unknown type ~s."
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (error-field condition)
                     (proto-parent (error-field condition))
                     (error-type-name condition)))))

(define-condition undefined-method-type (undefined-type)
  ((method :type protobuf-method
           :reader error-method
           :initarg :method
           :documentation "The method that references TYPE-NAME.")
   (where :type string
          :reader error-where
          :initarg :where
          :documentation "Description of which type referenced by the method is undefined."))
  (:documentation "Superclass for `undefined-type' errors related to a `protobuf-method'.  Indicates
                   that a schema contains a service with a method whose input, output, or stream
                   type is not a known message (or extend).")
  (:report (lambda (condition stream)
             (format stream "~? ~a type for rpc ~s in service ~s has unknown type ~s."
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (error-where condition)
                     (error-method condition)
                     (proto-parent (error-method condition))
                     (error-type-name condition)))))

(define-condition undefined-input-type (undefined-method-type)
  ()
  (:default-initargs :where "Input"))

(define-condition undefined-output-type (undefined-method-type)
  ()
  (:default-initargs :where "Output"))

(define-condition undefined-stream-type (undefined-method-type)
  ()
  (:default-initargs :where "Stream"))
