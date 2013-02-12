;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2013 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-TEST")

(define-test case-preservation-test ()
  (let ((service (proto:find-service :case-preservation "QUUXService")))
    (assert-true service)
    ;; We're reaching into the implementation to verify the objects have
    ;; been properly constructed.
    (let ((method (proto-impl:find-method service "QUUXMethod")))
      (assert-true method)
      (assert-equal (proto-input-name method) "QUUXRequest"
                    :test #'string=)
      (assert-equal (proto-output-name method) "QUUXResponse"
                    :test #'string=))))

(register-test 'case-preservation-test)
