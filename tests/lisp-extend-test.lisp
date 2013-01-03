;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-TEST")

(define-test extend-test ()
  (let* ((schema (proto:find-schema "ExtendTest"))
         (imported-schema (proto:find-schema "ExtendTestBase"))
         (foo (proto:find-message schema "Foo"))
         (bar (proto:find-message schema "Bar"))
         (quux (proto:find-message schema "Quux"))
         (ifoo (proto:find-message imported-schema "Foo"))
         (ibar (proto:find-message imported-schema "Bar"))
         (ibaz (proto:find-message imported-schema "Baz")))
    (destructuring-bind (local-local local-import import-local import-import)
        (proto-impl:proto-extenders bar)
      ;; Are we extending the right message?
      (assert-equal (proto-impl:proto-class local-local)
                    (proto-impl:proto-class foo))
      (assert-equal (proto-impl:proto-class local-import)
                    (proto-impl:proto-class foo))
      (assert-equal (proto-impl:proto-class import-local)
                    (proto-impl:proto-class ifoo))
      (assert-equal (proto-impl:proto-class import-import)
                    (proto-impl:proto-class ifoo))
      ;; Is the extended field of the right type?
      (assert-equal (proto-impl:proto-class
                     (first (proto-impl:proto-extended-fields local-local)))
                    (proto-impl:proto-class bar))
      (assert-equal (proto-impl:proto-class
                     (first (proto-impl:proto-extended-fields local-import)))
                    (proto-impl:proto-class ibar))
      (assert-equal (proto-impl:proto-class
                     (first (proto-impl:proto-extended-fields import-local)))
                    (proto-impl:proto-class bar))
      (assert-equal (proto-impl:proto-class
                     (first (proto-impl:proto-extended-fields import-import)))
                    (proto-impl:proto-class ibar)))
    ;; Smaller stand-alone test
    (let ((ebaz (first (proto-extenders quux))))
      (assert-equal (proto-impl:proto-class ebaz) (proto-impl:proto-class ibaz))
      (assert-equal (proto-impl:proto-class (first (proto-extended-fields ebaz)))
                    (proto-impl:proto-class quux)))))

(register-test 'extend-test)
