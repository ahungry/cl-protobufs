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

;;; Make sure we can import a schema by symbol name in a pure-lisp
;;; protobuf defintion.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (proto:define-schema symbol-imported-schema
      (:package proto_test)
    (proto:define-message symbol-imported-message ()))

)       ;eval-when

(proto:define-schema symbol-importer-schema
    (:package proto_test
     :import symbol-imported-schema)
  (proto:define-message symbol-importer-message ()
    (imported-type-field :type (or null symbol-imported-message))))
