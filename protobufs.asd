;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CL-USER")


(defsystem :protobufs
    :name "Protobufs"
    :author "Scott McKay"
    :version "0.1"
    :maintainer '("Scott McKay")
    :licence "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
    :description      "Protobufs for Common Lisp"
    :long-description "Protobufs for Common Lisp"
    :depends-on (:cl-ppcre
                 :closer-mop
                 :split-sequence
                 :drakma
                 :cl-unicode)
    :serial t
    :components
      ((:module "protobufs"
                :serial t
                :components ((:file "proto-pkgdcl")
                             (:file "utilities")
                             (:file "model-classes")
                             (:file "printer")
                             (:file "parser")
                             (:file "define-proto")
                             (:file "upgradable")
                             (:file "clos-transform")
                             (:file "wire-format")
                             (:file "text-format")
                             (:file "serialize")
                             (:file "asdf-support")
                             (:file "examples")))))
