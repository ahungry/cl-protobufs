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


(asdf:defsystem :protobufs
    :name "Protobufs"
    :author "Scott McKay"
    :version "1.0"
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
    :maintainer '("Scott McKay")
    :description      "Protobufs for Common Lisp"
    :long-description "Protobufs for Common Lisp"
    :depends-on (:closer-mop :babel)
    :serial t
    :components
      ((:module "packages"
                :serial t
                :pathname #p""
                :components
                 ((:file "pkgdcl")))
       (:module "models"
                :serial t
                :pathname #p""
                :depends-on ("packages")
                :components
                  ((:file "utilities")
                   (:file "model-classes")))
       (:module "parsing"
                :serial t
                :pathname #p""
                :depends-on ("models")
                :components
                  ((:file "printer")
                   (:file "parser")))
       (:module "schema"
                :serial t
                :pathname #p""
                :depends-on ("models")
                :components
                  ((:file "define-proto")
                   (:file "upgradable")
                   (:file "clos-transform")))
       (:module "serialization"
                :serial t
                :pathname #p""
                :depends-on ("models")
                :components
                  ((:file "text-format")
                   (:file "wire-format")
                   (:file "serialize")))
       (:module "misc"
                :serial t
                :pathname #p""
                :depends-on ("models" "parsing" "schema" "serialization")
                :components
                  ((:file "api")
                   (:file "asdf-support")
                   (:file "examples")))))

(pushnew :protobufs *features*)
