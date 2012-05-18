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


(asdf:defsystem protobufs-test
    :name "Protobufs Test"
    :author "Scott McKay"
    :version "1.0"
    :maintainer '("Scott McKay")
    :description      "Test code for Protobufs for Common Lisp"
    :long-description "Test code for Protobufs for Common Lisp"
    :depends-on (:protobufs :quux :test-tools
                 ;; Some of these tests use QRes business data
                 #+qres :qres-core)
    :serial t
    :components
      ((:module "packages"
                :serial t
                :pathname #p""
                :components
                 ((:file "pkgdcl")))
       ;; Wire format tests
       (:module "wire-level-tests"
                :serial t
                :pathname #p""
                :depends-on ("packages")
                :components
                  ((:file "varint-tests")
                   (:file "wire-tests")))

       ;; Simple tests
       (:module "object-level-tests"
                :serial t
                :pathname #p""
                :depends-on ("wire-level-tests")
                :components
                ((:file "serialization-tests")
                 (:file "stability-tests")))

       ;; Bob Brown's protocol buffers tests
       #+++notyet
       (:module "brown-tests"
                :serial t
                :pathname #p""
                :depends-on ("wire-level-tests" "object-level-tests")
                :components
                  ((:protobuf "testproto1")
                   (:protobuf "testproto2")
                   (:file "quick-tests" :depends-on ("testproto1" "testproto2"))
                   (:static-file "golden.data")))

       ;; Google's own protocol buffers and protobuf definitions tests
       #+++notyet
       (:module "google-tests"
                :serial t
                :pathname #p""
                :depends-on ("brown-tests")
                :components
                  ((:protobuf "descriptor")
                   (:protobuf "unittest_import")
                   (:protobuf "unittest" :depends-on ("unittest_import"))
                   (:file "full-tests" :depends-on ("unittest"))
                   (:static-file "golden_message.data")
                   (:static-file "golden_packed_message.data")))))
