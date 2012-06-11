;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CL-USER")


(asdf:defsystem :cl-protobufs-tests
  :name "Protobufs Tests"
  :author "Scott McKay"
  :version "1.0"
  :licence "MIT-style"
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
     (:module "brown-tests-proto"
	      :serial t
	      :pathname #p""
	      :components
		((:protobuf-file "testproto1")
		 (:protobuf-file "testproto2")))
     (:module "brown-tests"
	      :serial t
	      :pathname #p""
	      :depends-on ("object-level-tests" "brown-tests-proto")
	      :components
		((:file "quick-tests")
		 (:static-file "golden.data")))

     ;; Google's own protocol buffers and protobuf definitions tests
     #+++notyet
     (:module "google-tests-proto"
	      :serial t
	      :pathname #p""
	      :components
		((:protobuf-file "descriptor")
		 (:protobuf-file "unittest_import")
		 (:protobuf-file "unittest" :depends-on ("unittest_import"))))
     #+++notyet
     (:module "google-tests"
	      :serial t
	      :pathname #p""
	      :depends-on ("object-level-tests" "google-tests-proto")
	      :components
		((:file "full-tests")
		 (:static-file "golden_message.data")
		 (:static-file "golden_packed_message.data")))))
