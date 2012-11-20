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


;;; Package declaration for Protobufs tests

(defpackage protobufs-test
  (:nicknames :proto-test)
  (:use :common-lisp :protobufs :protobufs-implementation)
  (:shadowing-import-from :protobufs-implementation
   "FIND-METHOD")
  #+test-tools
  (:import-from :qtest
   "DEFINE-TEST"
   "DEFINE-TEST-SUITE"
   "REGISTER-TEST"
   "RUN-TEST"
   "ASSERT-EQUAL"
   "ASSERT-TRUE"
   "ASSERT-FALSE"
   "ASSERT-ERROR"))

(defpackage protobuf-unittest
  (:use :common-lisp :protobufs)
  (:nicknames :pbtest))

(defpackage protobuf-unittest-import
  (:use :common-lisp :protobufs)
  (:nicknames :pbtestimp))

(defpackage protobuf-geodata
  (:use :common-lisp :protobufs)
  (:nicknames :geodata))

(defpackage protobuf-forward-reference-unittest
  (:use :common-lisp :protobufs))
