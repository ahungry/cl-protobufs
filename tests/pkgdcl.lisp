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
   "FIND-METHOD"))

(defpackage protobuf-unittest
  (:use :common-lisp :protobufs)
  (:nicknames :pbtest))

(defpackage protobuf-unittest-import
  (:use :common-lisp :protobufs)
  (:nicknames :pbtestimp))
