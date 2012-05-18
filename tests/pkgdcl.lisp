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


;;; Package declaration for Protobufs tests

(defpackage protobufs-test
  (:use :common-lisp :protobufs :protobufs-implementation)
  (:nicknames :proto-test))

(defpackage protobuf-unittest
  (:use :common-lisp :protobufs))

(defpackage protobuf-unittest-import
  (:use :common-lisp :protobufs))


