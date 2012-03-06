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


;;; Package declaration for Protoubfs

(defpackage protobufs
  (:nicknames :proto)

  (:export
   ;; Model classes
   "PROTOBUF"
   "PROTOBUF-MESSAGE"
   "PROTOBUF-ENUM"
   "PROTOBUF-ENUM"
   "PROTOBUF-ENUM-VALUE"
   "PROTOBUF-FIELD"
   "PROTOBUF-SERVICE"
   "PROTOBUF-RPC"

   ;; Printing
   "WRITE-PROTOBUF"

   ;; Parsing
   "PARSE-PROTOBUF"
   "PARSE-PROTOBUF-FROM-STREAM"

   ;; Protobuf defining macros
   "DEFINE-PROTO"
   "DEFINE-ENUM"
   "DEFINE-MESSAGE"
   "DEFINE-SERVICE"
   
   ;; Upgradability testing
   "PROTOBUF-UPGRADABLE"

   ;; CLOS to Protobufs transformer
   "WRITE-PROTOBUF-SCHEMA-FOR-CLASSES"

   ;; Serialization
   "SERIALIZE-OBJECT-TO-STREAM"
   "SERIALIZE-OBJECT"
   "DESERIALIZE-OBJECT-FROM-STREAM"
   "DESERIALIZE-OBJECT"
   "OBJECT-SIZE"

   ;; Text printing
   "PRINT-TEXT-FORMAT"))

(defpackage protobufs-implementation
  (:nicknames :proto-impl)
  (:use :common-lisp :quux :protobufs)

  (:export
   ;; Model class protocol
   "PROTO-CLASS"
   "PROTO-COMMENT"
   "PROTO-DEFAULT"
   "PROTO-ENUM-NAME"
   "PROTO-ENUMS"
   "PROTO-FIELDS"
   "PROTO-IMPORTS"
   "PROTO-INDEX"
   "PROTO-INPUT-TYPE"
   "PROTO-MESSAGES"
   "PROTO-NAME"  
   "PROTO-OPTIONS"
   "PROTO-OUTPUT-TYPE"
   "PROTO-PACKAGE"
   "PROTO-PACKED"
   "PROTO-REQUIRED"
   "PROTO-RPCS"
   "PROTO-SERVICES"
   "PROTO-SYNTAX"
   "PROTO-TYPE"
   "PROTO-VALUE"
   "PROTO-VALUES"
   "FIND-MESSAGE-FOR-CLASS"
   "FIND-ENUM-FOR-TYPE"

   ;; CLOS to Protobufs transformer
   "CLASS-TO-PROTOBUF-MESSAGE"
   "SLOT-TO-PROTOBUF-FIELD"
   "CLOS-TYPE-TO-PROTOBUF-TYPE"
   "CLOS-TYPE-TO-PROTOBUF-REQUIRED"
   "CLOS-INIT-TO-PROTOBUF-DEFAULT"

   ;; Serialization
   "SERIALIZE-PRIM"
   "SERIALIZE-PACKED"
   "SERIALIZE-ENUM"
   "DESERIALIZE-PRIM"
   "DESERIALIZE-PACKED"
   "DESERIALIZE-ENUM"
   "PRIM-SIZE"
   "PACKED-SIZE"
   "ENUM-SIZE"

   ;; Raw encoding and decoding
   "ENCODE-UINT32"
   "ENCODE-UINT64"
   "ENCODE-SINGLE"
   "ENCODE-DOUBLE"
   "ENCODE-OCTETS"
   "ZIG-ZAG-ENCODE32"
   "ZIG-ZAG-ENCODE64"
   "DECODE-UINT32"
   "DECODE-UINT64"
   "DECODE-SINGLE"
   "DECODE-DOUBLE"
   "DECODE-OCTETS"
   "ZIG-ZAG-DECODE32"
   "ZIG-ZAG-DECODE64"
   "LENGTH32"
   "LENGTH64"

   ;; Utilities
   "PROTO-CLASS-NAME"
   "PROTO-ENUM-NAME"
   "PROTO-FIELD-NAME"))
