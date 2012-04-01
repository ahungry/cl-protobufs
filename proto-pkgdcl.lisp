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
   "PROTOBUF-OPTION"
   "PROTOBUF-ENUM"
   "PROTOBUF-ENUM-VALUE"
   "PROTOBUF-MESSAGE"
   "PROTOBUF-FIELD"
   "PROTOBUF-EXTENSION"
   "PROTOBUF-SERVICE"
   "PROTOBUF-RPC"

   ;; Printing
   "WRITE-PROTOBUF"

   ;; Parsing
   "PARSE-PROTOBUF-FROM-FILE"
   "PARSE-PROTOBUF-FROM-STREAM"

   ;; Protobuf defining macros
   "DEFINE-PROTO"
   "DEFINE-ENUM"
   "DEFINE-MESSAGE"
   "DEFINE-EXTENSION"
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
   "ABSTRACT-PROTOBUF"
   "BASE-PROTOBUF"
   "PROTO-CLASS"
   "PROTO-CLASS-OVERRIDE"
   "PROTO-DEFAULT"
   "PROTO-DOCUMENTATION"
   "PROTO-ENUMS"
   "PROTO-EXTENSION-FROM"
   "PROTO-EXTENSION-TO"
   "PROTO-EXTENSIONS"
   "PROTO-FIELDS"
   "PROTO-FUNCTION"
   "PROTO-IMPORTS"
   "PROTO-INDEX"
   "PROTO-INPUT-TYPE"
   "PROTO-MESSAGES"
   "PROTO-NAME"  
   "PROTO-OPTIMIZE"
   "PROTO-OPTIONS"
   "PROTO-OUTPUT-TYPE"
   "PROTO-PACKAGE"
   "PROTO-PACKED"
   "PROTO-READER"
   "PROTO-REQUIRED"
   "PROTO-RPCS"
   "PROTO-SERVICES"
   "PROTO-SYNTAX"
   "PROTO-TYPE"
   "PROTO-VALUE"
   "PROTO-VALUES"
   "FIND-PROTOBUF"
   "FIND-MESSAGE"
   "FIND-ENUM"
   "FIND-OPTION"

   ;; Printing
   "WRITE-PROTOBUF-AS"

   ;; Protobuf defining macros
   "ENSURE-ALL-PROTOBUFS"
   "ENSURE-PROTOBUF"
   
   ;; CLOS to Protobufs transformer
   "CLASS-TO-PROTOBUF-MESSAGE"
   "SLOT-TO-PROTOBUF-FIELD"
   "CLOS-TYPE-TO-PROTOBUF-TYPE"
   "CLOS-TYPE-TO-PROTOBUF-REQUIRED"
   "CLOS-INIT-TO-PROTOBUF-DEFAULT"
   "PROTOBUF-DEFAULT-TO-CLOS-INIT"

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
   "GENERATE-SERIALIZER"
   "GENERATE-DESERIALIZER"
   "GENERATE-OBJECT-SIZE"

   ;; Raw encoding and decoding
   "$WIRE-TYPE-VARINT"
   "$WIRE-TYPE-64BIT"
   "$WIRE-TYPE-STRING"
   "$WIRE-TYPE-32BIT"
   "MAKE-TAG"
   "ENCODE-UINT32"
   "ENCODE-UINT64"
   "ENCODE-FIXED32"
   "ENCODE-FIXED64"
   "ENCODE-SINGLE"
   "ENCODE-DOUBLE"
   "ENCODE-OCTETS"
   "ZIG-ZAG-ENCODE32"
   "ZIG-ZAG-ENCODE64"
   "DECODE-UINT32"
   "DECODE-UINT64"
   "DECODE-FIXED32"
   "DECODE-FIXED64"
   "DECODE-SINGLE"
   "DECODE-DOUBLE"
   "DECODE-OCTETS"
   "ZIG-ZAG-DECODE32"
   "ZIG-ZAG-DECODE64"
   "LENGTH32"
   "LENGTH64"
   "SKIP-ELEMENT"

   ;; Utilities
   "CLASS-NAME->PROTO"
   "ENUM-NAME->PROTO"
   "SLOT-NAME->PROTO"
   "PROTO->CLASS-NAME"
   "PROTO->ENUM-NAME"
   "PROTO->SLOT-NAME"
   "PROTOBUFS-WARNING"
   "PROTOBUFS-WARN"))
