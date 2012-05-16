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

  ;; Some types useful for defining messages
  (:export
   "INT32"
   "INT64"
   "UINT32"
   "UINT64"
   "SINT32"
   "SINT64"
   "FIXED32"
   "FIXED64"
   "SFIXED32"
   "SFIXED64"
   "BYTE-VECTOR"
   "LIST-OF")

  ;; ASDF module type
  (:export
   "PROTO-FILE")
  
  ;; The Protobufs API
  (:export
   ;; Model classes
   "PROTOBUF-SCHEMA"
   "PROTOBUF-OPTION"
   "PROTOBUF-ENUM"
   "PROTOBUF-ENUM-VALUE"
   "PROTOBUF-MESSAGE"
   "PROTOBUF-FIELD"
   "PROTOBUF-EXTENSION"
   "PROTOBUF-SERVICE"
   "PROTOBUF-METHOD"

   ;; .proto parsing and printing
   "PARSE-SCHEMA-FROM-FILE"
   "PARSE-SCHEMA-FROM-STREAM"
   "WRITE-SCHEMA"

   ;; Protobuf defining macros
   "DEFINE-SCHEMA"
   "DEFINE-ENUM"
   "DEFINE-MESSAGE"
   "DEFINE-EXTEND"
   "DEFINE-EXTENSION"
   "DEFINE-GROUP"
   "DEFINE-SERVICE"

   ;; Upgradability and equality testing
   "SCHEMA-UPGRADABLE"
   "SCHEMAS-EQUAL"

   ;; CLOS to Protobufs transformer
   "WRITE-SCHEMA-FOR-CLASSES"
   "GENERATE-SCHEMA-FOR-CLASSES"

   ;; Serialization and deserialization (wire format)
   "SERIALIZE-OBJECT-TO-FILE"
   "SERIALIZE-OBJECT-TO-STREAM"
   "SERIALIZE-OBJECT"
   "DESERIALIZE-OBJECT-FROM-FILE"
   "DESERIALIZE-OBJECT-FROM-STREAM"
   "DESERIALIZE-OBJECT"
   "OBJECT-SIZE"

   ;; Serialization and deserialization (text format)
   "PARSE-TEXT-FORMAT"
   "PRINT-TEXT-FORMAT")

  ;; Miscellaneous bits of the API
  (:export
   "GET-EXTENSION"
   "SET-EXTENSION"
   "HAS-EXTENSION"
   "CLEAR-EXTENSION"
   "OBJECT-INITIALIZED-P"
   "SLOT-INITIALIZED-P"
   "REINITIALIZE-OBJECT")

  ;; The Python "compatibility" API
  (:export
   "CLEAR"
   "HAS-FIELD"
   "IS-INITIALIZED"
   "OCTET-SIZE"
   "SERIALIZE"
   "MERGE-FROM-ARRAY"
   "MERGE-FROM-MESSAGE"))

(defpackage protobufs-implementation
  (:nicknames :proto-impl)
  (:use :common-lisp :protobufs)

  (:import-from :closer-mop
   "CLASS-SLOTS"
   "CLASS-DIRECT-SLOTS"
   "CLASS-PRECEDENCE-LIST"
   "SLOT-DEFINITION-NAME"
   "SLOT-DEFINITION-TYPE"
   "SLOT-DEFINITION-INITFORM"
   "SLOT-DEFINITION-READERS")

  (:export
   ;; Model class protocol
   "ABSTRACT-PROTOBUF"
   "BASE-PROTOBUF"
   "PROTO-ALIAS-FOR"
   "PROTO-CLASS"
   "PROTO-DEFAULT"
   "PROTO-DOCUMENTATION"
   "PROTO-ENUMS"
   "PROTO-EXTENSION-FROM"
   "PROTO-EXTENSION-TO"
   "PROTO-EXTENDED-FIELDS"
   "PROTO-EXTENDERS"
   "PROTO-EXTENSIONS"
   "PROTO-FIELDS"
   "PROTO-FUNCTION"
   "PROTO-IMPORTED-SCHEMAS"
   "PROTO-IMPORTS"
   "PROTO-INDEX"
   "PROTO-INPUT-NAME"
   "PROTO-INPUT-TYPE"
   "PROTO-LISP-PACKAGE"
   "PROTO-MESSAGE-TYPE"
   "PROTO-MESSAGES"
   "PROTO-METHODS"
   "PROTO-NAME"
   "PROTO-OPTIONS"
   "PROTO-OUTPUT-NAME"
   "PROTO-OUTPUT-TYPE"
   "PROTO-PACKAGE"
   "PROTO-PACKED"
   "PROTO-PARENT"
   "PROTO-READER"
   "PROTO-REQUIRED"
   "PROTO-SERVICES"
   "PROTO-SYNTAX"
   "PROTO-TYPE"
   "PROTO-VALUE"
   "PROTO-VALUES"
   "PROTO-WRITER"

   ;; Object lookup
   "*ALL-SCHEMAS*"
   "*ALL-MESSAGES*"
   "FIND-SCHEMA"
   "FIND-MESSAGE-FOR-CLASS"
   "FIND-MESSAGE"
   "FIND-ENUM"
   "FIND-FIELD"
   "FIND-OPTION"

   ;; Printing
   "WRITE-SCHEMA-AS"

   ;; Protobuf defining macros
   "ENSURE-ALL-SCHEMAS"
   "ENSURE-SCHEMA"

   ;; CLOS to Protobufs transformer
   "*ALIAS-EXISTING-CLASSES*"
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
   "$WIRE-TYPE-START-GROUP"
   "$WIRE-TYPE-END-GROUP"
   "$WIRE-TYPE-32BIT"
   "MAKE-TAG"
   "ENCODE-UINT32"
   "ENCODE-UINT64"
   "ENCODE-FIXED32"
   "ENCODE-FIXED64"
   "ENCODE-SINGLE"
   "ENCODE-DOUBLE"
   "ENCODE-STRING"
   "ENCODE-OCTETS"
   "ZIG-ZAG-ENCODE32"
   "ZIG-ZAG-ENCODE64"
   "DECODE-UINT32"
   "DECODE-UINT64"
   "DECODE-INT32"
   "DECODE-INT64"
   "DECODE-FIXED32"
   "DECODE-FIXED64"
   "DECODE-SINGLE"
   "DECODE-DOUBLE"
   "DECODE-STRING"
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
