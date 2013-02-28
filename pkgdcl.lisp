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


;;; Package declaration for Protobufs

(defpackage protobufs
  (:nicknames :proto)
  (:use)

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
   "LIST-OF"
   "VECTOR-OF"
   "BYTE-VECTOR"
   "MAKE-BYTE-VECTOR")

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
   "PROTOBUF-TYPE-ALIAS"        ;Lisp-only extension

   ;; Conditions
   "UNDEFINED-FIELD-TYPE"
   "UNDEFINED-INPUT-TYPE"
   "UNDEFINED-OUTPUT-TYPE"
   "UNDEFINED-STREAM-TYPE"
   "ERROR-TYPE-NAME"
   "ERROR-FIELD"
   "ERROR-METHOD"

   ;; Object lookup
   "FIND-MESSAGE"
   "FIND-MESSAGE-FOR-CLASS"
   "FIND-SCHEMA"
   "FIND-SERVICE"

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
   "DEFINE-TYPE-ALIAS"          ;Lisp-only extension

   ;; Upgradability and equality testing
   "SCHEMA-UPGRADABLE"
   "SCHEMAS-EQUAL"

   ;; CLOS to Protobufs transformer
   "WRITE-SCHEMA-FOR-CLASSES"
   "GENERATE-SCHEMA-FOR-CLASSES"

   ;; Serialization and deserialization (wire format)
   "SERIALIZE-OBJECT-TO-FILE"
   "SERIALIZE-OBJECT-TO-STREAM"
   "SERIALIZE-OBJECT-TO-BYTES"
   "SERIALIZE-OBJECT"
   "DESERIALIZE-OBJECT-FROM-FILE"
   "DESERIALIZE-OBJECT-FROM-STREAM"
   "DESERIALIZE-OBJECT-FROM-BYTES"
   "DESERIALIZE-OBJECT"
   "OBJECT-SIZE"

   ;; Serialization and deserialization (text format)
   "PARSE-TEXT-FORMAT"
   "PRINT-TEXT-FORMAT")

  ;; Extensions
  (:export
   "GET-EXTENSION"
   "SET-EXTENSION"
   "HAS-EXTENSION"
   "CLEAR-EXTENSION")

  ;; The Python "compatibility" API
  (:export
   "IS-INITIALIZED"
   "CLEAR"
   "HAS-FIELD"
   "CLEAR-FIELD"
   "OCTET-SIZE"
   "SERIALIZE"
   "MERGE-FROM-ARRAY"
   "MERGE-FROM-MESSAGE")

  ;; Miscellany
  (:export
   "OBJECT-INITIALIZED-P"
   "SLOT-INITIALIZED-P"
   "REINITIALIZE-OBJECT"
   "REINITIALIZE-FIELD"
   "REINITIALIZE-SLOT"))


(defpackage protobufs-implementation
  (:nicknames :proto-impl)
  (:use :common-lisp :protobufs)

  (:shadow
   "FIND-METHOD")
  (:import-from :closer-mop
   "CLASS-SLOTS"
   "CLASS-DIRECT-SLOTS"
   "CLASS-PRECEDENCE-LIST"
   "CLASS-FINALIZED-P"
   "FINALIZE-INHERITANCE"
   "SLOT-DEFINITION-NAME"
   "SLOT-DEFINITION-TYPE"
   "SLOT-DEFINITION-INITFORM"
   "SLOT-DEFINITION-INITFUNCTION"
   "SLOT-DEFINITION-READERS"
   "SLOT-DEFINITION-WRITERS")

  (:export
   ;; Base class for messages
   "BASE-PROTOBUF-MESSAGE"

   ;; Model class protocol
   "ABSTRACT-PROTOBUF"
   "BASE-PROTOBUF"
   "PROTO-ALIAS-FOR"
   "PROTO-CLASS"
   "PROTO-CLIENT-STUB"
   "PROTO-CONC-NAME"
   "PROTO-DEFAULT"
   "PROTO-DOCUMENTATION"
   "PROTO-ENUMS"
   "PROTO-EXTENSION-FROM"
   "PROTO-EXTENSION-TO"
   "PROTO-EXTENDED-FIELDS"
   "PROTO-EXTENDERS"
   "PROTO-EXTENSIONS"
   "PROTO-FIELDS"
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
   "PROTO-QUALIFIED-NAME"
   "PROTO-READER"
   "PROTO-REQUIRED"
   "PROTO-SERVER-STUB"
   "PROTO-SERVICES"
   "PROTO-SLOT"
   "PROTO-SOURCE-LOCATION"
   "PROTO-STREAMS-NAME"
   "PROTO-STREAMS-TYPE"
   "PROTO-SYNTAX"
   "PROTO-TYPE"
   "PROTO-VALUE"
   "PROTO-VALUES"
   "PROTO-WRITER"

   ;; Type aliases, a Lisp-only extension
   "PROTO-TYPE-ALIASES"
   "PROTO-LISP-TYPE"
   "PROTO-PROTO-TYPE"
   "PROTO-SERIALIZER"
   "PROTO-DESERIALIZER"
   "FIND-TYPE-ALIAS"

   ;; Controls
   "*PROTOBUF*"
   "*PROTOBUF-PACKAGE*"
   "*PROTOBUF-CONC-NAME*"
   "*PROTOBUF-PATHNAME*"
   "*PROTOBUF-SEARCH-PATH*"
   "*PROTOBUF-OUTPUT-PATH*"
 
   ;; Object lookup
   "*ALL-SCHEMAS*"
   "*ALL-MESSAGES*"
   "FIND-ENUM"
   "FIND-FIELD"
   "FIND-METHOD"                ;if you ":use proto-impl", watch for name clash
   "FIND-OPTION"
   "REMOVE-OPTIONS"

   ;; Printing
   "WRITE-SCHEMA-AS"

   ;; Protobuf defining macros
   "ENSURE-ALL-SCHEMAS"
   "ENSURE-SCHEMA"

   ;; CLOS to Protobufs transformer
   "*ALIAS-EXISTING-CLASSES*"
   "CLASS-TO-PROTOBUF-MESSAGE"
   "SLOT-TO-PROTOBUF-FIELD"
   "LISP-TYPE-TO-PROTOBUF-TYPE"
   "CLOS-TYPE-TO-PROTOBUF-TYPE"
   "CLOS-TYPE-TO-PROTOBUF-REQUIRED"
   "CLOS-INIT-TO-PROTOBUF-DEFAULT"
   "PROTOBUF-DEFAULT-TO-CLOS-INIT"

   ;; Serialization
   "SERIALIZE-PRIM"
   "SERIALIZE-PACKED"
   "SERIALIZE-ENUM"
   "SERIALIZE-PACKED-ENUM"
   "DESERIALIZE-PRIM"
   "DESERIALIZE-PACKED"
   "DESERIALIZE-ENUM"
   "DESERIALIZE-PACKED-ENUM"
   "PRIM-SIZE"
   "PACKED-SIZE"
   "ENUM-SIZE"
   "PACKED-ENUM-SIZE"
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
   "ENCODE-SFIXED32"
   "ENCODE-SFIXED64"
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
   "DECODE-SFIXED32"
   "DECODE-SFIXED64"
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
   "PROTOBUFS-WARN"
   "MAKE-QUALIFIED-NAME"

   ;; Stuff for ASDF
   "PARSE-PROTOBUF-FILE"
   "PROCESS-IMPORTS"
   "PROCESS-IMPORTS-FROM-FILE"

   ;; Stuff for RPC stubs
   "*RPC-PACKAGE*"
   "*RPC-CALL-FUNCTION*"))
