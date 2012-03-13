;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-IMPL")


;;; Protol buffers model classes

(defvar *all-protobufs* (make-hash-table :test #'equal)
  "A table mapping names to 'protobuf' schemas.")

(defun find-protobuf (name)
  "Given a name (a string or a symbol), return the 'protobuf' schema having that name."
  (gethash name *all-protobufs*))

;; A few things (the pretty printer) want to keep track of the current schema
(defvar *protobuf* nil)
(defvar *protobuf-package* nil)


;;; The model classes

(defclass abstract-protobuf () ())

(defclass base-protobuf (abstract-protobuf)
  ((name :type (or null string)                 ;the name of this .proto file/enum/message, etc
         :reader proto-name
         :initarg :name
         :initform nil)
   (class :type (or null symbol)                ;a Lisp "class name" for this object
          :accessor proto-class
          :initarg :class
          :initform nil)
   (options :type (list-of protobuf-option)     ;options, mostly just passed along
            :accessor proto-options
            :initarg :options
            :initform ())
   (doc :type (or null string)                  ;documentation for this object
        :accessor proto-documentation
        :initarg :documentation
        :initform nil))
  (:documentation
   "The base class for all Protobufs model classes."))


;; The protobuf, corresponds to one .proto file
(defclass protobuf (base-protobuf)
  ((syntax :type (or null string)               ;syntax, passed on but otherwise ignored
           :accessor proto-syntax
           :initarg :syntax
           :initform nil)
   (package :type (or null string)              ;the package
            :accessor proto-package
            :initarg :package
            :initform nil)
   ;;---*** Support imports properly
   (imports :type (list-of string)              ;any imports
            :accessor proto-imports
            :initarg :imports
            :initform ())
   (enums :type (list-of protobuf-enum)         ;the set of enum types
          :accessor proto-enums
          :initarg :enums
          :initform ())
   (messages :type (list-of protobuf-message)   ;the set of messages
             :accessor proto-messages
             :initarg :messages
             :initform ())
   (services :type (list-of protobuf-service)
             :accessor proto-services
             :initarg :services
             :initform ()))
  (:documentation
   "The model class that represents a Protobufs schema, i.e., one .proto file."))

(defmethod print-object ((p protobuf) stream)
  (print-unprintable-object (p stream :type t :identity t)
    (format stream "~@[~A~]~@[ (package ~A)~]"
            (proto-name p) (proto-package p))))

(defgeneric find-message-for-class (protobuf class)
  (:documentation
   "Given a protobuf schema or message and a class or class name,
    returns the protobuf message corresponding to the class."))

(defgeneric find-enum-for-type (protobuf type)
  (:documentation
   "Given a protobuf schema or message and the name of an enum type,
    returns the protobuf enum corresponding to the type."))

(defmethod find-message-for-class ((protobuf protobuf) (class symbol))
  (or (find class (proto-messages protobuf) :key #'proto-class)
      (some #'(lambda (msg) (find-message-for-class msg class)) (proto-messages protobuf))))

(defmethod find-message-for-class ((protobuf protobuf) (class class))
  (find-message-for-class protobuf (class-name class)))

(defmethod find-message-for-class ((protobuf protobuf) (class string))
  (or (find class (proto-messages protobuf) :key #'proto-name :test #'string=)
      (some #'(lambda (msg) (find-message-for-class msg class)) (proto-messages protobuf))))

(defmethod find-enum-for-type ((protobuf protobuf) type)
  (or (find type (proto-enums protobuf) :key #'proto-class)
      (some #'(lambda (msg) (find-enum-for-type msg type)) (proto-messages protobuf))))

(defmethod find-enum-for-type ((protobuf protobuf) (type string))
  (or (find type (proto-enums protobuf) :key #'proto-name :test #'string=)
      (some #'(lambda (msg) (find-enum-for-type msg type)) (proto-messages protobuf))))


;;--- For now, we support only the built-in options.
;;--- We will want to extend this to customizable options as well.
(defclass protobuf-option (abstract-protobuf)
  ((name :type string                           ;the key
         :reader proto-name
         :initarg :name)
   (value :type (or null string)                ;the value
          :accessor proto-value
          :initarg :value
          :initform nil))
  (:documentation
   "The model class that represents a Protobufs options, i.e., a keyword/value pair."))

(defmethod print-object ((o protobuf-option) stream)
  (print-unprintable-object (o stream :type t :identity t)
    (format stream "~A~@[ = ~S~]" (proto-name o) (proto-value o))))

(defun cl-user::protobuf-option (stream option colon-p atsign-p)
  (cond (colon-p                                ;~:/protobuf-option/ -- .proto format
         (format stream "~A~@[ = ~S~]" (proto-name option) (proto-value option)))
        (atsign-p                               ;~@/protobuf-option/ -- .lisp format
         (format stream "~S ~S" (proto-name option) (proto-value option)))
        (t                                      ;~/protobuf-option/  -- keyword/value format
         (format stream "~(:~A~) ~S" (proto-name option) (proto-value option)))))


;; A protobuf enumeration
(defclass protobuf-enum (base-protobuf)
  ((values :type (list-of protobuf-enum-value)  ;all the values for this enum type
           :accessor proto-values
           :initarg :values
           :initform ()))
  (:documentation
   "The model class that represents a Protobufs enumeration type."))

(defmethod print-object ((e protobuf-enum) stream)
  (print-unprintable-object (e stream :type t :identity t)
    (format stream "~A~@[ (~S)~]"
            (proto-name e) (proto-class e))))


;; A protobuf value within an enumeration
(defclass protobuf-enum-value (base-protobuf)
  ((index :type (integer #.(- (ash 1 31)) #.(1- (ash 1 31)))
          :accessor proto-index                 ;the index of the enum value
          :initarg :index)
   (value :type (or null symbol)
          :accessor proto-value                 ;the Lisp value of the enum
          :initarg :value
          :initform nil))
  (:documentation
   "The model class that represents a Protobufs enumeration value."))

(defmethod print-object ((v protobuf-enum-value) stream)
  (print-unprintable-object (v stream :type t :identity t)
    (format stream "~A = ~D~@[ (~S)~]"
            (proto-name v) (proto-index v) (proto-value v))))


;; A protobuf message
(defclass protobuf-message (base-protobuf)
  ((conc :type (or null string)                 ;the conc-name used for Lisp accessors
         :accessor proto-conc-name
         :initarg :conc-name
         :initform nil)
   (enums :type (list-of protobuf-enum)         ;the embedded enum types
          :accessor proto-enums
          :initarg :enums
          :initform ())
   (messages :type (list-of protobuf-message)   ;the embedded messages
             :accessor proto-messages
             :initarg :messages
             :initform ())
   (fields :type (list-of protobuf-field)       ;the fields
           :accessor proto-fields
           :initarg :fields
           :initform ())
   (extensions :type (list-of protobuf-extension) ;any extensions
               :accessor proto-extensions
               :initarg :extensions
               :initform ()))
    (:documentation
   "The model class that represents a Protobufs message."))

(defmethod print-object ((m protobuf-message) stream)
  (print-unprintable-object (m stream :type t :identity t)
    (format stream "~A~@[ (~S)~]"
            (proto-name m) (proto-class m))))

(defmethod find-message-for-class ((message protobuf-message) (class symbol))
  (find class (proto-messages message) :key #'proto-class))

(defmethod find-message-for-class ((message protobuf-message) (class class))
  (find-message-for-class message (class-name class)))

(defmethod find-message-for-class ((message protobuf-message) (class string))
  (find class (proto-messages message) :key #'proto-name :test #'string=))

(defmethod find-enum-for-type ((message protobuf-message) type)
  (find type (proto-enums message) :key #'proto-class))

(defmethod find-enum-for-type ((message protobuf-message) (type string))
  (find type (proto-enums message) :key #'proto-name :test #'string=))


;; A protobuf field within a message
(defclass protobuf-field (base-protobuf)
  ((type :type string                           ;the name of the Protobuf type for the field
         :accessor proto-type
         :initarg :type)
   (required :type (member :required :optional :repeated)
             :accessor proto-required
             :initarg :required)
   (index :type (integer 1 #.(1- (ash 1 29)))   ;the index number for this field
          :accessor proto-index
          :initarg :index)
   (value :type (or null symbol)                ;the Lisp slot holding the value within an object
          :accessor proto-value
          :initarg :value
          :initform nil)
   (default :type (or null string)              ;default value, pulled out of the options
            :accessor proto-default
            :initarg :default
            :initform nil)
   (packed :type (member t nil)                 ;packed, pulled out of the options
           :accessor proto-packed
           :initarg :packed
           :initform nil))
  (:documentation
   "The model class that represents one field within a Protobufs message."))

(defmethod initialize-instance :after ((field protobuf-field) &rest initargs)
  (declare (ignore initargs))
  (assert (not (<= 19000 (proto-index field) 19999)) ()
          "Protobuf field indexes between 19000 and 19999 are not allowed"))

(defmethod print-object ((f protobuf-field) stream)
  (print-unprintable-object (f stream :type t :identity t)
    (format stream "~A ~A~:[~*~*~; (~S~@[ :: ~S~])~] = ~D"
            (proto-type f) (proto-name f)
            (or (proto-value f) (proto-class f)) (proto-value f) (proto-class f)
            (proto-index f))))


;; An extension within a message
;;--- We still need to support 'extend', which depends on supporting 'import'
(defclass protobuf-extension (abstract-protobuf)
  ((from :type (integer 1 #.(1- (ash 1 29)))    ;the index number for this field
         :accessor proto-extension-from
         :initarg :from)
   (to :type (integer 1 #.(1- (ash 1 29)))      ;the index number for this field
       :accessor proto-extension-to
       :initarg :to))
  (:documentation
   "The model class that represents an extension with a Protobufs message."))

(defmethod print-object ((e protobuf-extension) stream)
  (print-unprintable-object (e stream :type t :identity t)
    (format stream "~D - ~D"
            (proto-extension-from e) (proto-extension-from e))))


;; A protobuf service
(defclass protobuf-service (base-protobuf)
  ((rpcs :type (list-of protobuf-rpc)           ;the RPCs in the service
         :accessor proto-rpcs
         :initarg :rpcs
         :initform ()))
  (:documentation
   "The model class that represents a Protobufs service."))

(defmethod print-object ((s protobuf-service) stream)
  (print-unprintable-object (s stream :type t :identity t)
    (format stream "~A"
            (proto-name s))))


;; A protobuf RPC within a service
(defclass protobuf-rpc (base-protobuf)
  ((itype :type (or null string)                ;the name of the input message type
          :accessor proto-input-type
          :initarg :input-type)
   (iclass :type (or null symbol)               ;the name of the input message type
           :accessor proto-input-class
           :initarg :input-class)
   (otype :type (or null string)                ;the name of the output message type
          :accessor proto-output-type
          :initarg :output-type)
   (oclass :type (or null symvol)               ;the name of the output message type
           :accessor proto-output-class
           :initarg :output-class))
  (:documentation
   "The model class that represents one RPC with a Protobufs service."))

(defmethod print-object ((r protobuf-rpc) stream)
  (print-unprintable-object (r stream :type t :identity t)
    (format stream "~A (~@[~A~]) => (~@[~A~])"
            (proto-name r) (proto-input-type r) (proto-output-type r))))
