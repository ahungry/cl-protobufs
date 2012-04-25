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

(defvar *all-messages* (make-hash-table)
  "A table mapping Lisp class names to 'protobuf' messages.")

(defmethod find-message-for-class ((class symbol))
  "Given the name of a class, return the 'protobuf' message and schema for the class."
  (gethash class *all-messages*))

(defmethod find-message-for-class ((class class))
  (gethash (class-name class) *all-messages*))

;; A few things (the pretty printer) want to keep track of the current schema
(defvar *protobuf* nil)
(defvar *protobuf-package* nil)


;;; The model classes

(defclass abstract-protobuf () ())

(defclass base-protobuf (abstract-protobuf)
  ((class :type (or null symbol)                ;the Lisp name for this object
          :accessor proto-class                 ;this often names a type or class
          :initarg :class
          :initform nil)
   (name :type (or null string)                 ;the Protobufs name for this enum, message, etc
         :reader proto-name
         :initarg :name
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
           :initform "proto2")
   (package :type (or null string)              ;the Protobufs package
            :accessor proto-package
            :initarg :package
            :initform nil)
   (lisp-pkg :type (or null string)              ;the Lisp package, from 'option lisp_package = ...'
             :accessor proto-lisp-package
             :initarg :lisp-package
             :initform nil)
   (imports :type (list-of string)              ;any imports
            :accessor proto-imports
            :initarg :imports
            :initform ())
   (optimize :type (member nil :space :speed)
             :accessor proto-optimize
             :initarg :optimize
             :initform nil)
   (enums :type (list-of protobuf-enum)         ;the set of enum types
          :accessor proto-enums
          :initarg :enums
          :initform ())
   (messages :type (list-of protobuf-message)   ;the set of messages
             :accessor proto-messages
             :initarg :messages
             :initform ())
   (extenders :type (list-of protobuf-message)  ;the set of extended messages
              :accessor proto-extenders
              :initarg :extenders
              :initform ())
   (services :type (list-of protobuf-service)
             :accessor proto-services
             :initarg :services
             :initform ()))
  (:documentation
   "The model class that represents a Protobufs schema, i.e., one .proto file."))

(defmethod initialize-instance :after ((protobuf protobuf) &rest initargs)
  (declare (ignore initargs))
  ;; Record this schema under both its Lisp and its Protobufs name
  (with-slots (class name) protobuf
    (when class
      (setf (gethash class *all-protobufs*) protobuf))
    (when name
      (setf (gethash name *all-protobufs*) protobuf))))

(defmethod make-load-form ((p protobuf) &optional environment)
  (make-load-form-saving-slots p :environment environment))

(defmethod print-object ((p protobuf) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "~@[~S~]~@[ (package ~A)~]"
            (proto-class p) (proto-package p))))

(defgeneric find-message (protobuf type)
  (:documentation
   "Given a protobuf schema or message and a type name or class name,
    returns the protobuf message corresponding to the type."))

(defmethod find-message ((protobuf protobuf) (type symbol))
  ;; Extended messages "shadow" non-extended ones
  (or (find type (proto-extenders protobuf) :key #'proto-class)
      (find type (proto-messages protobuf) :key #'proto-class)))

(defmethod find-message ((protobuf protobuf) (type class))
  (find-message protobuf (class-name type)))

(defmethod find-message ((protobuf protobuf) (type string))
  (or (find type (proto-extenders protobuf) :key #'proto-name :test #'string=)
      (find type (proto-messages protobuf) :key #'proto-name :test #'string=)))

(defgeneric find-enum (protobuf type)
  (:documentation
   "Given a protobuf schema or message and the name of an enum type,
    returns the protobuf enum corresponding to the type."))

(defmethod find-enum ((protobuf protobuf) type)
  (find type (proto-enums protobuf) :key #'proto-class))

(defmethod find-enum ((protobuf protobuf) (type string))
  (find type (proto-enums protobuf) :key #'proto-name :test #'string=))


;; We accept and store any option, but only act on a few: default, packed,
;; optimize_for, lisp_package, lisp_name, lisp_alias
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

(defmethod make-load-form ((o protobuf-option) &optional environment)
  (make-load-form-saving-slots o :environment environment))

(defmethod print-object ((o protobuf-option) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~A~@[ = ~S~]" (proto-name o) (proto-value o))))

(defmethod find-option ((protobuf base-protobuf) (name string))
  (let ((option (find name (proto-options protobuf) :key #'proto-name :test #'option-name=)))
    (and option (proto-value option))))

(defmethod find-option ((options list) (name string))
  (let ((option (find name options :key #'proto-name :test #'option-name=)))
    (and option (proto-value option))))

(defun option-name= (name1 name2)
  (let ((start1 (if (eql (char name1 0) #\() 1 0))
        (start2 (if (eql (char name2 0) #\() 1 0))
        (end1   (if (eql (char name1 0) #\() (- (length name1) 1) (length name1)))
        (end2   (if (eql (char name2 0) #\() (- (length name2) 1) (length name2))))
    (string= name1 name2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))


;; A protobuf enumeration
(defclass protobuf-enum (base-protobuf)
  ((alias :type (or null symbol)                ;use this if you want to make this enum
          :accessor proto-alias-for             ;  be an alias for an existing Lisp enum
          :initarg :alias-for
          :initform nil)
   (values :type (list-of protobuf-enum-value)  ;all the values for this enum type
           :accessor proto-values
           :initarg :values
           :initform ()))
  (:documentation
   "The model class that represents a Protobufs enumeration type."))

(defmethod make-load-form ((e protobuf-enum) &optional environment)
  (make-load-form-saving-slots e :environment environment))

(defmethod print-object ((e protobuf-enum) stream)
  (print-unreadable-object (e stream :type t :identity t)
    (format stream "~S~@[ (alias for ~S)~]"
            (proto-class e) (proto-alias-for e))))


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

(defmethod make-load-form ((v protobuf-enum-value) &optional environment)
  (make-load-form-saving-slots v :environment environment))

(defmethod print-object ((v protobuf-enum-value) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream "~A = ~D"
            (proto-name v) (proto-index v))))


;; A protobuf message
(defclass protobuf-message (base-protobuf)
  ((parent :type (or protobuf protobuf-message)
           :accessor proto-parent
           :initarg :parent)
   (conc :type (or null string)                 ;the conc-name used for Lisp accessors
         :accessor proto-conc-name
         :initarg :conc-name
         :initform nil)
   (alias :type (or null symbol)                ;use this if you want to make this message
          :accessor proto-alias-for             ;  be an alias for an existing Lisp class
          :initarg :alias-for
          :initform nil)
   (enums :type (list-of protobuf-enum)         ;the embedded enum types
          :accessor proto-enums
          :initarg :enums
          :initform ())
   (messages :type (list-of protobuf-message)   ;the embedded messages
             :accessor proto-messages
             :initarg :messages
             :initform ())
   (extenders :type (list-of protobuf-message)  ;the set of extended messages
              :accessor proto-extenders
              :initarg :extenders
              :initform ())
   (fields :type (list-of protobuf-field)       ;the fields
           :accessor proto-fields
           :initarg :fields
           :initform ())
   (extensions :type (list-of protobuf-extension) ;any extensions
               :accessor proto-extensions
               :initarg :extensions
               :initform ())
   (extension-p :type (member t nil)            ;true iff this message extends another message
                :accessor proto-extension-p
                :initarg :extension-p
                :initform nil))
    (:documentation
   "The model class that represents a Protobufs message."))

(defmethod initialize-instance :after ((message protobuf-message) &rest initargs)
  (declare (ignore initargs))
  ;; Record this message under just its Lisp class name
  (with-slots (class extension-p) message
    (when (and class (not extension-p))
      (setf (gethash class *all-messages*) message))))

(defmethod make-load-form ((m protobuf-message) &optional environment)
  (make-load-form-saving-slots m :environment environment))

(defmethod print-object ((m protobuf-message) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (format stream "~S~@[ (alias for ~S)~]~@[ (extended~*)~]"
            (proto-class m) (proto-alias-for m) (proto-extension-p m))))

(defmethod find-message ((message protobuf-message) (type symbol))
  ;; Extended messages "shadow" non-extended ones
  (or (find type (proto-extenders message) :key #'proto-class)
      (find type (proto-messages message) :key #'proto-class)
      (find-message (proto-parent message) type)))

(defmethod find-message ((message protobuf-message) (type class))
  (find-message message (class-name type)))

(defmethod find-message ((message protobuf-message) (type string))
  (or (find type (proto-extenders message) :key #'proto-name :test #'string=)
      (find type (proto-messages message) :key #'proto-name :test #'string=)
      (find-message (proto-parent message) type)))

(defmethod find-enum ((message protobuf-message) type)
  (or (find type (proto-enums message) :key #'proto-class)
      (find-enum (proto-parent message) type)))

(defmethod find-enum ((message protobuf-message) (type string))
  (or (find type (proto-enums message) :key #'proto-name :test #'string=)
      (find-enum (proto-parent message) type)))


;; A protobuf field within a message
;;--- Support the 'deprecated' option (have serialization ignore such fields?)
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
   (reader :type (or null symbol)               ;a reader that is used to access the value
           :accessor proto-reader               ;if it's supplied, it's used instead of 'value'
           :initarg :reader
           :initform nil)
   (writer :type (or null symbol list)          ;a writer that is used to set the value
           :accessor proto-writer               ;when it's a list, it's something like '(setf title)'
           :initarg :writer
           :initform nil)
   (default :type (or null string)              ;default value, pulled out of the options
            :accessor proto-default
            :initarg :default
            :initform nil)
   (packed :type (member t nil)                 ;packed, pulled out of the options
           :accessor proto-packed
           :initarg :packed
           :initform nil)
   (extension-p :type (member t nil)            ;true iff this field is an extension
                :accessor proto-extension-p
                :initarg :extension-p
                :initform nil))
  (:documentation
   "The model class that represents one field within a Protobufs message."))

(defmethod initialize-instance :after ((field protobuf-field) &rest initargs)
  (declare (ignore initargs))
  (when (slot-boundp field 'index)
    (assert (not (<= 19000 (proto-index field) 19999)) ()
            "Protobuf field indexes between 19000 and 19999 are not allowed")))

(defmethod make-load-form ((f protobuf-field) &optional environment)
  (make-load-form-saving-slots f :environment environment))

(defmethod print-object ((f protobuf-field) stream)
  (print-unreadable-object (f stream :type t :identity t)
    (format stream "~S :: ~S = ~D~@[ (extended~*)~]"
            (proto-value f) (proto-class f) (proto-index f) (proto-extension-p f))))


;; An extension within a message
(defclass protobuf-extension (abstract-protobuf)
  ((from :type (integer 1 #.(1- (ash 1 29)))    ;the index number for this field
         :accessor proto-extension-from
         :initarg :from)
   (to :type (integer 1 #.(1- (ash 1 29)))      ;the index number for this field
       :accessor proto-extension-to
       :initarg :to))
  (:documentation
   "The model class that represents an extension with a Protobufs message."))

(defmethod make-load-form ((e protobuf-extension) &optional environment)
  (make-load-form-saving-slots e :environment environment))

(defmethod print-object ((e protobuf-extension) stream)
  (print-unreadable-object (e stream :type t :identity t)
    (format stream "~D - ~D"
            (proto-extension-from e) (proto-extension-from e))))


;; A protobuf service
(defclass protobuf-service (base-protobuf)
  ((methods :type (list-of protobuf-method)     ;the methods in the service
            :accessor proto-methods
            :initarg :methods
            :initform ()))
  (:documentation
   "The model class that represents a Protobufs service."))

(defmethod make-load-form ((s protobuf-service) &optional environment)
  (make-load-form-saving-slots s :environment environment))

(defmethod print-object ((s protobuf-service) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "~A"
            (proto-name s))))


;; A protobuf method within a service
(defclass protobuf-method (base-protobuf)
  ((itype :type (or null symbol)                ;the Lisp type name of the input
           :accessor proto-input-type
           :initarg :input-type
           :initform nil)
   (iname :type (or null string)                ;the Protobufs name of the input
          :accessor proto-input-name
          :initarg :input-name
          :initform nil)
   (otype :type (or null symbol)                ;the Lisp type name of the output
           :accessor proto-output-type
           :initarg :output-type
           :initform nil)
   (oname :type (or null string)                ;the Protobufs name of the output
          :accessor proto-output-name
          :initarg :output-name
          :initform nil))
  (:documentation
   "The model class that represents one method with a Protobufs service."))

(defmethod make-load-form ((m protobuf-method) &optional environment)
  (make-load-form-saving-slots m :environment environment))

(defmethod print-object ((m protobuf-method) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (format stream "~S (~S) => (~S)"
            (proto-function m) (proto-input-type m) (proto-output-type m))))

;; The 'class' slot really holds the name of the function,
;; so let's give it a better name
(defmethod proto-function ((method protobuf-method))
  (proto-class method))

(defmethod (setf proto-function) (function (method protobuf-method))
  (setf (proto-function method) function))


;; Better type checking for these guys
#+quux (progn

(quux:declare-list-of protobuf-option)
(quux:declare-list-of protobuf-enum)
(quux:declare-list-of protobuf-enum-value)
(quux:declare-list-of protobuf-message)
(quux:declare-list-of protobuf-extension)
(quux:declare-list-of protobuf-field)
(quux:declare-list-of protobuf-service)
(quux:declare-list-of protobuf-method)

)       ;#+quux
