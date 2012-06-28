;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-IMPL")


;;; Protol buffers model classes

(defvar *all-schemas* (make-hash-table :test #'equal)
  "A table mapping names to 'protobuf-schema' objects.")

(defgeneric find-schema (name)
  (:documentation
   "Given a name (a symbol or string), return the 'protobuf-schema' object having that name."))

(defmethod find-schema ((name symbol))
  (values (gethash (keywordify name) *all-schemas*)))

(defmethod find-schema ((name string))
  (values (gethash (string-upcase name) *all-schemas*)))

(defmethod find-schema ((path pathname))
  "Given a pathname, return the 'protobuf-schema' object that came from that path."
  (values (gethash (make-pathname :type nil :defaults path) *all-schemas*)))


(defvar *all-messages* (make-hash-table :test #'equal)
  "A table mapping Lisp class names to 'protobuf-message' objects.")

(defgeneric find-message-for-class (class)
  (:documentation
   "Given a class or class name, return the message that globally has that name."))

(defmethod find-message-for-class (class)
  "Given the name of a class (a symbol or string), return the 'protobuf-message' for the class."
  (values (gethash class *all-messages*)))

(defmethod find-message-for-class ((class class))
  (values (gethash (class-name class) *all-messages*)))


;; A few things (the pretty printer) want to keep track of the current schema
(defvar *protobuf* nil
  "The Protobufs object currently being defined, either a schema or a message.")

(defvar *protobuf-package* nil
  "The Lisp package in which the Protobufs schema is being defined.")

(defvar *protobuf-conc-name* nil
  "A global conc-name to use for all the messages in this schema. This controls
   the name of the accessors the fields of each message.
   When it's nil, there is no global conc-name.
   When it's t, each message will use the message name as the conc-name.
   When it's a string, that string will be used as the conc-name for each message.
   'parse-schema-from-file' defaults conc-name to \"\", meaning that each field in
   every message has an accessor whose name is the name of the field.")

(defvar *protobuf-pathname* ()
  "The name of the file from where the .proto file is being parsed.")

(defvar *protobuf-search-path* ()
  "A search-path to use to resolve any relative pathnames.")

(defvar *protobuf-output-path* ()
  "A path to use to direct output during imports, etc.")


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
   (qual-name :type string                      ;the fully qualified name, e.g., "proto2.MessageSet"
              :accessor proto-qualified-name
              :initarg :qualified-name
              :initform "")
   (parent :type (or null base-protobuf)        ;this object's parent
           :accessor proto-parent
           :initarg :parent)
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

(defun find-qualified-name (name protos
                            &key (proto-key #'proto-name) (full-key #'proto-qualified-name)
                                 (lisp-key #'proto-class)
                                 relative-to)
  "Find something by its string name, first doing a simple name match,
   and, if that fails, exhaustively searching qualified names."
  (declare (ignore relative-to))
  (or (find name protos :key proto-key :test #'string=)
      ;;--- This needs more sophisticated search, e.g., relative to current namespace
      (find name protos :key full-key  :test #'string=)
      ;; Maybe we can find the symbol in Lisp land?
      (multiple-value-bind (name package path other)
          (proto->class-name name)
        (declare (ignore path))
        (let* ((name   (string name))
               (symbol (or (and package (find-symbol name package))
                           (and other
                                (find-proto-package other)
                                (find-symbol name (find-proto-package other))))))
          (when symbol
            (find symbol protos :key lisp-key))))))


;; A Protobufs schema, corresponds to one .proto file
(defclass protobuf-schema (base-protobuf)
  ((syntax :type (or null string)               ;syntax, passed on but otherwise ignored
           :accessor proto-syntax
           :initarg :syntax
           :initform "proto2")
   (package :type (or null string)              ;the Protobufs package
            :accessor proto-package
            :initarg :package
            :initform nil)
   (lisp-pkg :type (or null string)             ;the Lisp package, from 'option lisp_package = ...'
             :accessor proto-lisp-package
             :initarg :lisp-package
             :initform nil)
   (imports :type (list-of string)              ;the names of schemas to be imported
            :accessor proto-imports
            :initarg :imports
            :initform ())
   (schemas :type (list-of protobuf-schema)     ;the schemas that were successfully imported
            :accessor proto-imported-schemas    ;this gets used for chasing namespaces
            :initform ())
   (enums :type (list-of protobuf-enum)         ;the set of enum types
          :accessor proto-enums
          :initarg :enums
          :initform ())
   (messages :type (list-of protobuf-message)   ;all the messages within this protobuf
             :accessor proto-messages
             :initarg :messages
             :initform ())
   (extenders :type (list-of protobuf-message)  ;the 'extend' messages in this protobuf
              :accessor proto-extenders         ;these precede unextended messages in 'find-message'
              :initarg :extenders
              :initform ())
   (services :type (list-of protobuf-service)
             :accessor proto-services
             :initarg :services
             :initform ()))
  (:documentation
   "The model class that represents a Protobufs schema, i.e., one .proto file."))

(defmethod make-load-form ((s protobuf-schema) &optional environment)
  (with-slots (class name) s
    (multiple-value-bind (constructor initializer)
        (make-load-form-saving-slots s :environment environment)
      (values `(let ((s ,constructor))
                  (record-protobuf s ',class ',name nil)
                  s)
              initializer))))

(defgeneric record-protobuf (schema &optional symbol name type)
  (:documentation
   "Record all the names by which the Protobufs schema might be known.")
  (:method ((schema protobuf-schema) &optional symbol name type)
    (declare (ignore type))
    (let ((symbol (or symbol (proto-class schema)))
          (name   (or name (proto-name schema))))
      (when symbol
        (setf (gethash (keywordify symbol) *all-schemas*) schema))
      (when name
        (setf (gethash (string-upcase name) *all-schemas*) schema))
      (let ((path (or *protobuf-pathname* *compile-file-pathname*)))
        (when path
          ;; Record the file from which the Protobufs schema came, sans file type
          (setf (gethash (make-pathname :type nil :defaults path) *all-schemas*) schema))))))

(defmethod print-object ((s protobuf-schema) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "~@[~S~]~@[ (package ~A)~]"
            (proto-class s) (proto-package s))))

(defgeneric make-qualified-name (proto name)
  (:documentation
   "Give a schema or message and a name,
    generate a fully qualified name string for the name."))

(defmethod make-qualified-name ((schema protobuf-schema) name)
  ;; If we're at the schema, the qualified name is the schema's
  ;; package "dot" the name
  (if (proto-package schema)
    (strcat (proto-package schema) "." name)
    name))

(defgeneric find-enum (protobuf type &optional relative-to)
  (:documentation
   "Given a Protobufs schema or message and the name of an enum type,
    returns the Protobufs enum corresponding to the type."))

(defmethod find-enum ((schema protobuf-schema) (type symbol) &optional relative-to)
  (declare (ignore relative-to))
  (labels ((find-it (schema)
             (let ((enum (find type (proto-enums schema) :key #'proto-class)))
               (when enum
                 (return-from find-enum enum))
               (map () #'find-it (proto-imported-schemas schema)))))
    (find-it schema)))

(defmethod find-enum ((schema protobuf-schema) (name string) &optional relative-to)
  (let ((relative-to (or relative-to schema)))
    (labels ((find-it (schema)
               (let ((enum (find-qualified-name name (proto-enums schema)
                                                :relative-to relative-to)))
                 (when enum
                   (return-from find-enum enum))
                 (map () #'find-it (proto-imported-schemas schema)))))
      (find-it schema))))

(defgeneric find-message (protobuf type &optional relative-to)
  (:documentation
   "Given a Protobufs schema or message and a type name or class name,
    returns the Protobufs message corresponding to the type."))

(defmethod find-message ((schema protobuf-schema) (type symbol) &optional relative-to)
  (declare (ignore relative-to))
  ;; Extended messages "shadow" non-extended ones
  (labels ((find-it (schema)
             (let ((message (or (find type (proto-extenders schema) :key #'proto-class)
                                (find type (proto-messages  schema) :key #'proto-class))))
               (when message
                 (return-from find-message message))
               (map () #'find-it (proto-imported-schemas schema)))))
    (find-it schema)))

(defmethod find-message ((schema protobuf-schema) (type class) &optional relative-to)
  (find-message schema (class-name type) (or relative-to schema)))

(defmethod find-message ((schema protobuf-schema) (name string) &optional relative-to)
  (let ((relative-to (or relative-to schema)))
    (labels ((find-it (schema)
               (let ((message (or (find-qualified-name name (proto-extenders schema)
                                                       :relative-to relative-to)
                                  (find-qualified-name name (proto-messages  schema)
                                                       :relative-to relative-to))))
                 (when message
                   (return-from find-message message))
                 (map () #'find-it (proto-imported-schemas schema)))))
      (find-it schema))))

(defgeneric find-service (protobuf name)
  (:documentation
   "Given a Protobufs schema,returns the Protobufs service of the given name."))

(defmethod find-service ((schema protobuf-schema) (name symbol))
  (find name (proto-services schema) :key #'proto-class))

(defmethod find-service ((schema protobuf-schema) (name string))
  (find-qualified-name name (proto-services schema)))

;; Convenience function that accepts a schema name
(defmethod find-service (schema-name name)
  (let ((schema (find-schema schema-name)))
    (assert schema ()
            "There is no schema named ~A" schema-name)
    (find-service schema name)))


;; We accept and store any option, but only act on a few: default, packed,
;; optimize_for, lisp_package, lisp_name, lisp_alias
(defclass protobuf-option (abstract-protobuf)
  ((name :type string                           ;the key
         :reader proto-name
         :initarg :name)
   (value :accessor proto-value                 ;the (untyped) value
          :initarg :value
          :initform nil)
   (type :type (or null symbol)                 ;(optional) Lisp type,
         :reader proto-type                     ;  one of string, integer, sybol (for now)
         :initarg :type
         :initform 'string))
  (:documentation
   "The model class that represents a Protobufs options, i.e., a keyword/value pair."))

(defmethod make-load-form ((o protobuf-option) &optional environment)
  (make-load-form-saving-slots o :environment environment))

(defmethod print-object ((o protobuf-option) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~A~@[ = ~S~]" (proto-name o) (proto-value o))))

(defgeneric find-option (protobuf name)
  (:documentation
   "Given a Protobufs schema, message, enum, etc and the name of an option,
    returns the value of the option and its (Lisp) type. The third value is
    true if an option was found, otherwise it is false."))

(defmethod find-option ((protobuf base-protobuf) (name string))
  (let ((option (find name (proto-options protobuf) :key #'proto-name :test #'option-name=)))
    (if option
      (values (proto-value option) (proto-type option) t)
      (values nil nil nil))))

(defmethod find-option ((options list) (name string))
  (let ((option (find name options :key #'proto-name :test #'option-name=)))
    (if option
      (values (proto-value option) (proto-type option) t)
      (values nil nil nil))))

(defgeneric remove-options (protobuf &rest names)
  (:documentation
   "Given a Protobufs schema, message, enum, etc and a set of option names,
    remove all of those options from the set of options."))

(defmethod remove-options ((protobuf base-protobuf) &rest names)
  (dolist (name names (proto-options protobuf))
    (let ((option (find name (proto-options protobuf) :key #'proto-name :test #'option-name=)))
      (when option
        ;; This side-effects 'proto-options'
        (setf (proto-options protobuf) (remove option (proto-options protobuf)))))))

(defmethod remove-options ((options list) &rest names)
  (dolist (name names options)
    (let ((option (find name options :key #'proto-name :test #'option-name=)))
      (when option
        ;; This does not side-effect the list of options
        (setq options (remove option options))))))

(defun option-name= (name1 name2)
  (let* ((name1  (string name1))
         (name2  (string name2))
         (start1 (if (eql (char name1 0) #\() 1 0))
         (start2 (if (eql (char name2 0) #\() 1 0))
         (end1   (if (eql (char name1 0) #\() (- (length name1) 1) (length name1)))
         (end2   (if (eql (char name2 0) #\() (- (length name2) 1) (length name2))))
    (string= name1 name2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))


;; A Protobufs enumeration
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


;; A Protobufs value within an enumeration
(defclass protobuf-enum-value (base-protobuf)
  ((index :type (signed-byte 32)                ;the numeric value of the enum
          :accessor proto-index
          :initarg :index)
   (value :type (or null symbol)                ;the Lisp value of the enum
          :accessor proto-value
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


;; A Protobufs message
(defclass protobuf-message (base-protobuf)
  ((conc :type (or null string)                 ;the conc-name used for Lisp accessors
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
   (messages :type (list-of protobuf-message)   ;all the messages embedded in this one
             :accessor proto-messages
             :initarg :messages
             :initform ())
   (extenders :type (list-of protobuf-message)  ;the 'extend' messages embedded in this one
              :accessor proto-extenders         ;these precede unextended messages in 'find-message'
              :initarg :extenders
              :initform ())
   (fields :type (list-of protobuf-field)       ;all the fields of this message
           :accessor proto-fields               ;this includes local ones and extended ones
           :initarg :fields
           :initform ())
   (extended-fields :type (list-of protobuf-field) ;the extended fields defined in this message
                    :accessor proto-extended-fields
                    :initform ())
   (extensions :type (list-of protobuf-extension) ;any extension ranges
               :accessor proto-extensions
               :initarg :extensions
               :initform ())
   ;; :message is an ordinary message
   ;; :group is a (deprecated) group (kind of an "implicit" message)
   ;; :extends is an 'extends' to an existing message
   (message-type :type (member :message :group :extends)
                 :accessor proto-message-type
                 :initarg :message-type
                 :initform :message))
  (:documentation
   "The model class that represents a Protobufs message."))

(defmethod make-load-form ((m protobuf-message) &optional environment)
  (with-slots (class name message-type) m
    (multiple-value-bind (constructor initializer)
        (make-load-form-saving-slots m :environment environment)
      (values (if (eq message-type :extends)
                constructor
                `(let ((m ,constructor))
                   (record-protobuf m ',class ',name ',message-type)
                   m))
              initializer))))

(defmethod record-protobuf ((message protobuf-message) &optional class name type)
  ;; No need to record an extension, it's already been recorded
  (let ((class (or class (proto-class message)))
        (name  (or name (proto-name message)))
        (type  (or type (proto-message-type message))))
    (unless (eq type :extends)
      (when class
        (setf (gethash class *all-messages*) message))
      (when name
        (setf (gethash name *all-messages*) message)))))

(defmethod print-object ((m protobuf-message) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (format stream "~S~@[ (alias for ~S)~]~@[ (group~*)~]~@[ (extended~*)~]"
            (proto-class m) (proto-alias-for m)
            (eq (proto-message-type m) :group)
            (eq (proto-message-type m) :extends))))

(defmethod proto-package ((message protobuf-message))
  (and (proto-parent message)
       (proto-package (proto-parent message))))

(defmethod proto-lisp-package ((message protobuf-message))
  (and (proto-parent message)
       (proto-lisp-package (proto-parent message))))

(defmethod make-qualified-name ((message protobuf-message) name)
  ;; The qualified name is the message name "dot" the name
  (let ((qual-name (strcat (proto-name message) "." name)))
    (if (proto-parent message)
      ;; If there's a parent for this message (either a message or
      ;; the schema), prepend the name (or package) of the parent
      (make-qualified-name (proto-parent message) qual-name)
      ;; Guard against a message in the middle of nowhere
      qual-name)))

(defmethod find-message ((message protobuf-message) (type symbol) &optional relative-to)
  ;; Extended messages "shadow" non-extended ones
  (or (find type (proto-extenders message) :key #'proto-class)
      (find type (proto-messages message) :key #'proto-class)
      (find-message (proto-parent message) type (or relative-to message))))

(defmethod find-message ((message protobuf-message) (type class) &optional relative-to)
  (find-message message (class-name type) (or relative-to message)))

(defmethod find-message ((message protobuf-message) (name string) &optional relative-to)
  (let ((relative-to (or relative-to message)))
    (or (find-qualified-name name (proto-extenders message)
                             :relative-to relative-to)
        (find-qualified-name name (proto-messages message)
                             :relative-to relative-to)
        (find-message (proto-parent message) name relative-to))))

(defmethod find-enum ((message protobuf-message) type &optional relative-to)
  (or (find type (proto-enums message) :key #'proto-class)
      (find-enum (proto-parent message) type (or relative-to message))))

(defmethod find-enum ((message protobuf-message) (name string) &optional relative-to)
  (let ((relative-to (or relative-to message)))
    (or (find-qualified-name name (proto-enums message)
                             :relative-to relative-to)
        (find-enum (proto-parent message) name relative-to))))

(defgeneric find-field (message name &optional relative-to)
  (:documentation
   "Given a Protobufs message and a slot name, field name or index,
    returns the Protobufs field having that name."))

(defmethod find-field ((message protobuf-message) (name symbol) &optional relative-to)
  (declare (ignore relative-to))
  (find name (proto-fields message) :key #'proto-value))

(defmethod find-field ((message protobuf-message) (name string) &optional relative-to)
  (find-qualified-name name (proto-fields message) :lisp-key #'proto-value
                       :relative-to (or relative-to message)))

(defmethod find-field ((message protobuf-message) (index integer) &optional relative-to)
  (declare (ignore relative-to))
  (find index (proto-fields message) :key #'proto-index))


;; Extensions protocol
(defgeneric get-extension (object slot)
  (:documentation
   "Returns the value of the extended slot 'slot' in 'object'"))

(defgeneric set-extension (object slot value)
  (:documentation
   "Sets the value of the extended slot 'slot' to 'value' in 'object'"))

(defgeneric has-extension (object slot)
  (:documentation
   "Returns true iff the there is an extended slot named 'slot' in 'object'")
  ;; The only default method is for 'has-extension'
  ;; It's an error to call the other three functions on a non-extendable object
  (:method ((object standard-object) slot)
    (declare (ignore slot))
    nil))

(defgeneric clear-extension (object slot)
  (:documentation
   "Clears the value of the extended slot 'slot' from 'object'"))


(defconstant $empty-default 'empty-default
  "The marker used in 'proto-default' used to indicate that there is no default value.")
(defconstant $empty-list    'empty-list)
(defconstant $empty-vector  'empty-vector)

;; A Protobufs field within a message
;;--- Support the 'deprecated' option (have serialization ignore such fields?)
(defclass protobuf-field (base-protobuf)
  ((type :type string                           ;the name of the Protobuf type for the field
         :accessor proto-type
         :initarg :type)
   (required :type (member :required :optional :repeated)
             :accessor proto-required
             :initarg :required)
   (index :type (unsigned-byte 29)              ;the index number for this field
          :accessor proto-index                 ; which must be strictly positive
          :initarg :index)
   (value :type (or null symbol)                ;the Lisp slot holding the value within an object
          :accessor proto-value                 ;this also serves as the Lisp field name
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
   (default :accessor proto-default             ;default value (untyped), pulled out of the options
            :initarg :default
            :initform $empty-default)
   (packed :type (member t nil)                 ;packed, pulled out of the options
           :accessor proto-packed
           :initarg :packed
           :initform nil)
   ;; Copied from 'proto-message-type' of the field
   (message-type :type (member :message :group :extends)
                 :accessor proto-message-type
                 :initarg :message-type
                 :initform :message))
  (:documentation
   "The model class that represents one field within a Protobufs message."))

(defmethod initialize-instance :after ((field protobuf-field) &rest initargs)
  (declare (ignore initargs))
  (when (slot-boundp field 'index)
    (assert (and (plusp (proto-index field))
                 (not (<= 19000 (proto-index field) 19999))) ()
            "Protobuf field indexes must be positive and not between 19000 and 19999 (inclusive)")))

(defmethod make-load-form ((f protobuf-field) &optional environment)
  (make-load-form-saving-slots f :environment environment))

(defmethod print-object ((f protobuf-field) stream)
  (print-unreadable-object (f stream :type t :identity t)
    (format stream "~S :: ~S = ~D~@[ (group~*)~]~@[ (extended~*)~]"
            (proto-value f) (proto-class f) (proto-index f)
            (eq (proto-message-type f) :group)
            (eq (proto-message-type f) :extends))))

;; The 'value' slot really holds the name of the slot,
;; so let's give it a better name
(defmethod proto-slot ((field protobuf-field))
  (proto-value field))

(defmethod (setf proto-slot) (slot (field protobuf-field))
  (setf (proto-value field) slot))

(defgeneric empty-default-p (field)
  (:documentation
   "Returns true iff the default for the field is empty, ie, was not supplied.")
  (:method ((field protobuf-field))
    (let ((default (proto-default field)))
      (or (eq default $empty-default)
          (eq default $empty-list)
          (eq default $empty-vector)
          ;; Special handling for imported CLOS classes
          (and (not (eq (proto-required field) :optional))
               (or (null default) (equalp default #())))))))

(defgeneric vector-field-p (field)
  (:documentation
   "Returns true if the storage for a 'repeated' field is a vector,
    returns false if the storage is a list.")
  (:method ((field protobuf-field))
    (let ((default (proto-default field)))
      (or (eq default $empty-vector)
          (and (vectorp default) (not (stringp default)))))))


;; An extension range within a message
(defclass protobuf-extension (abstract-protobuf)
  ((from :type (integer 1 #.(1- (ash 1 29)))    ;the index number for this field
         :accessor proto-extension-from
         :initarg :from)
   (to :type (integer 1 #.(1- (ash 1 29)))      ;the index number for this field
       :accessor proto-extension-to
       :initarg :to))
  (:documentation
   "The model class that represents an extension range within a Protobufs message."))

(defmethod make-load-form ((e protobuf-extension) &optional environment)
  (make-load-form-saving-slots e :environment environment))

(defmethod print-object ((e protobuf-extension) stream)
  (print-unreadable-object (e stream :type t :identity t)
    (format stream "~D - ~D"
            (proto-extension-from e) (proto-extension-from e))))


;; A Protobufs service
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

(defgeneric find-method (service name)
  (:documentation
   "Given a Protobufs service and a method name,
    returns the Protobufs method having that name."))

(defmethod find-method ((service protobuf-service) (name symbol))
  (find name (proto-methods service) :key #'proto-class))

(defmethod find-method ((service protobuf-service) (name string))
  (find-qualified-name name (proto-methods service)))

(defmethod find-method ((service protobuf-service) (index integer))
  (find index (proto-methods service) :key #'proto-index))


;; A Protobufs method within a service
(defclass protobuf-method (base-protobuf)
  ((client-fn :type symbol                      ;the Lisp name of the client stb
              :accessor proto-client-stub
              :initarg :client-stub)
   (server-fn :type symbol                      ;the Lisp name of the server stb
              :accessor proto-server-stub
              :initarg :server-stub)
   (itype :type symbol                          ;the Lisp type name of the input
          :accessor proto-input-type
          :initarg :input-type)
   (iname :type (or null string)                ;the Protobufs name of the input
          :accessor proto-input-name
          :initarg :input-name
          :initform nil)
   (otype :type symbol                          ;the Lisp type name of the output
          :accessor proto-output-type
          :initarg :output-type)
   (oname :type (or null string)                ;the Protobufs name of the output
          :accessor proto-output-name
          :initarg :output-name
          :initform nil)
   (index :type (unsigned-byte 32)              ;an identifying index for this method
          :accessor proto-index                 ; (used by Stubby)
          :initarg :index))
  (:documentation
   "The model class that represents one method with a Protobufs service."))

(defmethod make-load-form ((m protobuf-method) &optional environment)
  (make-load-form-saving-slots m :environment environment))

(defmethod print-object ((m protobuf-method) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (format stream "~S (~S) => (~S)"
            (proto-class m) (proto-input-type m) (proto-output-type m))))
