;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012-2013 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-IMPL")


;;; Protocol buffer defining macros

;;; Base class for all Protobufs-defined classes

(defclass base-protobuf-message ()
  ;; Just one slot, to hold a size cached by 'object-size'
  ((%cached-size :type (or null fixnum)
                 :initform nil))
  (:documentation
   "The base class for all user-defined Protobufs messages."))


;;; The macros

;; Define a schema named 'type', corresponding to a .proto file of that name
(defmacro define-schema (type (&key name syntax package lisp-package import optimize
                                    options documentation)
                         &body messages &environment env)
  "Define a schema named 'type', corresponding to a .proto file of that name.
   'name' can be used to override the defaultly generated Protobufs name.
   'syntax' and 'package' are as they would be in a .proto file.
   'lisp-package' can be used to specify a Lisp package if it is different from
   the Protobufs package given by 'package'.
   'import' is a list of pathname strings to be imported.
   'optimize' can be either :space (the default) or :speed; if it is :speed, the
   serialization code will be much faster, but much less compact.
   'options' is a property list, i.e., (\"key1\" \"val1\" \"key2\" \"val2\" ...).

   The body consists of 'define-enum', 'define-message' or 'define-service' forms."
  (let* ((name     (or name (class-name->proto type)))
         (package  (and package (if (stringp package) package (string-downcase (string package)))))
         (lisp-pkg (and lisp-package (if (stringp lisp-package) lisp-package (string lisp-package))))
         (options  (remove-options
                     (loop for (key val) on options by #'cddr
                           collect (make-option (if (symbolp key) (slot-name->proto key) key) val))
                     "optimize_for" "lisp_package"))
         (imports  (if (listp import) import (list import)))
         (schema   (make-instance 'protobuf-schema
                     :class    type
                     :name     name
                     :syntax   (or syntax "proto2")
                     :package  package
                     :lisp-package (or lisp-pkg (substitute #\- #\_ package))
                     :imports  imports
                     :options  (if optimize
                                 (append options
                                         (list (make-option "optimize_for" (if (eq optimize :speed) "SPEED" "CODE_SIZE") 'symbol)))
                                 options)
                     :documentation documentation))
         (*protobuf* schema)
         (*protobuf-package* (or (find-proto-package lisp-pkg) *package*))
         (*protobuf-rpc-package* (or (find-proto-package (format nil "~A-~A" lisp-pkg 'rpc)) *package*)))
    (process-imports schema imports)
    (with-collectors ((forms collect-form))
      (dolist (msg messages)
        (assert (and (listp msg)
                     (member (car msg) '(define-enum define-message define-extend define-service
                                         define-type-alias))) ()
                "The body of ~S must be one of ~{~S~^ or ~}"
                'define-schema
                '(define-enum define-message define-extend define-service define-type-alias))
        ;; The macro-expander will return a form that consists
        ;; of 'progn' followed by a symbol naming what we've expanded
        ;; (define-enum, define-message, define-extend, define-service),
        ;; followed by the Lisp model object created by the defining form,
        ;; followed by other defining forms (e.g., deftype, defclass)
        (destructuring-bind (&optional progn model-type model definers)
            (macroexpand-1 msg env)
          (assert (eq progn 'progn) ()
                  "The macroexpansion for ~S failed" msg)
          (map () #'collect-form definers)
          (ecase model-type
            ((define-enum)
             (appendf (proto-enums schema) (list model)))
            ((define-type-alias)
             (appendf (proto-type-aliases schema) (list model)))
            ((define-message define-extend)
             (setf (proto-parent model) schema)
             (appendf (proto-messages schema) (list model))
             (when (eq (proto-message-type model) :extends)
               (appendf (proto-extenders schema) (list model))))
            ((define-service)
             (appendf (proto-services schema) (list model))))))
      (let ((var (intern (format nil "*~A*" type) *protobuf-package*)))
        `(progn
           ,@forms
           (defvar ,var nil)
           (let* ((old-schema ,var)
                  (new-schema ,schema))
             (when old-schema
               (multiple-value-bind (upgradable warnings)
                   (schema-upgradable old-schema new-schema)
                 (unless upgradable
                   (protobufs-warn "The old schema for ~S (~A) can't be safely upgraded; proceeding anyway"
                                   ',type ',name)
                   (map () #'protobufs-warn warnings))))
             (setq ,var new-schema)
             (record-protobuf ,var))
           ,@(with-collectors ((messages collect-message))
               (labels ((collect-messages (message)
                          (collect-message message)
                          (map () #'collect-messages (proto-messages message))))
                 (map () #'collect-messages (proto-messages schema)))
               (append 
                (mapcar #'(lambda (m) `(record-protobuf ,m)) messages)
                (when (eq optimize :speed)
                  (append (mapcar #'generate-object-size  messages)
                          (mapcar #'generate-serializer   messages)
                          (mapcar #'generate-deserializer messages)))))
           ,var)))))

(defmacro with-proto-source-location ((type name definition-type
                                       &optional pathname start-pos end-pos)
                                      &body body)
  "Establish a context which causes the generated Lisp code to have
   source location information that points to the .proto file.
   'type' is the name of the Lisp definition (a symbol).
   'name' is the name of the Protobufs definition (a string).
   'definition-type' is the kind of definition, e.g., 'protobuf-enum'.
   'pathname', 'start-pos' and 'end-pos' give the location of the definition
   in the .proto file."
  `(progn
     (record-proto-source-location ',type ,name ',definition-type
                                   ,pathname ,start-pos ,end-pos)
     ,@body))

#+ccl
(defun record-proto-source-location (type name definition-type
                                     &optional pathname start-pos end-pos)
  (declare (ignore name))
  (when (and ccl::*record-source-file*
             (typep pathname '(or string pathname)))
    (let ((ccl::*loading-toplevel-location* (ccl::make-source-note :filename  pathname
                                                                   :start-pos start-pos
                                                                   :end-pos   end-pos)))
      (ccl:record-source-file type definition-type))))

#-(or ccl)
(defun record-proto-source-location (type name definition-type
                                     &optional pathname start-pos end-pos)
  (declare (ignorable name type definition-type pathname start-pos end-pos)))

;; Define an enum type named 'type' and a Lisp 'deftype'
(defmacro define-enum (type (&key name conc-name alias-for options
                                  documentation source-location)
                       &body values)
  "Define a Protobufs enum type and a Lisp 'deftype' named 'type'.
   'name' can be used to override the defaultly generated Protobufs enum name.
   'conc-name' will be used as the prefix to the Lisp enum names, if it's supplied.
   If 'alias-for' is given, no Lisp 'deftype' will be defined. Instead, the enum
   will be used as an alias for an enum type that already exists in Lisp.
   'options' is a set of keyword/value pairs, both of which are strings.

   The body consists of the enum values in the form 'name' or (name index)."
  (let* ((name    (or name (class-name->proto type)))
         (options (loop for (key val) on options by #'cddr
                        collect (make-option (if (symbolp key) (slot-name->proto key) key) val)))
         (conc-name (conc-name-for-type type conc-name))
         (index -1)
         (enum  (make-instance 'protobuf-enum
                  :class  type
                  :name   name
                  :qualified-name (make-qualified-name *protobuf* name)
                  :parent *protobuf*
                  :alias-for alias-for
                  :options options
                  :documentation documentation
                  :source-location source-location)))
    (with-collectors ((vals  collect-val)
                      (forms collect-form))
      (dolist (val values)
        ;; Allow old (name index) and new (name :index index)
        (let* ((idx  (if (listp val)
                       (if (eq (second val) :index) (third val) (second val))
                       (incf index)))
               (name (if (listp val) (first val)  val))
               (val-name  (kintern (if conc-name (format nil "~A~A" conc-name name) (symbol-name name))))
               (enum-name (if conc-name (format nil "~A~A" conc-name name) (symbol-name name)))
               (vname     (enum-name->proto enum-name))
               (enum-val  (make-instance 'protobuf-enum-value
                            :name   vname
                            :qualified-name (make-qualified-name enum vname)
                            :index  idx
                            :value  val-name
                            :parent enum)))
          (collect-val val-name)
          (appendf (proto-values enum) (list enum-val))))
      (if alias-for
        ;; If we've got an alias, define a a type that is the subtype of
        ;; the Lisp enum so that typep and subtypep work
        (unless (eq type alias-for)
          (collect-form `(deftype ,type () ',alias-for)))
        ;; If no alias, define the Lisp enum type now
        (collect-form `(deftype ,type () '(member ,@vals))))
      `(progn
         define-enum
         ,enum
         ((with-proto-source-location (,type ,name protobuf-enum ,@source-location)
            ,@forms))))))

;; Helper for message-like forms
(defun generate-message-forms (type fields env &aux (index 0))
  "Generates the forms used by DEFINE-MESSAGE and DEFINE-GROUP."
  (with-accessors ((alias-for proto-alias-for)
                   (conc-name proto-conc-name)
                   (documentation proto-documentation))
      *protobuf*
    (with-collectors ((slots collect-slot)
                      (forms collect-form)
                      ;; The typedef needs to be first in forms otherwise ccl warns.
                      ;; We'll collect them separately and splice them in first.
                      (type-forms collect-type-form))
      (dolist (field fields)
        (case (car field)
          ((define-enum define-message define-extend define-extension define-group
             define-type-alias)
           (destructuring-bind (&optional progn model-type model definers extra-field extra-slot)
               (macroexpand-1 field env)
             (assert (eq progn 'progn) ()
                     "The macroexpansion for ~S failed" field)
             (map () #'collect-form definers)
             (ecase model-type
               ((define-enum)
                (appendf (proto-enums *protobuf*) (list model)))
               ((define-type-alias)
                (appendf (proto-type-aliases *protobuf*) (list model)))
               ((define-message define-extend)
                (setf (proto-parent model) *protobuf*)
                (appendf (proto-messages *protobuf*) (list model))
                (when (eq (proto-message-type model) :extends)
                  (appendf (proto-extenders *protobuf*) (list model))))
               ((define-group)
                (setf (proto-parent model) *protobuf*)
                (appendf (proto-messages *protobuf*) (list model))
                (when extra-slot
                  (collect-slot extra-slot))
                (appendf (proto-fields *protobuf*) (list extra-field)))
               ((define-extension)
                (appendf (proto-extensions *protobuf*) (list model))))))
          (otherwise
           (multiple-value-bind (field slot idx)
               (process-field field index :conc-name conc-name :alias-for alias-for)
             (assert (not (find-field *protobuf* (proto-index field))) ()
                     "The field ~S overlaps with another field in ~S"
                     (proto-value field) (proto-class *protobuf*))
             (setq index idx)
             (when slot
               (collect-slot slot))
             (appendf (proto-fields *protobuf*) (list field))))))
      (if alias-for
        ;; If we've got an alias, define a type that is the subtype of
        ;; the Lisp class that typep and subtypep work
        (unless (or (eq type alias-for) (find-class type nil))
          (collect-type-form `(deftype ,type () ',alias-for)))
        ;; If no alias, define the class now
        (collect-type-form `(defclass ,type (#+use-base-protobuf-message base-protobuf-message)
                              ,slots
                              ,@(and documentation `((:documentation ,documentation))))))
      (nconc type-forms forms))))


;; Define a message named 'name' and a Lisp 'defclass'
(defmacro define-message (type (&key name conc-name alias-for options
                                     documentation source-location)
                          &body fields &environment env)
  "Define a message named 'type' and a Lisp 'defclass'.
   'name' can be used to override the defaultly generated Protobufs message name.
   The body consists of fields, or 'define-enum' or 'define-message' forms.
   'conc-name' will be used as the prefix to the Lisp slot accessors, if it's supplied.
   If 'alias-for' is given, no Lisp class is defined. Instead, the message will be
   used as an alias for a class that already exists in Lisp. This feature is intended
   to be used to define messages that will be serialized from existing Lisp classes;
   unless you get the slot names or readers exactly right for each field, it will be
   the case that trying to (de)serialize into a Lisp object won't work.
   'options' is a set of keyword/value pairs, both of which are strings.

   Fields take the form (slot &key type name default reader writer)
   'slot' can be either a symbol giving the field name, or a list whose
   first element is the slot name and whose second element is the index.
   'type' is the type of the slot.
   'name' can be used to override the defaultly generated Protobufs field name.
   'default' is the default value for the slot.
   'reader' is a Lisp slot reader function to use to get the value, instead of
   using 'slot-value'; this is often used when aliasing an existing class.
   'writer' is a Lisp slot writer function to use to set the value."
  (let* ((name    (or name (class-name->proto type)))
         (options (loop for (key val) on options by #'cddr
                        collect (make-option (if (symbolp key) (slot-name->proto key) key) val)))
         (conc-name (conc-name-for-type type conc-name))
         (message (make-instance 'protobuf-message
                    :class type
                    :name  name
                    :qualified-name (make-qualified-name *protobuf* name)
                    :parent *protobuf*
                    :alias-for alias-for
                    :conc-name conc-name
                    :options   (remove-options options "default" "packed")
                    :documentation documentation
                    :source-location source-location))
         ;; Only now can we bind *protobuf* to the new message
         (*protobuf* message))
    `(progn
       define-message
       ,message
       ((with-proto-source-location (,type ,name protobuf-message ,@source-location)
          ,@(generate-message-forms type fields env))))))

(defun conc-name-for-type (type conc-name)
  (and conc-name
       (typecase conc-name
         ((member t) (format nil "~:@(~A~)-" type))
         ((or string symbol) (string-upcase (string conc-name)))
         (t nil))))

(defmacro define-extension (from to)
  "Define an extension range within a message.
   The \"body\" is the start and end of the range, both inclusive."
  (let ((to (etypecase to
              (integer to)
              (symbol (if (string-equal to "MAX") #.(1- (ash 1 29)) to)))))
    `(progn
       define-extension
       ,(make-instance 'protobuf-extension
          :from from
          :to   (if (eq to 'max) #.(1- (ash 1 29)) to))
       ())))

(defmacro define-extend (type (&key name conc-name options documentation)
                         &body fields &environment env)
  "Define an extension to the message named 'type'.
   'name' can be used to override the defaultly generated Protobufs message name.
   The body consists only  of fields.
   'options' is a set of keyword/value pairs, both of which are strings.

   Fields take the form (slot &key type name default reader writer)
   'slot' can be either a symbol giving the field name, or a list whose
   first element is the slot name and whose second element is the index.
   'type' is the type of the slot.
   'name' can be used to override the defaultly generated Protobufs field name.
   'default' is the default value for the slot.
   'reader' is a Lisp slot reader function to use to get the value, instead of
   using 'slot-value'; this is often used when aliasing an existing class.
   'writer' is a Lisp slot writer function to use to set the value."
  (flet ((gen-extend-field-forms (slot &optional field)
           (let* ((inits  (cdr slot))
                  (sname  (car slot))
                  (stable (fintern "~A-VALUES" sname))
                  (stype  (getf inits :type))
                  (reader (or (getf inits :accessor)
                              (getf inits :reader)
                              (intern (if conc-name (format nil "~A~A" conc-name sname) (symbol-name sname))
                                      *protobuf-package*)))
                  (writer (or (getf inits :writer)
                              (intern (format nil "~A-~A" 'set reader) *protobuf-package*)))
                  (default (getf inits :initform)))
             (when field
               ;; This so that (de)serialization works
               (setf (proto-reader field) reader
                     (proto-writer field) writer))

             ;; For the extended slots, each slot gets its own table
             ;; keyed by the object, which lets us avoid having a slot in each
             ;; instance that holds a table keyed by the slot name.
             ;; Multiple 'define-extends' on the same class in the same image
             ;; will result in harmless redefinitions, so squelch the warnings
             ;;--- Maybe these methods need to be defined in 'define-message'?
             `(without-redefinition-warnings ()
                (let ((,stable (tg:make-weak-hash-table :weakness :value :test #'eq)))
                  ,@(and reader `((defmethod ,reader ((object ,type))
                                    (gethash object ,stable ,default))))
                  ,@(and writer `((defmethod ,writer ((object ,type) value)
                                    (declare (type ,stype value))
                                    (setf (gethash object ,stable) value))))
                  ;; For Python compatibility
                  (defmethod get-extension ((object ,type) (slot (eql ',sname)))
                    (values (gethash object ,stable ,default)))
                  (defmethod set-extension ((object ,type) (slot (eql ',sname)) value)
                    (setf (gethash object ,stable) value))
                  (defmethod has-extension ((object ,type) (slot (eql ',sname)))
                    (multiple-value-bind (value foundp)
                        (gethash object ,stable)
                      (declare (ignore value))
                      foundp))
                  (defmethod clear-extension ((object ,type) (slot (eql ',sname)))
                    (remhash object ,stable)))
                ;; 'defsetf' needs to be visible at compile time
                ,@(and writer `((eval-when (:compile-toplevel :load-toplevel :execute)
                                  (defsetf ,reader ,writer)))))))
         (process-extend-field (field)
           (setf (proto-message-type field) :extends) ;this field is an extension
           (appendf (proto-fields *protobuf*) (list field))
           (appendf (proto-extended-fields *protobuf*) (list field))))
    (let* ((name    (or name (class-name->proto type)))
           (options (loop for (key val) on options by #'cddr
                          collect (make-option (if (symbolp key) (slot-name->proto key) key) val)))
           (message   (find-message *protobuf* type))
           (conc-name (or (conc-name-for-type type conc-name)
                          (and message (proto-conc-name message))))
           (alias-for (and message (proto-alias-for message)))
           (extends (and message
                         (make-instance 'protobuf-message
                           :class  (proto-class message)
                           :name   (proto-name message)
                           :qualified-name (proto-qualified-name message)
                           :parent *protobuf*
                           :alias-for alias-for
                           :conc-name conc-name
                           :enums    (copy-list (proto-enums message))
                           :messages (copy-list (proto-messages message))
                           :fields   (copy-list (proto-fields message))
                           :extensions (copy-list (proto-extensions message))
                           :options  (remove-options
                                      (or options (copy-list (proto-options message))) "default" "packed")
                           :message-type :extends ;this message is an extension
                           :documentation documentation
                           :type-aliases  (copy-list (proto-type-aliases message)))))
           ;; Only now can we bind *protobuf* to the new extended message
           (*protobuf* extends)
           (index 0))
      (assert message ()
              "There is no message named ~A to extend" name)
      (assert (eq type (proto-class message)) ()
              "The type ~S doesn't match the type of the message being extended ~S"
              type message)
      (with-collectors ((forms collect-form))
        (dolist (field fields)
          (assert (not (member (car field)
                               '(define-enum define-message define-extend define-extension
                                 define-type-alias))) ()
                                 "The body of ~S can only contain field and group definitions" 'define-extend)
          (case (car field)
            ((define-group)
             (destructuring-bind (&optional progn model-type model definers extra-field extra-slot)
                 (macroexpand-1 field env)
               (assert (and (eq progn 'progn)
                            (eq model-type 'define-group))
                            ()
                       "The macroexpansion for ~S failed" field)
               (map () #'collect-form definers)
               (setf (proto-parent model) extends)
               (appendf (proto-messages extends) (list model))
               (when extra-slot
                 (collect-form (gen-extend-field-forms extra-slot)))
               (process-extend-field extra-field)))
            (otherwise
             (multiple-value-bind (field slot idx)
                 (process-field field index :conc-name conc-name :alias-for alias-for)
               (assert (not (find-field extends (proto-index field))) ()
                       "The field ~S overlaps with another field in ~S"
                       (proto-value field) (proto-class extends))
               (assert (index-within-extensions-p idx message) ()
                       "The index ~D is not in range for extending ~S"
                       idx (proto-class message))
               (setq index idx)
               (when slot
                 (collect-form (gen-extend-field-forms slot field)))
               (process-extend-field field)))))
        `(progn
           define-extend
           ,extends
           ,forms)))))

(defun index-within-extensions-p (index message)
  (let ((extensions (proto-extensions message)))
    (some #'(lambda (ext)
              (and (i>= index (proto-extension-from ext))
                   (i<= index (proto-extension-to ext))))
          extensions)))

(defmacro define-group (type (&key index arity name conc-name alias-for reader options
                                   documentation source-location)
                        &body fields &environment env)
  "Define a message named 'type' and a Lisp 'defclass', *and* a field named type.
   This is deprecated in Protobufs, but if you have to use it, you must give
   'index' as the field index and 'arity' of :required, :optional or :repeated.
   'name' can be used to override the defaultly generated Protobufs message name.
   The body consists of fields, or 'define-enum' or 'define-message' forms.
   'conc-name' will be used as the prefix to the Lisp slot accessors, if it's supplied.
   If 'alias-for' is given, no Lisp class is defined. Instead, the message will be
   used as an alias for a class that already exists in Lisp. This feature is intended
   to be used to define messages that will be serialized from existing Lisp classes;
   unless you get the slot names or readers exactly right for each field, it will be
   the case that trying to (de)serialize into a Lisp object won't work.
   'options' is a set of keyword/value pairs, both of which are strings.

   Fields take the form (slot &key type name default reader writer)
   'slot' can be either a symbol giving the field name, or a list whose
   first element is the slot name and whose second element is the index.
   'type' is the type of the slot.
   'name' can be used to override the defaultly generated Protobufs field name.
   'default' is the default value for the slot.
   'reader' is a Lisp slot reader function to use to get the value, instead of
   using 'slot-value'; this is often used when aliasing an existing class.
   'writer' is a Lisp slot writer function to use to set the value."
  (check-type index integer)
  (check-type arity (member :required :optional :repeated))
  (let* ((slot    (or type (and name (proto->slot-name name *protobuf-package*))))
         (name    (or name (class-name->proto type)))
         (options (loop for (key val) on options by #'cddr
                        collect (make-option (if (symbolp key) (slot-name->proto key) key) val)))
         (conc-name (conc-name-for-type type conc-name))
         (reader  (or reader
                      (let ((msg-conc (proto-conc-name *protobuf*)))
                        (and msg-conc
                             (intern (format nil "~A~A" msg-conc slot) *protobuf-package*)))))
         (mslot   (unless alias-for
                    `(,slot ,@(case arity
                                (:required
                                 `(:type ,type))
                                (:optional
                                 `(:type (or ,type null)
                                   :initform nil))
                                (:repeated
                                 `(:type (list-of ,type)
                                   :initform ())))
                            ,@(and reader
                                   `(:accessor ,reader))
                            :initarg ,(kintern (symbol-name slot)))))
         (mfield  (make-instance 'protobuf-field
                    :name  (slot-name->proto slot)
                    :type  name
                    :class type
                    :qualified-name (make-qualified-name *protobuf* (slot-name->proto slot))
                    :parent *protobuf*
                    :required arity
                    :index index
                    :value slot
                    :reader reader
                    :message-type :group))
         (message (make-instance 'protobuf-message
                    :class type
                    :name  name
                    :qualified-name (make-qualified-name *protobuf* name)
                    :parent *protobuf*
                    :alias-for alias-for
                    :conc-name conc-name
                    :options   (remove-options options "default" "packed")
                    :message-type :group                ;this message is a group
                    :documentation documentation
                    :source-location source-location))
         ;; Only now can we bind *protobuf* to the (group) message
         (*protobuf* message))
    `(progn
       define-group
       ,message
       ((with-proto-source-location (,type ,name protobuf-message ,@source-location)
          ,@(generate-message-forms type fields env)))
       ,mfield
       ,mslot)))

(defun process-field (field index &key conc-name alias-for)
  "Process one field descriptor within 'define-message' or 'define-extend'.
   Returns a 'proto-field' object, a CLOS slot form and the incremented field index."
  (when (i= index 18999)                                ;skip over the restricted range
    (setq index 19999))
  (destructuring-bind (slot &rest other-options 
                       &key type reader writer name (default nil default-p) packed
                            ((:index idx)) options documentation &allow-other-keys) field
    ;; Allow old ((slot index) ...) or new (slot :index ...),
    ;; but only allow one of those two to be used simultaneously
    (assert (if idx (not (listp slot)) t) ()
            "Use either ((slot index) ...)  or (slot :index index ...), but not both")
    (let* ((idx  (or idx (if (listp slot) (second slot) (iincf index))))
           (slot (if (listp slot) (first slot) slot))
           (reader (or reader
                       (and conc-name
                            (intern (format nil "~A~A" conc-name slot) *protobuf-package*))))
           (options (append
                     (loop for (key val) on other-options by #'cddr
                           unless (member key '(:type :reader :writer :name :default :packed :documentation))
                             collect (make-option (slot-name->proto key) val))
                     (loop for (key val) on options by #'cddr
                           collect (make-option (if (symbolp key) (slot-name->proto key) key) val)))))
      (multiple-value-bind (ptype pclass)
          (clos-type-to-protobuf-type type)
        (multiple-value-bind (reqd vectorp)
            (clos-type-to-protobuf-required type)
          (let* ((default (if (eq reqd :repeated)
                            (if vectorp $empty-vector $empty-list)      ;to distinguish between list-of and vector-of
                            (if default-p default $empty-default)))
                 (cslot (unless alias-for
                          `(,slot :type ,type
                                  ,@(and reader
                                         (if writer
                                           `(:reader ,reader)
                                           `(:accessor ,reader)))
                                  ,@(and writer
                                         `(:writer ,writer))
                                  :initarg ,(kintern (symbol-name slot))
                                  ,@(cond ((eq reqd :repeated)
                                           ;; Repeated fields get a container for their elements
                                           (if vectorp
                                             `(:initform (make-array 5 :fill-pointer 0 :adjustable t))
                                             `(:initform ())))
                                          ((and (not default-p)
                                                (eq reqd :optional)
                                                ;; Use unbound for booleans only
                                                (not (eq pclass :bool)))
                                           `(:initform nil))
                                          (default-p
                                            `(:initform ,(protobuf-default-to-clos-init default type)))))))
                 (field (make-instance 'protobuf-field
                          :name  (or name (slot-name->proto slot))
                          :type  ptype
                          :class pclass
                          :qualified-name (make-qualified-name *protobuf* (or name (slot-name->proto slot)))
                          :parent *protobuf*
                          ;; One of :required, :optional or :repeated
                          :required reqd
                          :index  idx
                          :value  slot
                          :reader reader
                          :writer writer
                          :default default
                          ;; Pack the field only if requested and it actually makes sense
                          :packed  (and (eq reqd :repeated) packed t)
                          :options options
                          :documentation documentation)))
            (values field cslot idx)))))))

(defparameter *rpc-package* nil
  "The Lisp package that implements RPC.
   This should be set when an RPC package that uses CL-Protobufs gets loaded.")
(defparameter *rpc-call-function* nil
  "The Lisp function that implements RPC client-side calls.
   This should be set when an RPC package that uses CL-Protobufs gets loaded.")

;; Define a service named 'type' with generic functions declared for
;; each of the methods within the service
(defmacro define-service (type (&key name options
                                     documentation source-location)
                          &body method-specs)
  "Define a service named 'type' and Lisp 'defgeneric' for all its methods.
   'name' can be used to override the defaultly generated Protobufs service name.
   'options' is a set of keyword/value pairs, both of which are strings.

   The body is a set of method specs of the form (name (input-type [=>] output-type) &key options).
   'input-type' and 'output-type' may also be of the form (type &key name)."
  (let* ((name    (or name (class-name->proto type)))
         (options (loop for (key val) on options by #'cddr
                        collect (make-option (if (symbolp key) (slot-name->proto key) key) val)))
         (service (make-instance 'protobuf-service
                    :class type
                    :name  name
                    :qualified-name (make-qualified-name *protobuf* name)
                    :parent *protobuf*
                    :options options
                    :documentation documentation
                    :source-location source-location))
         (index 0))
    (with-collectors ((forms collect-form))
      (dolist (method method-specs)
        (destructuring-bind (function (&rest types)
                             &key name options documentation source-location) method
          (let* ((input-type   (first types))
                 (output-type  (if (string= (string (second types)) "=>") (third types) (second types)))
                 (streams-type (if (string= (string (second types)) "=>")
                                 (getf (cdddr types) :streams)
                                 (getf (cddr  types) :streams)))
                 (input-name (and (listp input-type)
                                  (getf (cdr input-type) :name)))
                 (input-type (if (listp input-type) (car input-type) input-type))
                 (output-name (and (listp output-type)
                                   (getf (cdr output-type) :name)))
                 (output-type (if (listp output-type) (car output-type) output-type))
                 (streams-name (and (listp streams-type)
                                    (getf (cdr streams-type) :name)))
                 (streams-type (if (listp streams-type) (car streams-type) streams-type))
                 (options (loop for (key val) on options by #'cddr
                                collect (make-option (if (symbolp key) (slot-name->proto key) key) val)))
                 (package   *protobuf-rpc-package*)
                 (client-fn (intern (format nil "~A-~A" 'call function) package))
                 (server-fn (intern (format nil "~A-~A" function 'impl) package))
                 (method  (make-instance 'protobuf-method
                            :class function
                            :name  (or name (class-name->proto function))
                            :qualified-name (make-qualified-name *protobuf* (or name (class-name->proto function)))
                            :parent service
                            :client-stub client-fn
                            :server-stub server-fn
                            :input-type  input-type
                            :input-name  (or input-name (class-name->proto input-type))
                            :output-type output-type
                            :output-name (or output-name (class-name->proto output-type))
                            :streams-type streams-type
                            :streams-name (and streams-type
                                               (or streams-name (class-name->proto streams-type)))
                            :index (iincf index)
                            :options options
                            :documentation documentation
                            :source-location source-location)))
            (appendf (proto-methods service) (list method))
            ;; The following are the hooks to an RPC implementation
            (let* ((vrequest  (intern (symbol-name 'request) package))
                   (vchannel  (intern (symbol-name 'channel) package))
                   (vcallback (intern (symbol-name 'callback) package)))
              ;; The client side stub, e.g., 'read-air-reservation'.
              ;; The expectation is that the RPC implementation will provide code to make it
              ;; easy to implement a method for this on each kind of channel (HTTP, TCP socket,
              ;; IPC, etc). Unlike C++/Java/Python, we don't need a client-side subclass,
              ;; because we can just use multi-methods.
              ;; The 'do-XXX' method calls the RPC code with the channel, the method
              ;; (i.e., a 'protobuf-method' object), the request and the callback function.
              ;; The RPC code should take care of serializing the input, transmitting the
              ;; request over the wire, waiting for input (or not if it's asynchronous),
              ;; filling in the output, and either returning the response (if synchronous)
              ;; or calling the callback with the response as an argument (if asynchronous).
              ;; It will also deserialize the response so that the client code sees the
              ;; response as an application object.
              (collect-form `(defgeneric ,client-fn (,vchannel ,vrequest &key ,vcallback)
                               ,@(and documentation `((:documentation ,documentation)))
                               #+(or ccl)
                               (declare (values ,output-type))
                               (:method (,vchannel (,vrequest ,input-type) &key ,vcallback)
                                 (declare (ignorable ,vchannel ,vcallback))
                                 (let ((call (and *rpc-package* *rpc-call-function*)))
                                   (assert call ()
                                           "There is no RPC package loaded!")
                                   (funcall call ,vchannel ',method ,vrequest
                                            :callback ,vcallback)))))
              ;; The server side stub, e.g., 'do-read-air-reservation'.
              ;; The expectation is that the server-side program will implement
              ;; a method with the business logic for this on each kind of channel
              ;; (HTTP, TCP socket, IPC, etc), possibly on a server-side subclass
              ;; of the input class.
              ;; The business logic is expected to perform the correct operations on
              ;; the input object, which arrived via Protobufs, and produce an output
              ;; of the given type, which will be serialized and sent back over the wire.
              ;; The channel objects hold client identity information, deadline info,
              ;; etc, and can be side-effected to indicate success or failure.
              ;; The RPC code provides the channel classes and does (de)serialization, etc
              (collect-form `(defgeneric ,server-fn (,vchannel ,vrequest)
                               ,@(and documentation `((:documentation ,documentation)))
                               #+(or ccl)
                               (declare (values ,output-type))))))))
      `(progn
         define-service
         ,service
         ((with-proto-source-location (,type ,name protobuf-service ,@source-location)
            ,@forms))))))


;; Lisp-only type aliases
(defmacro define-type-alias (type (&key name alias-for documentation source-location)
                             &key lisp-type proto-type serializer deserializer)
  "Define a Protobufs type alias Lisp 'deftype' named 'type'.
   'lisp-type' is the name of the Lisp type.
   'proto-type' is the name of a primitive Protobufs type, e.g., 'int32' or 'string'.
   'serializer' is a function that takes a Lisp object and generates a Protobufs object.
   'deserializer' is a function that takes a Protobufs object and generates a Lisp object.
   If 'alias-for' is given, no Lisp 'deftype' will be defined."
  (multiple-value-bind (type-str proto)
      (lisp-type-to-protobuf-type proto-type)
    (assert (keywordp proto) ()
            "The alias ~S must resolve to a Protobufs primitive type"
            type)
    (let* ((name  (or name (class-name->proto type)))
           (alias (make-instance 'protobuf-type-alias
                    :class  type
                    :name   name
                    :lisp-type  lisp-type
                    :proto-type proto
                    :proto-type-str type-str
                    :serializer   serializer
                    :deserializer deserializer
                    :qualified-name (make-qualified-name *protobuf* name)
                    :parent *protobuf*
                    :documentation documentation
                    :source-location source-location)))
      (with-collectors ((forms collect-form))
        (if alias-for
            ;; If we've got an alias, define a a type that is the subtype of
            ;; the Lisp enum so that typep and subtypep work
            (unless (eq type alias-for)
              (collect-form `(deftype ,type () ',alias-for)))
            ;; If no alias, define the Lisp enum type now
            (collect-form `(deftype ,type () ',lisp-type)))
        `(progn
           define-type-alias
           ,alias
           ((with-proto-source-location (,type ,name protobuf-type-alias ,@source-location)
              ,@forms)))))))


;;; Ensure everything in a Protobufs schema is defined

(defvar *undefined-messages* nil
  "Bound to a list of undefined messages during schame validation.")

;; A very useful tool during development...
(defun ensure-all-schemas ()
  (let ((protos (sort
                 (delete-duplicates
                  (loop for p being the hash-values of *all-schemas*
                        collect p))
                 #'string< :key #'proto-name)))
    (mapcan #'ensure-schema protos)))

(defgeneric ensure-schema (schema)
  (:documentation
   "Ensure that all of the types are defined in the Protobufs schema 'schema'.
    This returns two values:
     - A list whose elements are (<undefined-type> \"message:field\" ...)
     - The accumulated warnings table that has the same information as objects.")
  (:method ((schema protobuf-schema))
    (let ((*undefined-messages* (make-hash-table))
          (trace (list schema)))
      (map () (curry #'ensure-message trace) (proto-messages schema))
      (map () (curry #'ensure-service trace) (proto-services schema))
      (loop for type being the hash-keys of *undefined-messages*
              using (hash-value things)
            collect (list* type
                           (mapcar #'(lambda (thing)
                                       (format nil "~A:~A" (proto-name (car thing)) (proto-name (cdr thing))))
                                   things)) into warnings
            finally (return (values warnings *undefined-messages*))))))

(defgeneric ensure-message (trace message)
  (:method (trace (message protobuf-message))
    (let ((trace (cons message trace)))
      (map () (curry #'ensure-message trace) (proto-messages message))
      (map () (curry #'ensure-field trace message) (proto-fields message)))))

(defgeneric ensure-field (trace message field)
  (:method (trace message (field protobuf-field))
    (ensure-type trace message field (proto-class field))))

(defgeneric ensure-service (trace service)
  (:method (trace (service protobuf-service))
    (map () (curry #'ensure-method trace service) (proto-methods service))))

(defgeneric ensure-method (trace service method)
  (:method (trace service (method protobuf-method))
    (ensure-type trace service method (proto-input-type method))
    (ensure-type trace service method (proto-output-type method))
    (ensure-type trace service method (proto-streams-type method))))

;; 'message' and 'field' can be a message and a field or a service and a method
(defun ensure-type (trace message field type)
  (unless (keywordp type)
    (let ((msg (loop for p in trace
                     thereis (or (find-message p type)
                                 (find-enum p type)))))
      (unless msg
        (push (cons message field) (gethash type *undefined-messages*))))))
