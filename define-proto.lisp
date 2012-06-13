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


;;; Protocol buffer defining macros

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
                           collect (make-instance 'protobuf-option
                                     :name  (if (symbolp key) (slot-name->proto key) key)
                                     :value val))
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
                                 (append options (list (make-instance 'protobuf-option
                                                         :name  "optimize_for"
                                                         :value (if (eq optimize :speed) "SPEED" "CODE_SIZE")
                                                         :type  'symbol)))
                                 options)
                     :documentation documentation))
         (*protobuf* schema)
         (*protobuf-package* (or (find-proto-package lisp-pkg) *package*)))
    (apply #'process-imports schema imports)
    (with-collectors ((forms collect-form))
      (dolist (msg messages)
        (assert (and (listp msg)
                     (member (car msg) '(define-enum define-message define-extend define-service))) ()
                "The body of ~S must be one of ~{~S~^ or ~}"
                'define-schema '(define-enum define-message define-extend define-service))
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
             (setf (proto-enums schema) (nconc (proto-enums schema) (list model))))
            ((define-message define-extend)
             (setf (proto-parent model) schema)
             (setf (proto-messages schema) (nconc (proto-messages schema) (list model)))
             (when (eq (proto-message-type model) :extends)
               (setf (proto-extenders schema) (nconc (proto-extenders schema) (list model)))))
            ((define-service)
             (setf (proto-services schema) (nconc (proto-services schema) (list model)))))))
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
             (record-protobuf ,var)
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
             ,var))))))

;; Define an enum type named 'type' and a Lisp 'deftype'
(defmacro define-enum (type (&key name conc-name alias-for options documentation)
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
                        collect (make-instance 'protobuf-option
                                  :name  (if (symbolp key) (slot-name->proto key) key)
                                  :value val)))
         (conc-name (conc-name-for-type type conc-name))
         (index -1)
         (enum  (make-instance 'protobuf-enum
                  :class  type
                  :name   name
                  :alias-for alias-for
                  :options options
                  :documentation documentation)))
    (with-collectors ((vals  collect-val)
                      (forms collect-form))
      (dolist (val values)
        (let* ((idx  (if (listp val) (second val) (incf index)))
               (name (if (listp val) (first val)  val))
               (val-name  (kintern (if conc-name (format nil "~A~A" conc-name name) (symbol-name name))))
               (enum-name (if conc-name (format nil "~A~A" conc-name name) (symbol-name name)))
               (enum-val  (make-instance 'protobuf-enum-value
                            :name  (enum-name->proto enum-name)
                            :index idx
                            :value val-name)))
          (collect-val val-name)
          (setf (proto-values enum) (nconc (proto-values enum) (list enum-val)))))
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
         ,forms))))

;; Define a message named 'name' and a Lisp 'defclass'
(defmacro define-message (type (&key name conc-name alias-for options documentation)
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

   Fields take the form (slot &key type name default reader)
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
                        collect (make-instance 'protobuf-option
                                  :name  (if (symbolp key) (slot-name->proto key) key)
                                  :value val)))
         (conc-name (conc-name-for-type type conc-name))
         (message (make-instance 'protobuf-message
                    :class type
                    :name  name
                    :parent *protobuf*
                    :alias-for alias-for
                    :conc-name conc-name
                    :options   (remove-options options "default" "packed")
                    :documentation documentation))
         (index 0)
         (*protobuf* message))
    (with-collectors ((slots collect-slot)
                      (forms collect-form))
      (dolist (field fields)
        (case (car field)
          ((define-enum define-message define-extend define-extension define-group)
           (destructuring-bind (&optional progn model-type model definers extra-field extra-slot)
               (macroexpand-1 field env)
             (assert (eq progn 'progn) ()
                     "The macroexpansion for ~S failed" field)
             (map () #'collect-form definers)
             (ecase model-type
               ((define-enum)
                (setf (proto-enums message) (nconc (proto-enums message) (list model))))
               ((define-message define-extend)
                (setf (proto-parent model) message)
                (setf (proto-messages message) (nconc (proto-messages message) (list model)))
                (when (eq (proto-message-type model) :extends)
                  (setf (proto-extenders message) (nconc (proto-extenders message) (list model)))))
               ((define-group)
                (setf (proto-parent model) message)
                (setf (proto-messages message) (nconc (proto-messages message) (list model)))
                (when extra-slot
                  (collect-slot extra-slot))
                (setf (proto-fields message) (nconc (proto-fields message) (list extra-field))))
               ((define-extension)
                (setf (proto-extensions message) (nconc (proto-extensions message) (list model)))))))
          (otherwise
           (multiple-value-bind (field slot idx)
               (process-field field index :conc-name conc-name :alias-for alias-for)
             (assert (not (find-field message (proto-index field))) ()
                     "The field ~S overlaps with another field in ~S"
                     (proto-value field) (proto-class message))
             (setq index idx)
             (when slot
               (collect-slot slot))
             (setf (proto-fields message) (nconc (proto-fields message) (list field)))))))
      (if alias-for
        ;; If we've got an alias, define a a type that is the subtype of
        ;; the Lisp class that typep and subtypep work
        (unless (or (eq type alias-for) (find-class type nil))
          (collect-form `(deftype ,type () ',alias-for)))
        ;; If no alias, define the class now
        (collect-form `(defclass ,type () (,@slots)
                         ,@(and documentation `((:documentation ,documentation))))))
      `(progn
         define-message
         ,message
         ,forms))))

(defun conc-name-for-type (type conc-name)
  (and conc-name
       (typecase conc-name
         ((member t) (format nil "~A-" type))
         ((or string symbol) (string conc-name))
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

   Fields take the form (slot &key type name default reader)
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
                        collect (make-instance 'protobuf-option
                                  :name  (if (symbolp key) (slot-name->proto key) key)
                                  :value val)))
         (message   (find-message *protobuf* name))
         (conc-name (or (conc-name-for-type type conc-name)
                        (and message (proto-conc-name message))))
         (alias-for (and message (proto-alias-for message)))
         (extends (and message
                       (make-instance 'protobuf-message
                         :class  type
                         :name   name
                         :parent (proto-parent message)
                         :alias-for alias-for
                         :conc-name conc-name
                         :enums    (copy-list (proto-enums message))
                         :messages (copy-list (proto-messages message))
                         :fields   (copy-list (proto-fields message))
                         :options  (remove-options
                                     (or options (copy-list (proto-options message))) "default" "packed")
                         :extensions (copy-list (proto-extensions message))
                         :message-type :extends         ;this message is an extension
                         :documentation documentation)))
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
                             '(define-enum define-message define-extend define-extension))) ()
                "The body of ~S can only contain field and group definitions" 'define-extend)
        (case (car field)
          ((define-group)
           (destructuring-bind (&optional progn model-type model definers extra-field extra-slot)
               (macroexpand-1 field env)
             (assert (eq progn 'progn) ()
                     "The macroexpansion for ~S failed" field)
             (map () #'collect-form definers)
             (ecase model-type
               ((define-group)
                (setf (proto-parent model) extends)
                (setf (proto-messages extends) (nconc (proto-messages extends) (list model)))
                (when extra-slot
                  ;;--- Refactor to get rid of all this duplicated code!
                  (let* ((inits  (cdr extra-slot))
                         (sname  (car extra-slot))
                         (stable (fintern "~A-VALUES" sname))
                         (stype  (getf inits :type))
                         (reader (or (getf inits :accessor)
                                     (getf inits :reader)
                                     (intern (if conc-name (format nil "~A~A" conc-name sname) (symbol-name sname))
                                             *protobuf-package*)))
                         (writer (or (getf inits :writer)
                                     (intern (format nil "~A-~A" 'set reader) *protobuf-package*)))
                         (default (getf inits :initform)))
                    (collect-form `(without-redefinition-warnings ()
                                     (let ((,stable #+ccl  (make-hash-table :test #'eq :weak t)
                                                    #+sbcl (make-hash-table :test #'eq :weakness :value)))
                                       ,@(and reader `((defmethod ,reader ((object ,type))
                                                         (gethash object ,stable ,default))))
                                       ,@(and writer `((defmethod ,writer ((object ,type) value)
                                                         (declare (type ,stype value))
                                                         (setf (gethash object ,stable) value))))
                                       ,@(and writer `((defsetf ,reader ,writer)))
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
                                         (remhash object ,stable)))))))
                (setf (proto-message-type extra-field) :extends) ;this field is an extension
                (setf (proto-fields extends) (nconc (proto-fields extends) (list extra-field)))
                (setf (proto-extended-fields extends) (nconc (proto-extended-fields extends) (list extra-field)))))))
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
                 ;; For the extended slots, each slot gets its own table
                 ;; keyed by the object, which lets us avoid having a slot in each
                 ;; instance that holds a table keyed by the slot name
                 ;; Multiple 'define-extends' on the same class in the same image
                 ;; will result in harmless redefinitions, so squelch the warnings
                 ;;--- Maybe these methods need to be defined in 'define-message'?
                 (collect-form `(without-redefinition-warnings ()
                                  (let ((,stable #+ccl  (make-hash-table :test #'eq :weak t)
                                                 #+sbcl (make-hash-table :test #'eq :weakness :value)))
                                    ,@(and reader `((defmethod ,reader ((object ,type))
                                                      (gethash object ,stable ,default))))
                                    ,@(and writer `((defmethod ,writer ((object ,type) value)
                                                      (declare (type ,stype value))
                                                      (setf (gethash object ,stable) value))))
                                    ,@(and writer `((defsetf ,reader ,writer)))
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
                                      (remhash object ,stable)))))
                 ;; This so that (de)serialization works
                 (setf (proto-reader field) reader
                       (proto-writer field) writer)))
             (setf (proto-message-type field) :extends)         ;this field is an extension
             (setf (proto-fields extends) (nconc (proto-fields extends) (list field)))
             (setf (proto-extended-fields extends) (nconc (proto-extended-fields extends) (list field)))))))
      `(progn
         define-extend
         ,extends
         ,forms))))

(defun index-within-extensions-p (index message)
  (let ((extensions (proto-extensions message)))
    (some #'(lambda (ext)
              (and (i>= index (proto-extension-from ext))
                   (i<= index (proto-extension-to ext))))
          extensions)))

(defmacro define-group (type (&key index arity name conc-name alias-for reader options documentation)
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

   Fields take the form (slot &key type name default reader)
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
                        collect (make-instance 'protobuf-option
                                  :name  (if (symbolp key) (slot-name->proto key) key)
                                  :value val)))
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
                    :required arity
                    :index index
                    :value slot
                    :reader reader
                    :message-type :group))
         (message (make-instance 'protobuf-message
                    :class type
                    :name  name
                    :alias-for alias-for
                    :conc-name conc-name
                    :options   (remove-options options "default" "packed")
                    :message-type :group                ;this message is a group
                    :documentation documentation))
         (index 0)
         (*protobuf* message))
    (with-collectors ((slots collect-slot)
                      (forms collect-form))
      (dolist (field fields)
        (case (car field)
          ((define-enum define-message define-extend define-extension define-group)
           (destructuring-bind (&optional progn model-type model definers extra-field extra-slot)
               (macroexpand-1 field env)
             (assert (eq progn 'progn) ()
                     "The macroexpansion for ~S failed" field)
             (map () #'collect-form definers)
             (ecase model-type
               ((define-enum)
                (setf (proto-enums message) (nconc (proto-enums message) (list model))))
               ((define-message define-extend)
                (setf (proto-parent model) message)
                (setf (proto-messages message) (nconc (proto-messages message) (list model)))
                (when (eq (proto-message-type model) :extends)
                  (setf (proto-extenders message) (nconc (proto-extenders message) (list model)))))
               ((define-group)
                (setf (proto-parent model) message)
                (setf (proto-messages message) (nconc (proto-messages message) (list model)))
                (when extra-slot
                  (collect-slot extra-slot))
                (setf (proto-fields message) (nconc (proto-fields message) (list extra-field))))
               ((define-extension)
                (setf (proto-extensions message) (nconc (proto-extensions message) (list model)))))))
          (otherwise
           (multiple-value-bind (field slot idx)
               (process-field field index :conc-name conc-name :alias-for alias-for)
             (assert (not (find-field message (proto-index field))) ()
                     "The field ~S overlaps with another field in ~S"
                     (proto-value field) (proto-class message))
             (setq index idx)
             (when slot
               (collect-slot slot))
             (setf (proto-fields message) (nconc (proto-fields message) (list field)))))))
      (if alias-for
        ;; If we've got an alias, define a a type that is the subtype of
        ;; the Lisp class that typep and subtypep work
        (unless (or (eq type alias-for) (find-class type nil))
          (collect-form `(deftype ,type () ',alias-for)))
        ;; If no alias, define the class now
        (collect-form `(defclass ,type () (,@slots)
                         ,@(and documentation `((:documentation ,documentation))))))
      `(progn
         define-group
         ,message
         ,forms
         ,mfield
         ,mslot))))

(defun process-field (field index &key conc-name alias-for)
  "Process one field descriptor within 'define-message' or 'define-extend'.
   Returns a 'proto-field' object, a CLOS slot form and the incremented field index."
  (when (i= index 18999)                                ;skip over the restricted range
    (setq index 19999))
  (destructuring-bind (slot &rest other-options 
                       &key type reader writer name (default nil default-p) packed
                            options documentation &allow-other-keys) field
    (let* ((idx  (if (listp slot) (second slot) (iincf index)))
           (slot (if (listp slot) (first slot) slot))
           (reader (or reader
                       (and conc-name
                            (intern (format nil "~A~A" conc-name slot) *protobuf-package*))))
           (options (append
                     (loop for (key val) on other-options by #'cddr
                           unless (member key '(:type :reader :writer :name :default :packed :documentation))
                             collect (make-instance 'protobuf-option
                                       :name  (slot-name->proto key)
                                       :value val))
                     (loop for (key val) on options by #'cddr
                         collect (make-instance 'protobuf-option
                                   :name  (if (symbolp key) (slot-name->proto key) key)
                                   :value val)))))
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

;; Define a service named 'type' with generic functions declared for
;; each of the methods within the service
(defmacro define-service (type (&key name options documentation)
                          &body method-specs)
  "Define a service named 'type' and Lisp 'defgeneric' for all its methods.
   'name' can be used to override the defaultly generated Protobufs service name.
   'options' is a set of keyword/value pairs, both of which are strings.

   The body is a set of method specs of the form (name (input-type output-type) &key options).
   'input-type' and 'output-type' may also be of the form (type &key name)."
  (let* ((name    (or name (class-name->proto type)))
         (options (loop for (key val) on options by #'cddr
                        collect (make-instance 'protobuf-option
                                  :name  (if (symbolp key) (slot-name->proto key) key)
                                  :value val)))
         (service (make-instance 'protobuf-service
                    :class type
                    :name  name
                    :options options
                    :documentation documentation))
         (index 0))
    (with-collectors ((forms collect-form))
      (dolist (method method-specs)
        (destructuring-bind (function (input-type output-type) &key name options documentation) method
          (let* ((input-name (and (listp input-type)
                                  (getf (cdr input-type) :name)))
                 (input-type (if (listp input-type) (car input-type) input-type))
                 (output-name (and (listp output-type)
                                   (getf (cdr output-type) :name)))
                 (output-type (if (listp output-type) (car output-type) output-type))
                 (options (loop for (key val) on options by #'cddr
                                collect (make-instance 'protobuf-option
                                          :name  (if (symbolp key) (slot-name->proto key) key)
                                          :value val)))
                 (package   *protobuf-package*)
                 (client-fn function)
                 (server-fn (intern (format nil "~A-~A" 'do function) package))
                 (method  (make-instance 'protobuf-method
                            :class function
                            :name  (or name (class-name->proto function))
                            :client-stub client-fn
                            :server-stub server-fn
                            :input-type  input-type
                            :input-name  (or input-name (class-name->proto input-type))
                            :output-type output-type
                            :output-name (or output-name (class-name->proto output-type))
                            :index (iincf index)
                            :options options
                            :documentation documentation)))
            (setf (proto-methods service) (nconc (proto-methods service) (list method)))
            ;; The following are the hooks to CL-Stubby
            (let* ((vinput    (intern (format nil "~A-~A" (symbol-name input-type) 'in) package))
                   (voutput   (intern (format nil "~A-~A" (symbol-name output-type) 'out) package))
                   (vchannel  (intern (symbol-name 'channel) package))
                   (vcallback (intern (symbol-name 'callback) package)))
              ;; The client side stub, e.g., 'read-air-reservation'.
              ;; The expectation is that CL-Stubby will provide macrology to make it
              ;; easy to implement a method for this on each kind of channel (HTTP, TCP socket,
              ;; IPC, etc). Unlike C++/Java/Python, we don't need a client-side subclass,
              ;; because we can just use multi-methods.
              ;; The CL-Stubby macros take care of serializing the input, transmitting the
              ;; request over the wire, waiting for input (or not if it's asynchronous),
              ;; filling in the output, and calling the callback (if it's asynchronous).
              ;; It's not very Lispy to side-effect an output object, but it makes
              ;; asynchronous calls simpler.
              (collect-form `(defgeneric ,client-fn (,vchannel ,vinput ,voutput &key ,vcallback)
                               ,@(and documentation `((:documentation ,documentation)))
                               #-sbcl (declare (values ,output-type))))
              ;; The server side stub, e.g., 'do-read-air-reservation'.
              ;; The expectation is that the server-side program will implement
              ;; a method with the business logic for this on each kind of channel
              ;; (HTTP, TCP socket, IPC, etc), possibly on a server-side subclass
              ;; of the input class
              ;; The business logic is expected to perform the correct operations on
              ;; the input object, which arrived via Protobufs, and produce an output
              ;; of the given type, which will be serialized as a result.
              ;; The channel objects hold client identity information, deadline info,
              ;; etc, and can be side-effected to indicate success or failure
              ;; CL-Stubby provides the channel classes and does (de)serialization, etc
              (collect-form `(defgeneric ,server-fn (,vchannel ,vinput ,voutput)
                               ,@(and documentation `((:documentation ,documentation)))
                               #-sbcl (declare (values ,output-type))))))))
      `(progn
         define-service
         ,service
         ,forms))))


;;; Ensure everything in a Protobufs schema is defined

(defvar *undefined-messages*)

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
    (ensure-type trace service method (proto-output-type method))))

;; 'message' and 'field' can be a message and a field or a service and a method
(defun ensure-type (trace message field type)
  (unless (keywordp type)
    (let ((msg (loop for p in trace
                     thereis (or (find-message p type)
                                 (find-enum p type)))))
      (unless msg
        (push (cons message field) (gethash type *undefined-messages*))))))
