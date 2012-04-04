;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-IMPL")


;;; Protocol buffer defining macros

;; Define a schema named 'type', corresponding to a .proto file of that name
(defmacro define-proto (type (&key name syntax package import optimize options documentation)
                        &body messages &environment env)
  "Define a schema named 'type', corresponding to a .proto file of that name.
   'name' can be used to override the defaultly generated Protobufs name.
   'syntax' and 'package' are as they would be in a .proto file.
   'imports' is a list of pathname strings to be imported.
   'options' is a property list, i.e., (\"key1\" \"val1\" \"key2\" \"val2\" ...).

   The body consists of 'define-enum', 'define-message' or 'define-service' forms."
  (with-collectors ((enums collect-enum)
                    (msgs  collect-msg)
                    (svcs  collect-svc)
                    (forms collect-form))
    (dolist (msg messages)
      (assert (and (listp msg)
                   (member (car msg) '(define-enum define-message define-service))) ()
              "The body of ~S must be one of ~{~S~^ or ~}"
              'define-proto '(define-enum define-message define-service))
      ;; The macro-expander will return a form that consists
      ;; of 'progn' followed by a symbol naming what we've expanded
      ;; (define-enum, define-message, define-service), followed by
      ;; by a (optional) Lisp defining form (deftype, defclass),
      ;; followed by a form that creates the model object
      (destructuring-bind (&optional progn type model definers)
          (macroexpand-1 msg env)
        (assert (eq progn 'progn) ()
                "The macroexpansion for ~S failed" msg)
        (map () #'collect-form definers)
        (ecase type
          ((define-enum)
           (collect-enum model))
          ((define-message)
           (collect-msg model))
          ((define-service)
           (collect-svc model)))))
    (let ((var  (fintern "*~A*" type))
          (name (or name (class-name->proto type)))
          (options (loop for (key val) on options by #'cddr
                         collect `(make-instance 'protobuf-option
                                    :name ,key
                                    :value ,val))))
      `(progn
         ,@forms
         (defvar ,var nil)
         (let ((old ,var)
               (protobuf (make-instance 'protobuf
                           :class    ',type
                           :name     ',name
                           :syntax   ,(or syntax "proto2")
                           :package  ,(if (stringp package) package (string-downcase (string package)))
                           :imports  ',(if (listp import) import (list import))
                           :options  (list ,@options)
                           :optimize ,optimize
                           :enums    (list ,@enums)
                           :messages (list ,@msgs)
                           :services (list ,@svcs)
                           :documentation ,documentation)))
           (when old
             (multiple-value-bind (upgradable warnings)
                 (protobuf-upgradable old protobuf)
               (unless upgradable
                 (protobufs-warn "The old schema for ~S (~A) can't be safely upgraded; proceeding anyway"
                                 ',type ',name)
                 (map () #'protobufs-warn warnings))))
           (setq ,var protobuf)
           #+++ignore (
           ,@(when (eq optimize :speed)
               (mapcar #'generate-object-size (proto-messages protobuf)))
           ,@(when (eq optimize :speed)
               (mapcar #'generate-serializer (proto-messages protobuf)))
           ,@(when (eq optimize :speed)
               (mapcar #'generate-deserializer (proto-messages protobuf))) )
           protobuf)))))

;; Define an enum type named 'type' and a Lisp 'deftype'
(defmacro define-enum (type (&key name conc-name alias-for options documentation)
                       &body values)
  "Define an enum type named 'name' and a Lisp 'deftype'.
   'name' can be used to override the defaultly generated Protobufs enum name.
   'conc-name' will be used as the prefix to the Lisp enum names, if it's supplied.
   If 'alias-for' is given, no Lisp type is defined. Instead, the enum will be
   used as an alias for an enum type that already exists in Lisp.
   'options' is a set of keyword/value pairs, both of which are strings.

   The body consists of the enum values in the form 'name' or (name index)."
  (with-collectors ((vals  collect-val)
                    (evals collect-eval)
                    (forms collect-form))
    (let ((index 0))
      (dolist (val values)
        (let* ((idx  (if (listp val) (second val) (incf index)))
               (name (if (listp val) (first val)  val))
               (val-name  (kintern (if conc-name (format nil "~A~A" conc-name name) (symbol-name name))))
               (enum-name (if conc-name (format nil "~A~A" conc-name name) (symbol-name name))))
          (collect-val val-name)
          (collect-eval `(make-instance 'protobuf-enum-value
                           :name  ,(enum-name->proto enum-name)
                           :index ,idx
                           :value ,val-name)))))

    (if alias-for
      ;; If we've got an alias, define a a type that is the subtype of
      ;; the Lisp enum so that typep and subtypep work
      (unless (eq type alias-for)
        (collect-form `(deftype ,type () ',alias-for)))
      ;; If no alias, define the Lisp enum type now
      (collect-form `(deftype ,type () '(member ,@vals))))
    (let ((name (or name (class-name->proto type)))
          (options (loop for (key val) on options by #'cddr
                         collect `(make-instance 'protobuf-option
                                    :name ,key
                                    :value ,val))))
      `(progn
         define-enum
         (make-instance 'protobuf-enum
           :class  ',type
           :name   ',name
           :alias-for ',alias-for
           :options (list ,@options)
           :values  (list ,@evals)
           :documentation ,documentation)
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
   to be used to defined messages that will be serialized from existing Lisp classes;
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
   using 'slot-value'; this is often used when aliasing an existing class."
  (with-collectors ((enums collect-enum)
                    (msgs  collect-msg)
                    (flds  collect-field)
                    (slots collect-slot)
                    (forms collect-form))
    (let ((index 0))
      (declare (type fixnum index))
      (dolist (fld fields)
        (case (car fld)
          ((define-enum define-message define-extension)
           (destructuring-bind (&optional progn type model definers)
               (macroexpand-1 fld env)
             (assert (eq progn 'progn) ()
                     "The macroexpansion for ~S failed" fld)
             (map () #'collect-form definers)
             (ecase type
               ((define-enum)
                (collect-enum model))
               ((define-message)
                (collect-msg model))
               ((define-extension)
                (collect-msg model)))))
          (otherwise
           (when (i= index 18999)                       ;skip over the restricted range
             (setq index 19999))
           (destructuring-bind (slot &key type default reader name) fld
             (let* ((idx  (if (listp slot) (second slot) (iincf index)))
                    (slot (if (listp slot) (first slot) slot))
                    (reqd (clos-type-to-protobuf-required type))
                    (accessor (intern (if conc-name (format nil "~A~A" conc-name slot) (symbol-name slot))
                                      (symbol-package slot))))
               (multiple-value-bind (ptype pclass)
                   (clos-type-to-protobuf-type type)
                 (unless alias-for
                   (collect-slot `(,slot :type ,type
                                         :accessor ,accessor
                                         :initarg ,(kintern (symbol-name slot))
                                         ,@(cond ((and (null default) (eq reqd :repeated))
                                                  `(:initform ()))
                                                 ((and (null default) (eq reqd :optional))
                                                  `(:initform nil))
                                                 (default
                                                   `(:initform ,default))))))
                 (collect-field `(make-instance 'protobuf-field
                                   :name  ,(or name (slot-name->proto slot))
                                   :type  ,ptype
                                   :class ',pclass
                                   :required ,reqd
                                   :index  ,idx
                                   :value  ',slot
                                   :reader ',reader
                                   :default ,(and default (format nil "~A" default))
                                   :packed  ,(and (eq reqd :repeated)
                                                  (packed-type-p pclass)))))))))))
    (if alias-for
      ;; If we've got an alias, define a a type that is the subtype of
      ;; the Lisp class that typep and subtypep work
      (unless (or (eq type alias-for) (find-class type nil))
        (collect-form `(deftype ,type () ',alias-for)))
      ;; If no alias, define the class now
      (collect-form `(defclass ,type () (,@slots))))
    (let ((name (or name (class-name->proto type)))
          (options (loop for (key val) on options by #'cddr
                         collect `(make-instance 'protobuf-option
                                    :name ,key
                                    :value ,val))))
      `(progn
         define-message
         (make-instance 'protobuf-message
           :class ',type
           :name  ',name
           :alias-for ',alias-for
           :conc-name ,(and conc-name (string conc-name))
           :options  (list ,@options)
           :enums    (list ,@enums)
           :messages (list ,@msgs)
           :fields   (list ,@flds)
           :documentation ,documentation)
         ,forms))))

(defmacro define-extension (from to)
  "Define an extension range within a message.
   The \"body\" is the start and end of the range, both inclusive."
  `(progn
     define-extension
     (make-instance 'protobuf-extension
       :from ,from
       :to   ,to)
     ()))

;; Define a service named 'type' with generic functions declared for
;; each of the RPCs within the service
(defmacro define-service (type (&key name options documentation)
                          &body rpc-specs)
  "Define a service named 'type' and Lisp 'defgeneric' for all its RPCs.
   'name' can be used to override the defaultly generated Protobufs service name.
   'options' is a set of keyword/value pairs, both of which are strings.

   The body is a set of RPC specs of the form (name (input-type output-type) &key options).
   'input-type' and 'output-type' may also be of the form (type &key name)."
  (with-collectors ((rpcs collect-rpc)
                    (forms collect-form))
    (dolist (rpc rpc-specs)
      (destructuring-bind (function (input-type output-type) &key name options) rpc
        (let* ((input-name (and (listp input-type)
                                (getf (cdr input-type) :name)))
               (input-type (if (listp input-type) (car input-type) input-type))
               (output-name (and (listp output-type)
                                 (getf (cdr output-type) :name)))
               (output-type (if (listp output-type) (car output-type) output-type))
               (options (loop for (key val) on options by #'cddr
                              collect `(make-instance 'protobuf-option
                                         :name ,key
                                         :value ,val))))
          (collect-rpc `(make-instance 'protobuf-rpc
                          :class ',function
                          :name  ',(or name (class-name->proto function))
                          :input-type  ',input-type
                          :input-name  ',(or input-name (class-name->proto input-type))
                          :output-type ',output-type
                          :output-name ',(or output-name (class-name->proto output-type))
                          :options (list ,@options)))
          (let ((vcontroller (intern (symbol-name 'controller) (symbol-package function)))
                (vcallback   (intern (symbol-name 'callback) (symbol-package function))))
            ;;--- Is this really what the stub's signature should be?
            (collect-form `(defgeneric ,function (,vcontroller ,input-type &optional ,vcallback)
                             (declare (values ,output-type))))))))
    (let ((name (or name (class-name->proto type)))
          (options (loop for (key val) on options by #'cddr
                         collect `(make-instance 'protobuf-option
                                    :name ,key
                                    :value ,val))))
      `(progn
         define-service
         (make-instance 'protobuf-service
           :class ',type
           :name  ',name
           :options  (list ,@options)
           :rpcs (list ,@rpcs)
           :documentation ,documentation)
         ,forms))))


;;; Ensure everything in a Protobufs schema is defined

(defvar *undefined-messages*)

;; A very useful tool during development...
(defun ensure-all-protobufs ()
  (let ((protos (sort
                 (delete-duplicates
                  (loop for p being the hash-values of *all-protobufs*
                        collect p))
                 #'string< :key #'proto-name)))
    (mapcan #'ensure-protobuf protos)))

(defmethod ensure-protobuf ((proto protobuf))
  "Ensure that all of the types are defined in the Protobufs schema 'proto'.
   This returns two values:
    - A list whose elements are (<undefined-type> \"message:field\" ...)
    - The accumulated warnings table that has the same information as objects."
  (let ((*undefined-messages* (make-hash-table))
        (trace (list proto)))
    (map () (curry #'ensure-message trace) (proto-messages proto))
    (map () (curry #'ensure-service trace) (proto-services proto))
    (loop for type being the hash-keys of *undefined-messages*
            using (hash-value things)
          collect (list* type
                         (mapcar #'(lambda (thing)
                                     (format nil "~A:~A" (proto-name (car thing)) (proto-name (cdr thing))))
                                 things)) into warnings
          finally (return (values warnings *undefined-messages*)))))

(defmethod ensure-message (trace (message protobuf-message))
  (let ((trace (cons message trace)))
    (map () (curry #'ensure-message trace) (proto-messages message))
    (map () (curry #'ensure-field trace message) (proto-fields message))))

(defmethod ensure-field (trace message (field protobuf-field))
  (ensure-type trace message field (proto-class field)))

(defmethod ensure-service (trace (service protobuf-service))
  (map () (curry #'ensure-rpc trace service) (proto-rpcs service)))

(defmethod ensure-rpc (trace service (rpc protobuf-rpc))
  (ensure-type trace service rpc (proto-input-type rpc))
  (ensure-type trace service rpc (proto-output-type rpc)))

;; 'message' and 'field' can be a message and a field or a service and an RPC
(defun ensure-type (trace message field type)
  (unless (keywordp type)
    (let ((msg (loop for p in trace
                     thereis (or (find-message p type)
                                 (find-enum p type)))))
      (unless msg
        (push (cons message field) (gethash type *undefined-messages*))))))
