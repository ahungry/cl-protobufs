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

;; Define a schema named 'name', corresponding to a .proto file of that name
(defmacro define-proto (name (&key proto-name syntax package import options documentation)
                        &body messages &environment env)
  "Define a schema named 'name', corresponding to a .proto file of that name.
   'proto-name' can be used to override the defaultly generated name.
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
    (let ((vname (fintern "*~A*" name))
          (pname (or proto-name (class-name->proto name)))
          (cname name)
          (options (loop for (key val) on options by #'cddr
                         collect `(make-instance 'protobuf-option
                                    :name ,key
                                    :value ,val))))
      `(progn
         ,@forms
         (defvar ,vname nil)
         (let ((old      ,vname)
               (protobuf (make-instance 'protobuf
                           :name     ',pname
                           :class    ',cname
                           :package  ,(if (stringp package) package (string-downcase (string package)))
                           :imports  ',(if (listp import) import (list import))
                           :syntax   ,(or syntax "proto2")
                           :options  (list ,@options)
                           :enums    (list ,@enums)
                           :messages (list ,@msgs)
                           :services (list ,@svcs)
                           :documentation ,documentation)))
           (when old
             (multiple-value-bind (upgradable warnings)
                 (protobuf-upgradable old protobuf)
               (unless upgradable
                 (protobufs-warn "The old schema for ~S (~A) can't be safely upgraded; proceeding anyway"
                                 ',cname ',pname)
                 (map () #'protobufs-warn warnings))))
           (setq ,vname protobuf)
           (setf (gethash ',pname *all-protobufs*) protobuf)
           (setf (gethash ',cname *all-protobufs*) protobuf)
           protobuf)))))

;; Define an enum type named 'name' and a Lisp 'deftype'
(defmacro define-enum (name (&key proto-name conc-name type options documentation)
                       &body values)
  "Define an enum type named 'name' and a Lisp 'deftype'.
   'proto-name' can be used to override the defaultly generated Protobufs name.
   'conc-name' will be used as the prefix to the Lisp enum names, if it's supplied.
   If 'type' is given, no Lisp deftype is defined. This feature is intended to be used
   to model enum types that already exist in Lisp.
   'options' is a set of keyword/value pairs, both of which are strings.
   The body consists of the enum values in the form (name &key index)."
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

    (if type
      ;; If we've got a type override, define a type matching the Lisp name
      ;; of this message so that typep and subtypep work
      (unless (eq name type)
        (collect-form `(deftype ,name () ',type)))
      ;; If no type override, define the type now
      (collect-form `(deftype ,name () '(member ,@vals))))
    (let ((options (loop for (key val) on options by #'cddr
                         collect `(make-instance 'protobuf-option
                                    :name ,key
                                    :value ,val))))
      `(progn
         define-enum
         (make-instance 'protobuf-enum
           :name   ,(or proto-name (class-name->proto name))
           :class  ',name
           :class-override ',type
           :options (list ,@options)
           :values  (list ,@evals)
           :documentation ,documentation)
         ,forms))))

;; Define a message named 'name' and a Lisp 'defclass'
(defmacro define-message (name (&key proto-name conc-name class options documentation)
                          &body fields &environment env)
  "Define a message named 'name' and a Lisp 'defclass'.
   'proto-name' can be used to override the defaultly generated Protobufs name.
   The body consists of fields, or 'define-enum' or 'define-message' forms.
   'conc-name' will be used as the prefix to the Lisp slot accessors, if it's supplied.
   If 'class' is given, no Lisp class is defined. This feature is intended to be used
   to model messages that will be serialized from existing Lisp classes; unless you
   get the slot names correct in each field, it will be the case that trying to
   deserialize into a Lisp object won't work.
   'options' is a set of keyword/value pairs, both of which are strings.
   Fields take the form (name &key type default reader)
   'name' can be either a symbol giving the field name, or a list whose
   first element is the field name and whose second element is the index."
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
           (destructuring-bind (slot &key type default reader proto-name) fld
             (let* ((idx  (if (listp slot) (second slot) (iincf index)))
                    (slot (if (listp slot) (first slot) slot))
                    (reqd (clos-type-to-protobuf-required type))
                    (accessor (intern (if conc-name (format nil "~A~A" conc-name slot) (symbol-name slot))
                                      (symbol-package slot))))
               (multiple-value-bind (ptype pclass)
                   (clos-type-to-protobuf-type type)
                 (unless class
                   (collect-slot `(,slot :type ,type
                                         :accessor ,accessor
                                         :initarg ,(kintern (symbol-name slot))
                                         ,@(and default (list :initform default)))))
                 (collect-field `(make-instance 'protobuf-field
                                   :name  ,(or proto-name (slot-name->proto slot))
                                   :type  ,ptype
                                   :class ',pclass
                                   :required ,reqd
                                   :index  ,idx
                                   :value  ',slot
                                   :reader ',reader
                                   :default ,(and default (format nil "~A" default))
                                   :packed  ,(and (eq reqd :repeated)
                                                  (packed-type-p pclass)))))))))))
    (if class
      ;; If we've got a class override, define a type matching the Lisp name
      ;; of this message so that typep and subtypep work
      (unless (eq name class)
        (collect-form `(deftype ,name () ',class)))
      ;; If no class override, define the class now
      (collect-form `(defclass ,name () (,@slots))))
    (let ((options (loop for (key val) on options by #'cddr
                         collect `(make-instance 'protobuf-option
                                    :name ,key
                                    :value ,val))))
      `(progn
         define-message
         (make-instance 'protobuf-message
           :name  ,(or proto-name (class-name->proto name))
           :class ',name
           :class-override ',class
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

;; Define a service named 'name' with generic functions declared for
;; each of the RPCs within the service
(defmacro define-service (name (&key proto-name options documentation)
                          &body rpc-specs)
  "Define a service named 'name' and a Lisp 'defgeneric'.
   'proto-name' can be used to override the defaultly generated Protobufs name.
   'options' is a set of keyword/value pairs, both of which are strings.
   The body is a set of RPC specs of the form (name (input-type output-type) &key options)."
  (with-collectors ((rpcs collect-rpc)
                    (forms collect-form))
    (dolist (rpc rpc-specs)
      (destructuring-bind (name (input-class output-class) &key options) rpc
        (let ((options (loop for (key val) on options by #'cddr
                             collect `(make-instance 'protobuf-option
                                        :name ,key
                                        :value ,val))))
          (collect-rpc `(make-instance 'protobuf-rpc
                          :name ,(class-name->proto name)
                          :class ',name
                          :input-type  ,(and input-class  (class-name->proto input-class))
                          :input-class ',input-class
                          :output-type  ,(and output-class (class-name->proto output-class))
                          :output-class ',output-class
                          :options (list ,@options)))
          ;;--- Is this really all we need as the stub for the RPC?
          (collect-form `(defgeneric ,name (,@(and input-class (list input-class)))
                           (declare (values ,output-class)))))))
    (let ((options (loop for (key val) on options by #'cddr
                         collect `(make-instance 'protobuf-option
                                    :name ,key
                                    :value ,val))))
      `(progn
         define-service
         (make-instance 'protobuf-service
           :name ,(or proto-name (class-name->proto name))
           :class ',name
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
                     thereis (or (find-message-for-class p type)
                                 (find-enum-for-type p type)))))
      (unless msg
        (push (cons message field) (gethash type *undefined-messages*))))))
