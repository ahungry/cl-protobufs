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
   'syntax' and 'package' are as in .proto files.
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
    ;;--- This should warn if the old one isn't upgradable to the new one
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
         (let ((protobuf (make-instance 'protobuf
                           :name     ',pname
                           :class    ',cname
                           :package  ,(if (stringp package) package (string-downcase (string package)))
                           :imports  ',(if (listp import) import (list import))
                           :syntax   ,syntax
                           :options  (list ,@options)
                           :enums    (list ,@enums)
                           :messages (list ,@msgs)
                           :services (list ,@svcs)
                           :documentation ,documentation)))
           (setq ,vname protobuf)
           (setf (gethash ',pname *all-protobufs*) protobuf)
           (setf (gethash ',cname *all-protobufs*) protobuf)
           protobuf)))))

;; Define an enum type named 'name' and a Lisp 'deftype'
(defmacro define-enum (name (&key proto-name conc-name options documentation) &body values)
  "Define an enum type named 'name' and a Lisp 'deftype'.
  'proto-name' can be used to override the defaultly generated name.
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
    (collect-form `(deftype ,name () '(member ,@vals)))
    (let ((options (loop for (key val) on options by #'cddr
                         collect `(make-instance 'protobuf-option
                                    :name ,key
                                    :value ,val))))
      `(progn
         define-enum
         (make-instance 'protobuf-enum
           :name   ,(or proto-name (class-name->proto name))
           :class  ',name
           :options (list ,@options)
           :values  (list ,@evals)
           :documentation ,documentation)
         ,forms))))

;; Define a message named 'name' and a Lisp 'defclass'
(defmacro define-message (name (&key proto-name conc-name options documentation)
                          &body fields &environment env)
  "Define a message named 'name' and a Lisp 'defclass'.
   'proto-name' can be used to override the defaultly generated name.
   The body consists of fields, or 'define-enum' or 'define-message' forms.
   Fields take the form (name &key type default index)."
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
           (destructuring-bind (slot &key type default) fld
             (let* ((idx  (if (listp slot) (second slot) (iincf index)))
                    (slot (if (listp slot) (first slot) slot))
                    (reqd (clos-type-to-protobuf-required type))
                    (accessor (intern (if conc-name (format nil "~A~A" conc-name slot) (symbol-name slot))
                                      (symbol-package slot))))
               (multiple-value-bind (ptype pclass)
                   (clos-type-to-protobuf-type type)
                 (collect-slot `(,slot :type ,type
                                       :accessor ,accessor
                                       :initarg ,(kintern (symbol-name slot))
                                       ,@(and default (list :initform default))))
                 (collect-field `(make-instance 'protobuf-field
                                   :name  ,(slot-name->proto slot)
                                   :type  ,ptype
                                   :class ',pclass
                                   :required ,reqd
                                   :index ,idx
                                   :value ',slot
                                   :default ,(and default (format nil "~A" default))
                                   :packed  ,(and (eq reqd :repeated)
                                                  (packed-type-p pclass)))))))))))
    (collect-form `(defclass ,name () (,@slots)))
    (let ((options (loop for (key val) on options by #'cddr
                         collect `(make-instance 'protobuf-option
                                    :name ,key
                                    :value ,val))))
      `(progn
         define-message
         (make-instance 'protobuf-message
           :name  ,(or proto-name (class-name->proto name))
           :class ',name
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

;; Define a service named 'name' and a Lisp 'defun'
(defmacro define-service (name (&key proto-name options documentation) &body rpc-specs)
  "Define a service named 'name' and a Lisp 'defun'.
   'proto-name' can be used to override the defaultly generated name.
   The body consists of a set of RPC specs of the form (name input-type output-type)."
  (with-collectors ((rpcs collect-rpc))
    (dolist (rpc rpc-specs)
      (destructuring-bind (name input-type output-type &key options) rpc
        (let ((options (loop for (key val) on options by #'cddr
                             collect `(make-instance 'protobuf-option
                                        :name ,key
                                        :value ,val))))
          (collect-rpc `(make-instance 'protobuf-rpc
                          :name ,(class-name->proto name)
                          :class ',name
                          :input-type  ,(and input-type  (class-name->proto input-type))
                          :output-type ,(and output-type (class-name->proto output-type))
                          :options (list ,@options))))))
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
         ()))))                                         ;---*** define Lisp stub here
