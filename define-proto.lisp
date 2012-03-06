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
;; 'proto-name' can be used to override the defaultly generated name
;; 'package' and 'imports' are as in .proto files
;; The body consists of 'define-enum', 'define-message' or 'define-service' forms
(defmacro define-proto (name (&key proto-name package import syntax options)
                        &body messages &environment env)
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
    (let ((sname   (fintern "*~A*" name)))
      `(progn
         ,@forms
         (defvar ,sname (make-instance 'protobuf
                          :name     ,(or proto-name (proto-class-name name))
                          :package  ,(if (stringp package) package (string-downcase (string package)))
                          :imports  ',(if (consp import) import (list import))
                          :syntax   ,syntax
                          :options  '(,@options)
                          :enums    (list ,@enums)
                          :messages (list ,@msgs)
                          :services (list ,@svcs)))))))

;; Define an enum type named 'name' and a Lisp 'deftype'
;; 'proto-name' can be used to override the defaultly generated name
;; The body consists of the enum values in the form (name &key index)
(defmacro define-enum (name (&key proto-name) &body values)
  (with-collectors ((vals  collect-val)
                    (evals collect-eval)
                    (forms collect-form))
    (let ((index 0))
      (dolist (val values)
        (destructuring-bind (name)
            (if (listp val) val (list val))             ;---*** &KEY INDEX?
          (let ((lname (kintern (symbol-name name))))
            (collect-val lname)
            (collect-eval `(make-instance 'protobuf-enum-value
                             :name  ,(proto-enum-name name)
                             :index ,(incf index)
                             :value ,lname))))))
    (collect-form `(deftype ,name () '(member ,@vals)))
    `(progn
       define-enum
       (make-instance 'protobuf-enum
         :name   ,(or proto-name (proto-class-name name))
         :class  ',name
         :values (list ,@evals))
       ,forms)))

;; Define a message named 'name' and a Lisp 'defclass'
;; 'proto-name' can be used to override the defaultly generated name
;; The body consists of fields, or 'define-enum' or 'define-message' forms
;; Fields take the form (name &key type default index)
(defmacro define-message (name (&key proto-name) &body fields &environment env)
  (with-collectors ((enums collect-enum)
                    (msgs  collect-msg)
                    (flds  collect-field)
                    (slots collect-slot)
                    (forms collect-form))
    (let ((index 0))
      (dolist (fld fields)
        (case (car fld)
          ((define-enum define-message)
           (destructuring-bind (&optional progn type model definers)
               (macroexpand-1 fld env)
             (assert (eq progn 'progn) ()
                     "The macroexpansion for ~S failed" fld)
             (map () #'collect-form definers)
             (ecase type
               ((define-enum)
                (collect-enum model))
               ((define-message)
                (collect-msg model)))))
          (otherwise
           (destructuring-bind (slot &key type default) fld ;---*** &KEY INDEX?
             (multiple-value-bind (ptype pclass)
                 (clos-type-to-protobuf-type type)
               (collect-slot `(,slot :type ,type
                                     :ACCESSOR ,SLOT    ;---*** BETTER ACCESSOR NAME VIA :CONC-NAME
                                     :initarg ,(kintern (symbol-name slot))
                                     ,@(and default (list :initform default))))
               (collect-field `(make-instance 'protobuf-field
                                 :name  ,(proto-field-name slot)
                                 :type  ,ptype
                                 :class ',pclass
                                 :required ,(clos-type-to-protobuf-required type)
                                 :index ,(incf index)
                                 :value ',slot
                                 :default ,(and default (FORMAT NIL "~A" DEFAULT))      ;---***
                                 :packed  ,(packed-type-p pclass)))))))))
    (collect-form `(defclass ,name () (,@slots)))
    `(progn
       define-message
       (make-instance 'protobuf-message
         :name  ,(or proto-name (proto-class-name name))
         :class ',name
         :enums    (list ,@enums)
         :messages (list ,@msgs)
         :fields   (list ,@flds))
       ,forms)))

;; Define a service named 'name' and a Lisp 'defun'
;; 'proto-name' can be used to override the defaultly generated name
;; The body consists of a set of RPC specs of the form (name input-type output-type)
(defmacro define-service (name (&key proto-name) &body rpc-specs)
  (with-collectors ((rpcs collect-rpc))
    (dolist (rpc rpc-specs)
      (destructuring-bind (name input-type output-type) rpc
        (collect-rpc `(make-instance 'protobuf-rpc
                        :name ,(proto-class-name name)
                        :input-type  ,(and input-type  (proto-class-name input-type))
                        :output-type ,(and output-type (proto-class-name output-type))))))
    `(progn
       define-service
       (make-instance 'protobuf-service
         :name ,(or proto-name (proto-class-name name))
         :rpcs (list ,@rpcs))
       ())))                                            ;---*** DEFINE LISP STUB HERE
