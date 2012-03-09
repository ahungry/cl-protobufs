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


;;; Protobufs pretty printing

(defun write-protobuf (protobuf &key (stream *standard-output*) (type :proto))
  "Writes the protobuf object 'protobuf' (schema, message, enum, etc) onto
   the given stream 'stream'in the format given by 'type' (:proto, :text, etc)."
   (let ((*protobuf* protobuf))
     (write-protobuf-as type protobuf stream)))

(defgeneric write-protobuf-as (type protobuf stream &key indentation)
  (:documentation
   "Writes the protobuf object 'protobuf' (schema, message, enum, etc) onto
    the given stream 'stream'in the format given by 'type' (:proto, :text, etc)."))


;;; Pretty print as a .proto file

(defmethod write-protobuf-as ((type (eql :proto)) (protobuf protobuf) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name class syntax package imports options) (proto- protobuf)
    (when syntax
      (format stream "~&syntax = \"~A\";~%~%" syntax))
    (when package
      (format stream "~&package ~A;~%~%" package))
    (when imports
      (dolist (import imports)
        (format stream "~&import \"~A\";~%" import))
      (terpri stream))
    (when options
      (dolist (option options)
        (format stream "~&option ~A~@[ = ~S~];~%" (proto-name option) (proto-value option)))
      (terpri stream))
    (dolist (enum (proto-enums protobuf))
      (write-protobuf-as type enum stream :indentation indentation)
      (terpri stream))
    (dolist (msg (proto-messages protobuf))
      (write-protobuf-as type msg stream :indentation indentation)
      (terpri stream))
    (dolist (svc (proto-services protobuf))
      (write-protobuf-as type svc stream :indentation indentation)
      (terpri stream))))


(defmethod write-protobuf-as ((type (eql :proto)) (enum protobuf-enum) stream
                              &key (indentation 0))
  (with-prefixed-accessors (comment name) (proto- enum)
    (when comment
      (format stream "~&~@[~VT~]// ~A~%"
              (and (not (zerop indentation)) indentation) comment))
    (format stream "~&~@[~VT~]enum ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (dolist (value (proto-values enum))
      (write-protobuf-as type value stream :indentation (+ indentation 2)))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defmethod write-protobuf-as ((type (eql :proto)) (val protobuf-enum-value) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name index) (proto- val)
    (format stream "~&~@[~VT~]~A = ~D;~%"
            (and (not (zerop indentation)) indentation) name index)))


(defmethod write-protobuf-as ((type (eql :proto)) (message protobuf-message) stream
                              &key (indentation 0))
  (with-prefixed-accessors (comment name) (proto- message)
    (when comment
      (format stream "~&~@[~VT~]// ~A~%"
              (and (not (zerop indentation)) indentation) comment))
    (format stream "~&~@[~VT~]message ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (dolist (enum (proto-enums message))
      (write-protobuf-as type enum stream :indentation (+ indentation 2)))
    (dolist (msg (proto-messages message))
      (write-protobuf-as type msg stream :indentation (+ indentation 2)))
    (dolist (field (proto-fields message))
      (write-protobuf-as type field stream :indentation (+ indentation 2)))
    (dolist (extension (proto-extensions message))
      (write-protobuf-as type extension stream :indentation (+ indentation 2)))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defparameter *protobuf-field-comment-column* 56)
(defmethod write-protobuf-as ((type (eql :proto)) (field protobuf-field) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name type required index default packed comment) (proto- field)
    (let ((dflt (if (stringp default)
                  (if (string-empty-p default) nil default)
                  default)))
      (format stream "~&~@[~VT~]~(~A~) ~A ~A = ~D~@[ [default = ~A]~]~@[ [packed=true]~*~];~:[~*~*~;~VT// ~A~]~%"
              (and (not (zerop indentation)) indentation)
              required type name index dflt packed
              comment *protobuf-field-comment-column* comment))))

(defmethod write-protobuf-as ((type (eql :proto)) (extension protobuf-extension) stream
                              &key (indentation 0))
  (with-prefixed-accessors (from to) (proto-extension- extension)
    (format stream "~&~@[~VT~]extensions ~D to ~D;~%"
            (and (not (zerop indentation)) indentation)
            from to)))


(defmethod write-protobuf-as ((type (eql :proto)) (service protobuf-service) stream
                              &key (indentation 0))
  (with-prefixed-accessors (comment name) (proto- service)
    (when comment
      (format stream "~&~@[~VT~]// ~A~%"
              (and (not (zerop indentation)) indentation) comment))
    (format stream "~&~@[~VT~]service ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (dolist (rpc (proto-rpcs service))
      (write-protobuf-as type rpc stream :indentation (+ indentation 2)))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defmethod write-protobuf-as ((type (eql :proto)) (rpc protobuf-rpc) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name input-type output-type options) (proto- rpc)
    (format stream "~&~@[~VT~]rpc ~A (~@[~A~])~@[ returns (~A)~]"
            (and (not (zerop indentation)) indentation)
            name input-type output-type)
    (cond (options
           (format stream " {~%")
           (dolist (option options)
             (format stream "~&~@[~VT~]option ~A~@[ = ~S~];~%"
                     (+ indentation 2)
                     (proto-name option) (proto-value option)))
           (format stream "~@[~VT~]}"
                   (and (not (zerop indentation)) indentation)))
          (t
           (format stream ";~%")))))


;;; Pretty print as a .lisp file

(defmethod write-protobuf-as ((type (eql :lisp)) (protobuf protobuf) stream
                              &key (indentation 0))
  (declare (ignore indentation))
  (with-prefixed-accessors (name class package imports options) (proto- protobuf)
    (when package
      (format stream "~&(in-package \"~A\")~%~%" package))
    (format stream "~&(proto:define-proto ~(~A~)" (or class name))
    (if (or package imports options)
      (format stream "~%    (")
      (format stream " ("))
    (let ((spaces ""))
      (when package
        (format stream "~A:package ~A" spaces package)
        (when (or imports options)
          (terpri stream))
        (setq spaces "     "))
      (when imports
        (cond ((= (length imports) 1)
               (format stream "~A:import \"~A\"" spaces (car imports)))
              (t
               (format stream "~A:import (" spaces)
               (format stream "~{\"~A\"~^ ~}" imports)
               (format stream ")")))
        (when options
          (terpri stream))
        (setq spaces "     "))
      (when options
        (format stream "~A:options (" spaces)
        (format stream "~{~/protobuf-option/~^ ~}" options)
        (format stream ")~%"))))
  (format stream ")")
  (dolist (enum (proto-enums protobuf))
    (write-protobuf-as type enum stream :indentation 2))
  (dolist (msg (proto-messages protobuf))
    (write-protobuf-as type msg stream :indentation 2))
  (dolist (svc (proto-services protobuf))
    (write-protobuf-as type svc stream :indentation 2))
  (format stream ")~%"))


(defmethod write-protobuf-as ((type (eql :lisp)) (enum protobuf-enum) stream
                              &key (indentation 0))
  (terpri stream)
  (with-prefixed-accessors (comment class) (proto- enum)
    (when comment
      (format stream "~@[~VT~];; ~A~%"
              (and (not (zerop indentation)) indentation) comment))
    (format stream "~@[~VT~](proto:define-enum ~(~S~) ()"
            (and (not (zerop indentation)) indentation) class)
    (loop for (value . more) on (proto-values enum) doing
      (write-protobuf-as type value stream :indentation (+ indentation 2))
      (when more
        (terpri stream)))
    (format stream ")")))

(defmethod write-protobuf-as ((type (eql :lisp)) (val protobuf-enum-value) stream
                              &key (indentation 0))
  (with-prefixed-accessors (value index) (proto- val)
    (format stream "~&~@[~VT~](~(~A~) ~D)"
            (and (not (zerop indentation)) indentation) value index)))


(defmethod write-protobuf-as ((type (eql :lisp)) (message protobuf-message) stream
                              &key (indentation 0))
  (with-prefixed-accessors (comment class conc-name) (proto- message)
    (when comment
      (format stream "~&~@[~VT~];; ~A~%"
              (and (not (zerop indentation)) indentation) comment))
    (format stream "~&~@[~VT~](proto:define-message ~(~S~)"
            (and (not (zerop indentation)) indentation) class)
    (if conc-name
      (format stream "~%~VT(" (+ indentation 4))
      (format stream " ("))
    (when conc-name
      (format stream ":conc-name ~(~A~)" conc-name))
    (format stream ")")
    (loop for (enum . more) on (proto-enums message) doing
      (write-protobuf-as type enum stream :indentation (+ indentation 2))
      (when more
        (terpri stream)))
    (loop for (msg . more) on (proto-messages message) doing
      (write-protobuf-as type msg stream :indentation (+ indentation 2))
      (when more
        (terpri stream)))
    (loop for (field . more) on (proto-fields message) doing
      (write-protobuf-as type field stream :indentation (+ indentation 2))
      (when more
        (terpri stream)))
    (loop for (extension . more) on (proto-extensions message) doing
      (write-protobuf-as type extension stream :indentation (+ indentation 2))
      (when more
        (terpri stream)))
    (format stream ")")))

(defparameter *protobuf-slot-comment-column* 56)
(defmethod write-protobuf-as ((type (eql :lisp)) (field protobuf-field) stream
                              &key (indentation 0))
  (with-prefixed-accessors (value type class required default comment) (proto- field)
    (let ((dflt (cond ((or (null default)
                           (and (stringp default) (string-empty-p default)))
                       nil)
                      ((member class '(:int32 :uint32 :int64 :uint64 :sint32 :sint64
                                       :fixed32 :sfixed32 :fixed64 :sfixed64
                                       :single :double))
                       (read-from-string default))
                      ((eq class :bool)
                       (if (string= default "true") t nil))
                      (t default)))
          (clss (let ((cl (case class
                            ((:int32 :uint32 :int64 :uint64 :sint32 :sint64
                              :fixed32 :sfixed32 :fixed64 :sfixed64) 'integer)
                            ((:single) 'float)
                            ((:double) 'double-float)
                            ((:bool)   'boolean)
                            ((:string) 'string)
                            ((:symbol) 'symbol)
                            (otherwise class))))
                  (cond ((eq required :optional)
                         `(or null ,cl))
                        ((eq required :repeated)
                         `(list-of ,cl))
                        (t cl)))))
      (format stream (if (keywordp class)
                       ;; Keyword means a primitive type, print default with ~S
                       "~&~@[~VT~](~(~S~) :type ~(~S~)~@[ :default ~S~])~:[~*~*~;~VT; ~A~]"
                       ;; Non-keyword means an enum type, print default with ~A
                       "~&~@[~VT~](~(~S~) :type ~(~S~)~@[ :default ~(:~A~)~])~:[~*~*~;~VT; ~A~]")
              (and (not (zerop indentation)) indentation)
              value clss dflt
              comment *protobuf-slot-comment-column* comment))))

(defmethod write-protobuf-as ((type (eql :lisp)) (extension protobuf-extension) stream
                              &key (indentation 0))
  (with-prefixed-accessors (from to) (proto-extension- extension)
    (format stream "~&~@[~VT~](define-extension ~D ~D)"
            (and (not (zerop indentation)) indentation)
            from to)))


(defmethod write-protobuf-as ((type (eql :lisp)) (service protobuf-service) stream
                              &key (indentation 0))
  (with-prefixed-accessors (comment class conc-name) (proto- service)
    (when comment
      (format stream "~&~@[~VT~];; ~A~%"
              (and (not (zerop indentation)) indentation) comment))
    (format stream "~&~@[~VT~](proto:define-service ~(~S~) ()"
            (and (not (zerop indentation)) indentation) (proto-class service))
    (loop for (rpc . more) on (proto-rpcs service) doing
      (write-protobuf-as type rpc stream :indentation (+ indentation 2))
      (when more
        (terpri stream)))
    (format stream ")")))

(defmethod write-protobuf-as ((type (eql :lisp)) (rpc protobuf-rpc) stream
                              &key (indentation 0))
  (with-prefixed-accessors (class input-type output-type options) (proto- rpc)
    (let ((in  (find-message-for-class *protobuf* input-type))
          (out (find-message-for-class *protobuf* output-type)))
      (format stream "~&~@[~VT~](~(~S~) ~(~S~) ~(~S~)"
              (and (not (zerop indentation)) indentation) class
              (if in  (proto-class in)  input-type)
              (if out (proto-class out) output-type))
      (cond (options
             (format stream "~%~VT:options ("
                     (+ indentation 3))
             (loop for (option . more) on options doing
               (format stream "~S ~S"
                       (proto-name option) (proto-value option))
               (when more
                 (format stream " ")))
             (format stream "))"))
            (t
             (format stream ")"))))))
