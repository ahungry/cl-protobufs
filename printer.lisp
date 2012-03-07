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
   (write-protobuf-as type protobuf stream))

(defgeneric write-protobuf-as (type protobuf stream &key indentation)
  (:documentation
   "Writes the protobuf object 'protobuf' (schema, message, enum, etc) onto
    the given stream 'stream'in the format given by 'type' (:proto, :text, etc)."))


;;; Pretty print as a .proto file

(defmethod write-protobuf-as ((type (eql :proto)) (protobuf protobuf) stream
                              &key (indentation 0))
  (when (proto-syntax protobuf)
    (format stream "~&syntax = \"~A\";~%~%" (proto-syntax protobuf)))
  (when (proto-package protobuf)
    (format stream "~&package ~A;~%~%" (proto-package protobuf)))
  (when (proto-imports protobuf)
    (dolist (import (proto-imports protobuf))
      (format stream "~&import \"~A\";~%" import))
    (format stream "~%"))
  (when (proto-options protobuf)
    (dolist (option (proto-options protobuf))
      (format stream "~&option ~A;~%" option))
    (format stream "~%"))
  (dolist (enum (proto-enums protobuf))
    (write-protobuf-as type enum stream :indentation indentation)
    (terpri stream))
  (dolist (msg (proto-messages protobuf))
    (write-protobuf-as type msg stream :indentation indentation)
    (terpri stream))
  (dolist (svc (proto-services protobuf))
    (write-protobuf-as type svc stream :indentation indentation)
    (terpri stream)))


(defmethod write-protobuf-as ((type (eql :proto)) (enum protobuf-enum) stream
                              &key (indentation 0))
  (when (proto-comment enum)
    (format stream "~&~@[~VT~]// ~A~%"
            (and (not (zerop indentation)) indentation) (proto-comment enum)))
  (format stream "~&~@[~VT~]enum ~A {~%"
          (and (not (zerop indentation)) indentation) (proto-name enum))
  (dolist (value (proto-values enum))
    (write-protobuf-as type value stream :indentation (+ indentation 2)))
  (format stream "~&~@[~VT~]}~%"
          (and (not (zerop indentation)) indentation)))

(defmethod write-protobuf-as ((type (eql :proto)) (val protobuf-enum-value) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name index) (proto- val)
    (format stream "~&~@[~VT~]~A = ~D;~%"
            (and (not (zerop indentation)) indentation) name index)))


(defmethod write-protobuf-as ((type (eql :proto)) (message protobuf-message) stream
                              &key (indentation 0))
  (when (proto-comment message)
    (format stream "~&~@[~VT~]// ~A~%"
            (and (not (zerop indentation)) indentation) (proto-comment message)))
  (format stream "~&~@[~VT~]message ~A {~%"
          (and (not (zerop indentation)) indentation) (proto-name message))
  (dolist (enum (proto-enums message))
    (write-protobuf-as type enum stream :indentation (+ indentation 2)))
  (dolist (msg (proto-messages message))
    (write-protobuf-as type msg stream :indentation (+ indentation 2)))
  (dolist (field (proto-fields message))
    (write-protobuf-as type field stream :indentation (+ indentation 2)))
  (format stream "~&~@[~VT~]}~%"
          (and (not (zerop indentation)) indentation)))

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


(defmethod write-protobuf-as ((type (eql :proto)) (service protobuf-service) stream
                              &key (indentation 0))
  (when (proto-comment service)
    (format stream "~&~@[~VT~]// ~A~%"
            (and (not (zerop indentation)) indentation) (proto-comment service)))
  (format stream "~&~@[~VT~]service ~A {~%"
          (and (not (zerop indentation)) indentation) (proto-name service))
  (dolist (rpc (proto-rpcs service))
    (write-protobuf-as type rpc stream :indentation (+ indentation 2)))
  (format stream "~&~@[~VT~]}~%"
          (and (not (zerop indentation)) indentation)))

(defmethod write-protobuf-as ((type (eql :proto)) (rpc protobuf-rpc) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name input-type output-type) (proto- rpc)
    (format stream "~&~@[~VT~]rpc ~A (~@[~A~])~@[ returns (~A)~];~%"
            (and (not (zerop indentation)) indentation)
            name input-type output-type)))


;;; Pretty print as a .lisp file

(defmethod write-protobuf-as ((type (eql :lisp)) (protobuf protobuf) stream
                              &key (indentation 0))
  (declare (ignore indentation))
  (when (proto-package protobuf)
    (format stream "~&(in-package \"~A\")~%~%" (proto-package protobuf)))
  (format stream "~&(proto:define-proto ~(~A~)~%    ("
          (or (proto-class protobuf)  (proto-name protobuf)))
  (let ((spaces ""))
    (when (proto-package protobuf)
      (format stream "~A:package ~A~%" spaces (proto-package protobuf))
      (setq spaces "     "))
    (when (proto-imports protobuf)
      (cond ((= (length (proto-imports protobuf)) 1)
             (format stream "~A:import \"~A\"~%" spaces (car (proto-imports protobuf))))
            (t
             (format stream "~A:import (" spaces)
             (format stream "~{\"~A\"~^ ~}" (proto-imports protobuf))
             (format stream ")~%")))
      (setq spaces "     "))
    (when (proto-options protobuf)
      (format stream "~A:options (" spaces)
      (format stream "~{\"~A\"~^ ~}" (proto-options protobuf))
      (format stream ")~%")))
  (format stream ")~%")
  (dolist (enum (proto-enums protobuf))
    (write-protobuf-as type enum stream :indentation 2)
    (terpri stream))
  (dolist (msg (proto-messages protobuf))
    (write-protobuf-as type msg stream :indentation 2)
    (terpri stream))
  (dolist (svc (proto-services protobuf))
    (write-protobuf-as type svc stream :indentation 2)
    (terpri stream))
  (format stream ")~%"))


(defmethod write-protobuf-as ((type (eql :lisp)) (enum protobuf-enum) stream
                              &key (indentation 0))
  (when (proto-comment enum)
    (format stream "~&~@[~VT~];; ~A~%"
            (and (not (zerop indentation)) indentation) (proto-comment enum)))
  (format stream "~&~@[~VT~](proto:define-enum ~(~S~)~%~VT("
          (and (not (zerop indentation)) indentation) (proto-class enum)
          (+ indentation 4))
  (format stream ")~%")
  (dolist (value (proto-values enum))
    (write-protobuf-as type value stream :indentation (+ indentation 2)))
  (format stream "~&~@[~VT~])~%"
          (and (not (zerop indentation)) indentation)))

(defmethod write-protobuf-as ((type (eql :lisp)) (val protobuf-enum-value) stream
                              &key (indentation 0))
  (with-prefixed-accessors (value index) (proto- val)
    (format stream "~&~@[~VT~](~(~A~) ~D)~%"
            (and (not (zerop indentation)) indentation) value index)))


(defmethod write-protobuf-as ((type (eql :lisp)) (message protobuf-message) stream
                              &key (indentation 0))
  (when (proto-comment message)
    (format stream "~&~@[~VT~];; ~A~%"
            (and (not (zerop indentation)) indentation) (proto-comment message)))
  (format stream "~&~@[~VT~](proto:define-message ~(~S~)~%~VT("
          (and (not (zerop indentation)) indentation) (proto-class message)
          (+ indentation 4))
  (when (proto-conc-name message)
    (format stream ":conc-name ~(~A~)~%" (proto-conc-name message)))
  (format stream ")~%")
  (dolist (enum (proto-enums message))
    (write-protobuf-as type enum stream :indentation (+ indentation 2)))
  (dolist (msg (proto-messages message))
    (write-protobuf-as type msg stream :indentation (+ indentation 2)))
  (dolist (field (proto-fields message))
    (write-protobuf-as type field stream :indentation (+ indentation 2)))
  (format stream "~&~@[~VT~])~%"
          (and (not (zerop indentation)) indentation)))

(defparameter *protobuf-slot-comment-column* 56)
(defmethod write-protobuf-as ((type (eql :lisp)) (field protobuf-field) stream
                              &key (indentation 0))
  (with-prefixed-accessors (value type class required default comment) (proto- field)
    (let ((dflt (cond ((or (null default)
                           (and (stringp default) (string-empty-p default)))
                       nil)
                      ((member type '(:int32 :uint32 :int64 :uint64 :sint32 :sint64
                                      :fixed32 :sfixed32 :fixed64 :sfixed64
                                      :single :double))
                       (read-from-string default))
                      ((eq type :bool)
                       (if (string= default "true") t nil))
                      (t default)))
          (clss (cond ((eq required :optional)
                       `(or null ,class))
                      ((eq required :repeated)
                       `(list-of ,class))
                      (t class))))
      (format stream (if (keywordp type)
                       ;; Keyword means a primitive type, print default with ~S
                       "~&~@[~VT~](~(~S~) :type ~(~S~)~@[ :default ~S~])~:[~*~*~;~VT; ~A~]~%"
                       ;; Non-keyword means an enum type, print default with ~A
                       "~&~@[~VT~](~(~S~) :type ~(~S~)~@[ :default ~(:~A~)~])~:[~*~*~;~VT; ~A~]~%")
              (and (not (zerop indentation)) indentation)
              value clss dflt
              comment *protobuf-slot-comment-column* comment))))


(defmethod write-protobuf-as ((type (eql :lisp)) (service protobuf-service) stream
                              &key (indentation 0))
  (when (proto-comment service)
    (format stream "~&~@[~VT~];; ~A~%"
            (and (not (zerop indentation)) indentation) (proto-comment service)))
  (format stream "~&~@[~VT~](proto:define-service ~(~S~)~%~VT("
          (and (not (zerop indentation)) indentation) (proto-class service)
          (+ indentation 4))
  (format stream ")~%")
  (dolist (rpc (proto-rpcs service))
    (write-protobuf-as type rpc stream :indentation (+ indentation 2)))
  (format stream "~&~@[~VT~])~%"
          (and (not (zerop indentation)) indentation)))

(defmethod write-protobuf-as ((type (eql :lisp)) (rpc protobuf-rpc) stream
                              &key (indentation 0))
  (with-prefixed-accessors (class input-type output-type) (proto- rpc)
    (format stream "~&~@[~VT~](~(~S~) ~(~S~) ~(~S~))~%"
            (and (not (zerop indentation)) indentation)
            class input-type output-type)))
