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

(defmethod write-protobuf ((protobuf protobuf) stream &key (indentation 0))
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
    (write-protobuf enum stream :indentation indentation)
    (terpri stream))
  (dolist (msg (proto-messages protobuf))
    (write-protobuf msg stream :indentation indentation)
    (terpri stream))
  (dolist (svc (proto-services protobuf))
    (write-protobuf svc stream :indentation indentation)
    (terpri stream)))


(defmethod write-protobuf ((message protobuf-message) stream &key (indentation 0))
  (when (proto-comment message)
    (format stream "~&~@[~VT~]// ~A~%"
            (and (not (zerop indentation)) indentation) (proto-comment message)))
  (format stream "~&~@[~VT~]message ~A {~%"
          (and (not (zerop indentation)) indentation) (proto-name message))
  (dolist (enum (proto-enums message))
    (write-protobuf enum stream :indentation (+ indentation 2)))
  (dolist (msg (proto-messages message))
    (write-protobuf msg stream :indentation (+ indentation 2)))
  (dolist (field (proto-fields message))
    (write-protobuf field stream :indentation (+ indentation 2)))
  (format stream "~&~@[~VT~]}~%"
          (and (not (zerop indentation)) indentation)))

(defparameter *protobuf-field-comment-column* 56)
(defmethod write-protobuf ((field protobuf-field) stream &key (indentation 0))
  (with-prefixed-accessors (name type required index default packed comment) (proto- field)
    (let ((def (if (stringp default)
                 (if (string-empty-p default) nil default)
                 default)))
      (format stream "~&~@[~VT~]~(~A~) ~A ~A = ~D~@[ [default = ~A]~]~@[ [packed=true]~*~];~:[~*~*~;~VT// ~A~]~%"
              (and (not (zerop indentation)) indentation)
              required type name index def packed
              comment *protobuf-field-comment-column* comment))))


(defmethod write-protobuf ((enum protobuf-enum) stream &key (indentation 0))
  (when (proto-comment enum)
    (format stream "~&~@[~VT~]// ~A~%"
            (and (not (zerop indentation)) indentation) (proto-comment enum)))
  (format stream "~&~@[~VT~]enum ~A {~%"
          (and (not (zerop indentation)) indentation) (proto-name enum))
  (dolist (value (proto-values enum))
    (write-protobuf value stream :indentation (+ indentation 2)))
  (format stream "~&~@[~VT~]}~%"
          (and (not (zerop indentation)) indentation)))

(defmethod write-protobuf ((val protobuf-enum-value) stream &key (indentation 0))
  (with-prefixed-accessors (name index) (proto- val)
    (format stream "~&~@[~VT~]~A = ~D;~%"
            (and (not (zerop indentation)) indentation) name index)))


(defmethod write-protobuf ((service protobuf-service) stream &key (indentation 0))
  (when (proto-comment service)
    (format stream "~&~@[~VT~]// ~A~%"
            (and (not (zerop indentation)) indentation) (proto-comment service)))
  (format stream "~&~@[~VT~]service ~A {~%"
          (and (not (zerop indentation)) indentation) (proto-name service))
  (dolist (rpc (proto-rpcs service))
    (write-protobuf rpc stream :indentation (+ indentation 2)))
  (format stream "~&~@[~VT~]}~%"
          (and (not (zerop indentation)) indentation)))

(defmethod write-protobuf ((rpc protobuf-rpc) stream &key (indentation 0))
  (with-prefixed-accessors (name input-type output-type) (proto- rpc)
    (format stream "~&~@[~VT~]rpc ~A (~@[~A~])~@[ returns (~A)~];~%"
            (and (not (zerop indentation)) indentation)
            name input-type output-type)))
