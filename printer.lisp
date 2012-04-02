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


;;; Protobufs schema pretty printing

(defun write-protobuf (protobuf &key (stream *standard-output*) (type :proto))
  "Writes the protobuf object 'protobuf' (schema, message, enum, etc) onto
   the given stream 'stream'in the format given by 'type' (:proto, :text, etc)."
   (let ((*protobuf* protobuf))
     (write-protobuf-as type protobuf stream)))

(defgeneric write-protobuf-as (type protobuf stream &key indentation)
  (:documentation
   "Writes the protobuf object 'protobuf' (schema, message, enum, etc) onto
    the given stream 'stream' in the format given by 'type' (:proto, :text, etc)."))

(defgeneric write-protobuf-documentation (type docstring stream &key indentation)
  (:documentation
   "Writes a the docstring as a \"block comment\" onto the given stream 'stream'
    in the format given by 'type' (:proto, :text, etc)."))


;;; Pretty print a schema as a .proto file

(defmethod write-protobuf-as ((type (eql :proto)) (protobuf protobuf) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name documentation syntax package imports optimize options) (proto- protobuf)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (when syntax
      (format stream "~&syntax = \"~A\";~%~%" syntax))
    (when package
      (format stream "~&package ~A;~%~%" (substitute #\_ #\- package)))
    (when imports
      (dolist (import imports)
        (format stream "~&import \"~A\";~%" import))
      (terpri stream))
    (when optimize
      (format stream "~&option optimize_for ~A;~%~%"
              (if (eq optimize :space) "CODE_SIZE" "SPEED")))
    (when options
      (dolist (option options)
        (format stream "~&option ~:/protobuf-option/;~%" option))
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

(defmethod write-protobuf-documentation ((type (eql :proto)) docstring stream
                                         &key (indentation 0))
  (let ((lines (split-string docstring :separators '(#\newline #\return))))
    (dolist (line lines)
      (format stream "~&~@[~VT~]// ~A~%"
              (and (not (zerop indentation)) indentation) line))))


(defmethod write-protobuf-as ((type (eql :proto)) (enum protobuf-enum) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name class alias-for documentation options) (proto- enum)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]enum ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (let ((other (and class (not (string= name (class-name->proto class))) class)))
      (when other
        (format stream "~&~VToption lisp_name = \"~A:~A\";~%"
                (+ indentation 2) (package-name (symbol-package class)) (symbol-name class))))
    (when alias-for
      (format stream "~&~VToption lisp_alias = \"~A:~A\";~%"
              (+ indentation 2) (package-name (symbol-package alias-for)) (symbol-name alias-for)))
    (dolist (option options)
      (format stream "~&option ~:/protobuf-option/;~%" option))
    (dolist (value (proto-values enum))
      (write-protobuf-as type value stream :indentation (+ indentation 2)))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defparameter *protobuf-enum-comment-column* 56)
(defmethod write-protobuf-as ((type (eql :proto)) (val protobuf-enum-value) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name documentation index) (proto- val)
    (format stream "~&~@[~VT~]~A = ~D;~:[~*~*~;~VT// ~A~]~%"
            (and (not (zerop indentation)) indentation) name index
            documentation *protobuf-enum-comment-column* documentation)))


(defmethod write-protobuf-as ((type (eql :proto)) (message protobuf-message) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name class alias-for documentation options) (proto- message)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]message ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (let ((other (and class (not (string= name (class-name->proto class))) class)))
      (when other
        (format stream "~&~VToption lisp_name = \"~A:~A\";~%"
                (+ indentation 2) (package-name (symbol-package class)) (symbol-name class))))
    (when alias-for
      (format stream "~&~VToption lisp_alias = \"~A:~A\";~%"
              (+ indentation 2) (package-name (symbol-package alias-for)) (symbol-name alias-for)))
    (dolist (option options)
      (format stream "~&~VToption ~:/protobuf-option/;~%"
              (+ indentation 2) option))
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
  (with-prefixed-accessors (name documentation required index default packed) (proto- field)
    (let ((dflt (if (stringp default)
                  (if (string-empty-p default) nil default)
                  default)))
      (format stream "~&~@[~VT~]~(~A~) ~A ~A = ~D~@[ [default = ~A]~]~@[ [packed=true]~*~];~:[~*~*~;~VT// ~A~]~%"
              (and (not (zerop indentation)) indentation)
              required (proto-type field) name index dflt packed
              documentation *protobuf-field-comment-column* documentation))))

(defmethod write-protobuf-as ((type (eql :proto)) (extension protobuf-extension) stream
                              &key (indentation 0))
  (with-prefixed-accessors (from to) (proto-extension- extension)
    (format stream "~&~@[~VT~]extensions ~D to ~D;~%"
            (and (not (zerop indentation)) indentation)
            from to)))


(defmethod write-protobuf-as ((type (eql :proto)) (service protobuf-service) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name documentation) (proto- service)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]service ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (dolist (rpc (proto-rpcs service))
      (write-protobuf-as type rpc stream :indentation (+ indentation 2)))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defmethod write-protobuf-as ((type (eql :proto)) (rpc protobuf-rpc) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name documentation input-name output-name options) (proto- rpc)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]rpc ~A (~@[~A~])~@[ returns (~A)~]"
            (and (not (zerop indentation)) indentation)
            name input-name output-name)
    (cond (options
           (format stream " {~%")
           (dolist (option options)
             (format stream "~&~@[~VT~]option ~:/protobuf-option/;~%"
                     (+ indentation 2) option))
           (format stream "~@[~VT~]}"
                   (and (not (zerop indentation)) indentation)))
          (t
           (format stream ";~%")))))


;;; Pretty print a schema as a .lisp file

(defmethod write-protobuf-as ((type (eql :lisp)) (protobuf protobuf) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name class documentation package imports optimize options) (proto- protobuf)
    (when package
      (format stream "~&(in-package \"~A\")~%~%" package))
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&(proto:define-proto ~(~A~)" (or class name))
    (if (or package imports optimize options documentation)
      (format stream "~%    (")
      (format stream " ("))
    (let ((spaces ""))
      (when package
        (format stream "~A:package ~A" spaces package)
        (when (or imports optimize options documentation)
          (terpri stream))
        (setq spaces "     "))
      (when imports
        (cond ((= (length imports) 1)
               (format stream "~A:import \"~A\"" spaces (car imports)))
              (t
               (format stream "~A:import (~{\"~A\"~^ ~})" spaces imports)))
        (when (or optimize options documentation)
          (terpri stream))
        (setq spaces "     "))
      (when optimize
        (format stream "~A:optimize ~(~S~)" spaces optimize)
        (when (or options documentation)
          (terpri stream))
        (setq spaces "     "))
      (when options
        (format stream "~A:options (~{~@/protobuf-option/~^ ~})" spaces options)
        (when documentation
          (terpri stream))
        (setq spaces "     "))
      (when documentation
        (format stream "~A:documentation ~S" spaces documentation)))
    (format stream ")")
    (dolist (enum (proto-enums protobuf))
      (write-protobuf-as type enum stream :indentation 2))
    (dolist (msg (proto-messages protobuf))
      (write-protobuf-as type msg stream :indentation 2))
    (dolist (svc (proto-services protobuf))
      (write-protobuf-as type svc stream :indentation 2))
    (format stream ")~%")))

(defmethod write-protobuf-documentation ((type (eql :lisp)) docstring stream
                                         &key (indentation 0))
  (let ((lines (split-string docstring :separators '(#\newline #\return))))
    (dolist (line lines)
      (format stream "~&~@[~VT~];; ~A~%"
              (and (not (zerop indentation)) indentation) line))))


(defmethod write-protobuf-as ((type (eql :lisp)) (enum protobuf-enum) stream
                              &key (indentation 0))
  (terpri stream)
  (with-prefixed-accessors (name class alias-for documentation) (proto- enum)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~@[~VT~](proto:define-enum ~(~S~)"
            (and (not (zerop indentation)) indentation) class)
    (let ((other (and name (not (string= name (class-name->proto class))) name)))
      (cond ((or other alias-for documentation)
             (format stream "~%~@[~VT~](~:[~*~*~;:name ~(~S~)~@[~%~VT~]~]~
                                        ~:[~*~*~;:alias-for ~(~S~)~@[~%~VT~]~]~
                                        ~:[~*~;:documentation ~S~])"
                     (+ indentation 4)
                     other other (and (or alias-for documentation) (+ indentation 5))
                     alias-for alias-for (and documentation (+ indentation 5))
                     documentation documentation))
            (t
             (format stream " ()"))))
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
  (with-prefixed-accessors (name class alias-for conc-name documentation) (proto- message)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~](proto:define-message ~(~S~)"
            (and (not (zerop indentation)) indentation) class)
    (let ((other (and name (not (string= name (class-name->proto class))) name)))
      (cond ((or alias-for conc-name documentation)
             (format stream "~%~@[~VT~](~:[~*~*~;:name ~(~S~)~@[~%~VT~]~]~
                                        ~:[~*~*~;:alias-for ~(~S~)~@[~%~VT~]~]~
                                        ~:[~*~*~;:conc-name ~(~S~)~@[~%~VT~]~]~
                                        ~:[~*~;:documentation ~S~])"
                     (+ indentation 4)
                     other other (and (or alias-for conc-name documentation) (+ indentation 5))
                     alias-for alias-for (and (or documentation conc-name) (+ indentation 5))
                     conc-name conc-name (and documentation (+ indentation 5))
                     documentation documentation))
            (t
             (format stream " ()"))))
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
  (with-prefixed-accessors (value reader class documentation required default) (proto- field)
    (let ((dflt (protobuf-default-to-clos-init default class))
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
                       "~&~@[~VT~](~(~S~) :type ~(~S~)~@[ :default ~S~]~@[ :reader ~(~S~)~])~:[~*~*~;~VT; ~A~]"
                       ;; Non-keyword must mean an enum type, print default with ~A
                       "~&~@[~VT~](~(~S~) :type ~(~S~)~@[ :default ~(:~A~)~]~@[ :reader ~(~S~)~])~:[~*~*~;~VT; ~A~]")
              (and (not (zerop indentation)) indentation)
              value clss dflt reader
              documentation *protobuf-slot-comment-column* documentation))))

(defmethod write-protobuf-as ((type (eql :lisp)) (extension protobuf-extension) stream
                              &key (indentation 0))
  (with-prefixed-accessors (from to) (proto-extension- extension)
    (format stream "~&~@[~VT~](define-extension ~D ~D)"
            (and (not (zerop indentation)) indentation)
            from to)))


(defmethod write-protobuf-as ((type (eql :lisp)) (service protobuf-service) stream
                              &key (indentation 0))
  (with-prefixed-accessors (class documentation conc-name) (proto- service)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~](proto:define-service ~(~S~)"
            (and (not (zerop indentation)) indentation) (proto-class service))
    (cond (documentation
           (format stream "~%~@[~VT~](:documentation ~S)"
                   (+ indentation 4) documentation))
          (t
           (format stream " ()")))
    (loop for (rpc . more) on (proto-rpcs service) doing
      (write-protobuf-as type rpc stream :indentation (+ indentation 2))
      (when more
        (terpri stream)))
    (format stream ")")))

(defmethod write-protobuf-as ((type (eql :lisp)) (rpc protobuf-rpc) stream
                              &key (indentation 0))
  (with-prefixed-accessors
      (function documentation input-type output-type options) (proto- rpc)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~](~(~S~) (~(~S~) ~(~S~))"
            (and (not (zerop indentation)) indentation)
            function input-type output-type)
    (when options
      (format stream "~%~VT:options (~{~@/protobuf-option/~^ ~})"
              (+ indentation 2) options))
    (format stream ")")))
