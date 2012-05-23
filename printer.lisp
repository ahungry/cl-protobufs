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

(defun write-schema (protobuf &rest keys
                     &key (stream *standard-output*) (type :proto) &allow-other-keys)
  "Writes the object 'protobuf' (schema, message, enum, etc) onto the
   stream 'stream'in the format given by 'type' (:proto, :text, etc)."
   (let ((*protobuf* protobuf))
     (apply #'write-schema-as type protobuf stream keys)))

(defgeneric write-schema-as (type protobuf stream &key indentation &allow-other-keys)
  (:documentation
   "Writes the protobuf object 'protobuf' (schema, message, enum, etc) onto
    the given stream 'stream' in the format given by 'type' (:proto, :text, etc).
    If 'more' is true, this means there are more enum values, fields, etc to
    be written after the current one."))

(defgeneric write-schema-documentation (type docstring stream &key indentation)
  (:documentation
   "Writes the docstring as a \"block comment\" onto the given stream 'stream'
    in the format given by 'type' (:proto, :text, etc)."))


;;; Pretty print a schema as a .proto file

(defmethod write-schema-as ((type (eql :proto)) (schema protobuf-schema) stream
                            &key (indentation 0))
  (with-prefixed-accessors (name documentation syntax package imports options) (proto- schema)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (when syntax
      (format stream "~&syntax = \"~A\";~%~%" syntax))
    (when package
      (format stream "~&package ~A;~%~%" (substitute #\_ #\- package)))
    (when imports
      (dolist (import imports)
        (format stream "~&import \"~A\";~%" import))
      (terpri stream))
    (write-schema-header type schema stream)
    (when options
      (dolist (option options)
        (format stream "~&option ~:/protobuf-option/;~%" option))
      (terpri stream))
    (loop for (enum . more) on (proto-enums schema) doing
      (write-schema-as type enum stream :indentation indentation :more more)
      (terpri stream))
    (loop for (msg . more) on (proto-messages schema) doing
      (write-schema-as type msg stream :indentation indentation :more more)
      (terpri stream))
    (loop for (svc . more) on (proto-services schema) doing
      (write-schema-as type svc stream :indentation indentation :more more)
      (terpri stream))))

(defmethod write-schema-documentation ((type (eql :proto)) docstring stream
                                       &key (indentation 0))
  (let ((lines (split-string docstring :separators '(#\newline #\return))))
    (dolist (line lines)
      (format stream "~&~@[~VT~]// ~A~%"
              (and (not (zerop indentation)) indentation) line))))

;; Lisp was born in 1958 :-)
(defvar *lisp-options* '(("lisp_package" string 195801)
                         ("lisp_name"    string 195802)
                         ("lisp_alias"   string 195803)
                         ("lisp_type"    string 195804)
                         ("lisp_class"   string 195805)
                         ("lisp_slot"    string 195806)))

(defvar *option-types* '(("optimize_for"          symbol)
                         ("deprecated"            symbol)
                         ("cc_generic_services"   symbol)
                         ("java_generic_services" symbol)
                         ("py_generic_services"   symbol)
                         ("ctype"                 symbol)))

(defmethod write-schema-header ((type (eql :proto)) (schema protobuf-schema) stream)
  (when (any-lisp-option schema)
    (format stream "~&import \"net/proto2/proto/descriptor.proto\";~%~%")
    (format stream "~&extend proto2.MessageOptions {~%")
    (loop for (option type index) in *lisp-options* doing
      (format stream "~&  optional ~(~A~) ~A = ~D;~%" type option index))
    (format stream "~&}~%~%")))

(defmethod any-lisp-option ((schema protobuf-schema))
  (labels ((find-one (protobuf)
             (dolist (enum (proto-enums protobuf))
               (with-prefixed-accessors (name class alias-for) (proto- enum)
                 (when (or alias-for
                           (and class (not (string-equal name (class-name->proto class))) class))
                   (return-from any-lisp-option t))))
             (dolist (msg (proto-messages protobuf))
               (with-prefixed-accessors (name class alias-for) (proto- msg)
                 (when (or alias-for
                           (and class (not (string-equal name (class-name->proto class))) class))
                   (return-from any-lisp-option t))))
             (map () #'find-one (proto-messages protobuf))))
    (find-one schema)
    nil))

(defun cl-user::protobuf-option (stream option colon-p atsign-p)
  (let ((type (or (second (find (proto-name option) *option-types* :key #'first :test #'string=))
                  (proto-type option))))
    (cond (colon-p                              ;~:/protobuf-option/ -- .proto format
           (let ((fmt-control
                  (cond ((find (proto-name option) *lisp-options* :key #'first :test #'string=)
                         (if (eq type 'symbol) "(~A)~@[ = ~A~]" "(~A)~@[ = ~S~]"))
                        (t
                         (if (eq type 'symbol) "~A~@[ = ~A~]" "~A~@[ = ~S~]")))))
             (format stream fmt-control (proto-name option) (proto-value option))))
          (atsign-p                             ;~@/protobuf-option/ -- .lisp format
           (let ((fmt-control (if (eq type 'symbol) "~(~S~) ~A" "~(~S~) ~S")))
             (format stream fmt-control (proto-name option) (proto-value option))))
          (t                                    ;~/protobuf-option/  -- keyword/value format
           (let ((fmt-control (if (eq type 'symbol) "~(:~A~) ~A" "~(:~A~) ~S")))
             (format stream fmt-control (proto-name option) (proto-value option)))))))

(defmethod write-schema-as ((type (eql :proto)) (enum protobuf-enum) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name class alias-for documentation options) (proto- enum)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]enum ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (let ((other (and class (not (string-equal name (class-name->proto class))) class)))
      (when other
        (format stream "~&~VToption (lisp_name) = \"~A:~A\";~%"
                (+ indentation 2) (package-name (symbol-package class)) (symbol-name class))))
    (when alias-for
      (format stream "~&~VToption (lisp_alias) = \"~A:~A\";~%"
              (+ indentation 2) (package-name (symbol-package alias-for)) (symbol-name alias-for)))
    (dolist (option options)
      (format stream "~&option ~:/protobuf-option/;~%" option))
    (loop for (value . more) on (proto-values enum) doing
      (write-schema-as type value stream :indentation (+ indentation 2) :more more))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defparameter *protobuf-enum-comment-column* 56)
(defmethod write-schema-as ((type (eql :proto)) (val protobuf-enum-value) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation index) (proto- val)
    (format stream "~&~@[~VT~]~A = ~D;~:[~*~*~;~VT// ~A~]~%"
            (and (not (zerop indentation)) indentation) name index
            documentation *protobuf-enum-comment-column* documentation)))


(defmethod write-schema-as ((type (eql :proto)) (message protobuf-message) stream
                            &key (indentation 0) more index arity)
  (declare (ignore more arity))
  (with-prefixed-accessors (name class alias-for message-type documentation options) (proto- message)
    (cond ((eq message-type :group)
           ;; If we've got a group, the printer for fields has already
           ;; printed a partial line (nice modularity, huh?)
           (format stream "group ~A = ~D {~%" name index)
           (let ((other (and class (not (string-equal name (class-name->proto class))) class)))
             (when other
               (format stream "~&~VToption (lisp_name) = \"~A:~A\";~%"
                       (+ indentation 2) (package-name (symbol-package class)) (symbol-name class))))
           (when alias-for
             (format stream "~&~VToption (lisp_alias) = \"~A:~A\";~%"
                     (+ indentation 2) (package-name (symbol-package alias-for)) (symbol-name alias-for)))
           (dolist (option options)
             (format stream "~&~VToption ~:/protobuf-option/;~%"
                     (+ indentation 2) option))
           (loop for (enum . more) on (proto-enums message) doing
             (write-schema-as type enum stream :indentation (+ indentation 2) :more more))
           (loop for (field . more) on (proto-fields message) doing
             (write-schema-as type field stream
                              :indentation (+ indentation 2) :more more :message message))
           (format stream "~&~@[~VT~]}~%"
                   (and (not (zerop indentation)) indentation)))
          (t
           (when documentation
             (write-schema-documentation type documentation stream :indentation indentation))
           (format stream "~&~@[~VT~]~A ~A {~%"
                   (and (not (zerop indentation)) indentation)
                   (if (eq message-type :message) "message" "extend") name)
           (let ((other (and class (not (string-equal name (class-name->proto class))) class)))
             (when other
               (format stream "~&~VToption (lisp_name) = \"~A:~A\";~%"
                       (+ indentation 2) (package-name (symbol-package class)) (symbol-name class))))
           (when alias-for
             (format stream "~&~VToption (lisp_alias) = \"~A:~A\";~%"
                     (+ indentation 2) (package-name (symbol-package alias-for)) (symbol-name alias-for)))
           (dolist (option options)
             (format stream "~&~VToption ~:/protobuf-option/;~%"
                     (+ indentation 2) option))
           (cond ((eq message-type :extends)
                  (loop for (field . more) on (proto-extended-fields message) doing
                    (write-schema-as type field stream
                                     :indentation (+ indentation 2) :more more
                                     :message message)))
                 (t
                  (loop for (enum . more) on (proto-enums message) doing
                    (write-schema-as type enum stream :indentation (+ indentation 2) :more more))
                  (loop for (msg . more) on (proto-messages message) doing
                    (unless (eq (proto-message-type msg) :group)
                      (write-schema-as type msg stream :indentation (+ indentation 2) :more more)))
                  (loop for (field . more) on (proto-fields message) doing
                    (write-schema-as type field stream
                                     :indentation (+ indentation 2) :more more
                                     :message message))
                  (loop for (extension . more) on (proto-extensions message) doing
                    (write-schema-as type extension stream :indentation (+ indentation 2) :more more))))
           (format stream "~&~@[~VT~]}~%"
                   (and (not (zerop indentation)) indentation))))))

(defparameter *protobuf-field-comment-column* 56)
(defmethod write-schema-as ((type (eql :proto)) (field protobuf-field) stream
                            &key (indentation 0) more message)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation required type index packed options) (proto- field)
    (let* ((class (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
           (msg   (and (not (keywordp class))
                       (or (find-message message class) (find-enum message class)))))
      (cond ((and (typep msg 'protobuf-message)
                  (eq (proto-message-type msg) :group))
             (format stream "~&~@[~VT~]~(~A~) "
                     (and (not (zerop indentation)) indentation) required)
             (write-schema-as :proto msg stream :indentation indentation :index index :arity required))
            (t
             (let* ((defaultp (not (empty-default-p field)))
                    (default  (proto-default field))
                    (default  (and defaultp
                                   (cond ((and (typep msg 'protobuf-enum)
                                               (or (stringp default) (symbolp default)))
                                          (let ((e (find default (proto-values msg)
                                                         :key #'proto-name :test #'string=)))
                                            (and e (proto-name e))))
                                         ((eq class :bool)
                                          (if (boolean-true-p default) "true" "false"))
                                         (t default))))
                    (default  (and defaultp
                                   (if (stringp default) (escape-string default) default))))
               (format stream (if (and (keywordp class) (not (eq class :bool)))
                                ;; Keyword class means a primitive type, print default with ~S
                                "~&~@[~VT~]~(~A~) ~A ~A = ~D~
                                 ~:[~*~; [default = ~S]~]~@[ [packed = true]~*~]~{ [~:/protobuf-option/]~};~
                                 ~:[~*~*~;~VT// ~A~]~%"
                                ;; Non-keyword class means an enum type, print default with ~A"
                                "~&~@[~VT~]~(~A~) ~A ~A = ~D~
                                 ~:[~*~; [default = ~A]~]~@[ [packed = true]~*~]~{ [~:/protobuf-option/]~};~
                                 ~:[~*~*~;~VT// ~A~]~%")
                       (and (not (zerop indentation)) indentation)
                       required type name index defaultp default packed options
                       documentation *protobuf-field-comment-column* documentation)))))))

(defun escape-string (string)
  (if (every #'(lambda (ch) (and (standard-char-p ch) (graphic-char-p ch))) string)
    string
    (with-output-to-string (s)
      (loop for ch across string
            as esc = (escape-char ch)
            do (format s "~A" esc)))))

(defmethod write-schema-as ((type (eql :proto)) (extension protobuf-extension) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (from to) (proto-extension- extension)
    (format stream "~&~@[~VT~]extensions ~D~:[~*~; to ~D~];~%"
            (and (not (zerop indentation)) indentation)
            from (not (eql from to)) (if (eql to #.(1- (ash 1 29))) "max" to))))


(defmethod write-schema-as ((type (eql :proto)) (service protobuf-service) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation) (proto- service)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]service ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (loop for (method . more) on (proto-methods service) doing
      (write-schema-as type method stream :indentation (+ indentation 2) :more more))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defmethod write-schema-as ((type (eql :proto)) (method protobuf-method) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation input-name output-name options) (proto- method)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
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

(defvar *show-lisp-enum-indexes* t)
(defvar *show-lisp-field-indexes* t)

(defmethod write-schema-as ((type (eql :lisp)) (schema protobuf-schema) stream
                            &key (indentation 0)
                                 (show-field-indexes *show-lisp-field-indexes*)
                                 (show-enum-indexes *show-lisp-enum-indexes*))
  (with-prefixed-accessors (name class documentation package lisp-package imports) (proto- schema)
    (let* ((optimize (let ((opt (find-option schema "optimize_for")))
                       (and opt (cond ((string= opt "SPEED") :speed)
                                      ((string= opt "CODE_SIZE") :space)
                                      (t nil)))))
           (options  (remove-if #'(lambda (x) (string= (proto-name x) "optimize_for"))
                                (proto-options schema)))
           (pkg      (and package (if (stringp package) package (string package))))
           (lisp-pkg (and lisp-package (if (stringp lisp-package) lisp-package (string lisp-package))))
           (*show-lisp-enum-indexes* show-enum-indexes)
           (*show-lisp-field-indexes* show-field-indexes)
           (*protobuf-package* (or (find-package lisp-pkg)
                                   (find-package (string-upcase lisp-pkg))
                                   *package*))
           (*package* *protobuf-package*))
      (when (or lisp-pkg pkg)
        (let ((pkg (string-upcase (or lisp-pkg pkg))))
          (format stream "~&(eval-when (:execute :compile-toplevel :load-toplevel) ~
                          ~%  (unless (find-package \"~A\") ~
                          ~%    (defpackage ~A (:use :COMMON-LISP :PROTOBUFS)))) ~
                          ~%(in-package \"~A\")~%~%"
                  pkg pkg pkg)))
      (when documentation
        (write-schema-documentation type documentation stream :indentation indentation))
      (format stream "~&(proto:define-schema ~(~A~)" (or class name))
      (if (or pkg lisp-pkg imports optimize options documentation)
        (format stream "~%    (")
        (format stream " ("))
      (let ((spaces ""))
        (when pkg
          (format stream "~A:package \"~A\"" spaces pkg)
          (when (or lisp-pkg imports optimize options documentation)
            (terpri stream))
          (setq spaces "     "))
        (when lisp-pkg
          (format stream "~A:lisp-package \"~A\"" spaces lisp-pkg)
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
      (loop for (enum . more) on (proto-enums schema) doing
        (write-schema-as type enum stream :indentation 2 :more more))
      (loop for (msg . more) on (proto-messages schema) doing
        (write-schema-as type msg stream :indentation 2 :more more))
      (loop for (svc . more) on (proto-services schema) doing
        (write-schema-as type svc stream :indentation 2 :more more)))
    (format stream ")~%")))

(defmethod write-schema-documentation ((type (eql :lisp)) docstring stream
                                       &key (indentation 0))
  (let ((lines (split-string docstring :separators '(#\newline #\return))))
    (dolist (line lines)
      (format stream "~&~@[~VT~];; ~A~%"
              (and (not (zerop indentation)) indentation) line))))

(defmethod write-schema-header ((type (eql :lisp)) (schema protobuf-schema) stream)
  (declare (ignorable type stream))
  nil)

(defmethod write-schema-as ((type (eql :lisp)) (enum protobuf-enum) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (terpri stream)
  (with-prefixed-accessors (name class alias-for documentation) (proto- enum)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~@[~VT~](proto:define-enum ~(~S~)"
            (and (not (zerop indentation)) indentation) class)
    (let ((other (and name (not (string-equal name (class-name->proto class))) name)))
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
      (write-schema-as type value stream :indentation (+ indentation 2) :more more)
      (when more
        (terpri stream)))
    (format stream ")")))

(defmethod write-schema-as ((type (eql :lisp)) (val protobuf-enum-value) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (value index) (proto- val)
    (if *show-lisp-enum-indexes*
      (format stream "~&~@[~VT~](~(~A~) ~D)"
              (and (not (zerop indentation)) indentation) value index)
      (format stream "~&~@[~VT~]~(~A~)"
              (and (not (zerop indentation)) indentation) value))))


(defmethod write-schema-as ((type (eql :lisp)) (message protobuf-message) stream
                            &key (indentation 0) more index arity)
  (declare (ignore more))
  (with-prefixed-accessors (name class alias-for conc-name message-type documentation) (proto- message)
    (cond ((eq message-type :group)
           (when documentation
             (write-schema-documentation type documentation stream :indentation indentation))
           (format stream "~&~@[~VT~](proto:define-group ~(~S~)"
                   (and (not (zerop indentation)) indentation) class)
           (let ((other (and name (not (string-equal name (class-name->proto class))) name)))
             (format stream "~%~@[~VT~](:index ~D~@[~%~VT~]~
                                        :arity ~(~S~)~@[~%~VT~]~
                                        ~:[~*~*~;:name ~(~S~)~@[~%~VT~]~]~
                                        ~:[~*~*~;:alias-for ~(~S~)~@[~%~VT~]~]~
                                        ~:[~*~*~;:conc-name ~(~S~)~@[~%~VT~]~]~
                                        ~:[~*~;:documentation ~S~])"
                     (+ indentation 4)
                     index (+ indentation 5)
                     arity (and (or other alias-for conc-name documentation) (+ indentation 5))
                     other other (and (or alias-for conc-name documentation) (+ indentation 5))
                     alias-for alias-for (and (or documentation conc-name) (+ indentation 5))
                     conc-name conc-name (and documentation (+ indentation 5))
                     documentation documentation))
           (loop for (enum . more) on (proto-enums message) doing
             (write-schema-as type enum stream :indentation (+ indentation 2) :more more)
             (when more
               (terpri stream)))
           (loop for (field . more) on (proto-fields message) doing
             (write-schema-as type field stream
                              :indentation (+ indentation 2) :more more
                              :message message)
             (when more
               (terpri stream))))
          (t
           (when documentation
             (write-schema-documentation type documentation stream :indentation indentation))
           (format stream "~&~@[~VT~](proto:define-~A ~(~S~)"
                   (and (not (zerop indentation)) indentation)
                   (if (eq message-type :message) "message" "extend") class)
           (let ((other (and name (not (string-equal name (class-name->proto class))) name)))
             (cond ((eq message-type :extends)
                    (format stream " ()"))
                   ((or other alias-for conc-name documentation)
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
           (cond ((eq message-type :extends)
                  (loop for (field . more) on (proto-extended-fields message) doing
                    (write-schema-as type field stream
                                     :indentation (+ indentation 2) :more more
                                     :message message)
                    (when more
                      (terpri stream))))
                 (t
                  (loop for (enum . more) on (proto-enums message) doing
                    (write-schema-as type enum stream :indentation (+ indentation 2) :more more)
                    (when more
                      (terpri stream)))
                  (loop for (msg . more) on (proto-messages message) doing
                    (unless (eq (proto-message-type msg) :group)
                      (write-schema-as type msg stream :indentation (+ indentation 2) :more more)
                      (when more
                        (terpri stream))))
                  (loop for (field . more) on (proto-fields message) doing
                    (write-schema-as type field stream
                                     :indentation (+ indentation 2) :more more
                                     :message message)
                    (when more
                      (terpri stream)))
                  (loop for (extension . more) on (proto-extensions message) doing
                    (write-schema-as type extension stream :indentation (+ indentation 2) :more more)
                    (when more
                      (terpri stream)))))))
    (format stream ")")))

(defparameter *protobuf-slot-comment-column* 56)
(defmethod write-schema-as ((type (eql :lisp)) (field protobuf-field) stream
                            &key (indentation 0) more message)
  (with-prefixed-accessors (value required index packed options documentation) (proto- field)
    (let* ((class (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
           (msg   (and (not (keywordp class))
                       (or (find-message message class) (find-enum message class))))
           (type  (let ((cl (case class
                              ((:int32)       'int32)
                              ((:int64)       'int64)
                              ((:uint32)     'uint32)
                              ((:uint64)     'uint64)
                              ((:sint32)     'sint32)
                              ((:sint64)     'sint64)
                              ((:fixed32)   'fixed32)
                              ((:fixed64)   'fixed64)
                              ((:sfixed32) 'sfixed32)
                              ((:sfixed64) 'sfixed64)
                              ((:float)  'float)
                              ((:double) 'double-float)
                              ((:bool)   'boolean)
                              ((:string) 'string)
                              ((:bytes)  'byte-vector)
                              ((:symbol) 'symbol)
                              (otherwise class))))
                    (cond ((eq required :optional)
                           `(or null ,cl))
                          ((eq required :repeated)
                           (if (eq (proto-default field) $empty-vector)
                             `(vector-of ,cl)
                             `(list-of ,cl)))
                          (t cl)))))
      (cond ((and (typep msg 'protobuf-message)
                  (eq (proto-message-type msg) :group))
             (write-schema-as :lisp msg stream :indentation indentation :index index :arity required))
            (t
             (let* ((defaultp (not (empty-default-p field)))
                    (default  (proto-default field))
                    (default  (and defaultp
                                   (cond ((and (typep msg 'protobuf-enum)
                                               (or (stringp default) (symbolp default)))
                                          (let ((e (find default (proto-values msg)
                                                         :key #'proto-name :test #'string=)))
                                            (and e (proto-value e))))
                                         ((eq class :bool)
                                          (boolean-true-p default))
                                         (t default))))
                    (default  (and defaultp
                                   (if (stringp default) (escape-string default) default)))
                    (reader (unless (eq (proto-reader field) value) (proto-reader field)))
                    (writer (unless (eq (proto-writer field) value) (proto-writer field)))
                    (slot-name (if *show-lisp-field-indexes*
                                 (format nil "(~(~S~) ~D)" value index)
                                 (format nil "~(~S~)" value))))
               (format stream (if (and (keywordp class) (not (eq class :bool)))
                                ;; Keyword class means a primitive type, print default with ~S
                                "~&~@[~VT~](~A :type ~(~S~)~:[~*~; :default ~S~]~
                                 ~@[ :reader ~(~S~)~]~@[ :writer ~(~S~)~]~@[ :packed ~(~S~)~]~
                                 ~@[ :options (~{~@/protobuf-option/~^ ~})~])~
                                 ~:[~*~*~;~VT; ~A~]"
                                ;; Non-keyword class means an enum type, print default with ~(~S~)
                                "~&~@[~VT~](~A :type ~(~S~)~:[~*~; :default ~(~S~)~]~
                                 ~@[ :reader ~(~S~)~]~@[ :writer ~(~S~)~]~@[ :packed ~(~S~)~]~
                                 ~@[ :options (~{~@/protobuf-option/~^ ~})~])~
                                 ~:[~*~*~;~VT; ~A~]")
                       (and (not (zerop indentation)) indentation)
                       slot-name type defaultp default reader writer packed options
                       ;; Don't write the comment if we'll insert a close paren after it
                       (and more documentation) *protobuf-slot-comment-column* documentation)))))))

(defmethod write-schema-as ((type (eql :lisp)) (extension protobuf-extension) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (from to) (proto-extension- extension)
    (format stream "~&~@[~VT~](proto:define-extension ~D ~D)"
            (and (not (zerop indentation)) indentation)
            from (if (eql to #.(1- (ash 1 29))) "max" to))))


(defmethod write-schema-as ((type (eql :lisp)) (service protobuf-service) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (class documentation conc-name) (proto- service)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~](proto:define-service ~(~S~)"
            (and (not (zerop indentation)) indentation) (proto-class service))
    (cond (documentation
           (format stream "~%~@[~VT~](:documentation ~S)"
                   (+ indentation 4) documentation))
          (t
           (format stream " ()")))
    (loop for (method . more) on (proto-methods service) doing
      (write-schema-as type method stream :indentation (+ indentation 2) :more more)
      (when more
        (terpri stream)))
    (format stream ")")))

(defmethod write-schema-as ((type (eql :lisp)) (method protobuf-method) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors
      (function documentation input-type output-type options) (proto- method)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~](~(~S~) (~(~S~) ~(~S~))"
            (and (not (zerop indentation)) indentation)
            function input-type output-type)
    (when options
      (format stream "~%~VT:options (~{~@/protobuf-option/~^ ~})"
              (+ indentation 2) options))
    (format stream ")")))
