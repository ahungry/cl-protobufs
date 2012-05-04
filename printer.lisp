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

(defgeneric write-protobuf-as (type protobuf stream &key indentation &allow-other-keys)
  (:documentation
   "Writes the protobuf object 'protobuf' (schema, message, enum, etc) onto
    the given stream 'stream' in the format given by 'type' (:proto, :text, etc).
    If 'more' is true, this means there are more enum values, fields, etc to
    be written after the current one."))

(defgeneric write-protobuf-documentation (type docstring stream &key indentation)
  (:documentation
   "Writes the docstring as a \"block comment\" onto the given stream 'stream'
    in the format given by 'type' (:proto, :text, etc)."))


;;; Pretty print a schema as a .proto file

(defmethod write-protobuf-as ((type (eql :proto)) (protobuf protobuf) stream
                              &key (indentation 0))
  (with-prefixed-accessors (name documentation syntax package imports options) (proto- protobuf)
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
    (write-protobuf-header type stream)
    (when options
      (dolist (option options)
        (format stream "~&option ~:/protobuf-option/;~%" option))
      (terpri stream))
    (loop for (enum . more) on (proto-enums protobuf) doing
      (write-protobuf-as type enum stream :indentation indentation :more more)
      (terpri stream))
    (loop for (msg . more) on (proto-messages protobuf) doing
      (write-protobuf-as type msg stream :indentation indentation :more more)
      (terpri stream))
    (loop for (svc . more) on (proto-services protobuf) doing
      (write-protobuf-as type svc stream :indentation indentation :more more)
      (terpri stream))))

(defmethod write-protobuf-documentation ((type (eql :proto)) docstring stream
                                         &key (indentation 0))
  (let ((lines (split-string docstring :separators '(#\newline #\return))))
    (dolist (line lines)
      (format stream "~&~@[~VT~]// ~A~%"
              (and (not (zerop indentation)) indentation) line))))

(defvar *lisp-options* '(("lisp_package" "string" 195801)
                         ("lisp_name"    "string" 195802)
                         ("lisp_alias"   "string" 195803)
                         ("lisp_type"    "string" 195804)
                         ("lisp_class"   "string" 195805)
                         ("lisp_slot"    "string" 195806)))

(defvar *option-types* '(("optimize_for" symbol)))

(defmethod write-protobuf-header ((type (eql :proto)) stream)
  (format stream "~&import \"net/proto2/proto/descriptor.proto\";~%~%")
  (format stream "~&extend proto2.MessageOptions {~%")
  (loop for (option type index) in *lisp-options* doing
    (format stream "~&  optional ~A ~A = ~D;~%" type option index))
  (format stream "~&}~%~%"))

(defun cl-user::protobuf-option (stream option colon-p atsign-p)
  (let ((type (or (second (find (proto-name option) *option-types* :key #'first :test #'string=))
                  'string)))
    (cond (colon-p                              ;~:/protobuf-option/ -- .proto format
           (let ((fmt-control
                  (cond ((find (proto-name option) *lisp-options* :key #'first :test #'string=)
                         (if (eql type 'symbol) "(~A)~@[ = ~A~]" "(~A)~@[ = ~S~]"))
                        (t
                         (if (eql type 'symbol) "~A~@[ = ~A~]" "~A~@[ = ~S~]")))))
             (format stream fmt-control (proto-name option) (proto-value option))))
          (atsign-p                             ;~@/protobuf-option/ -- .lisp format
           (format stream "~S ~S" (proto-name option) (proto-value option)))
          (t                                    ;~/protobuf-option/  -- keyword/value format
           (format stream "~(:~A~) ~S" (proto-name option) (proto-value option))))))

(defmethod write-protobuf-as ((type (eql :proto)) (enum protobuf-enum) stream
                              &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name class alias-for documentation options) (proto- enum)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]enum ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (let ((other (and class (not (string= name (class-name->proto class))) class)))
      (when other
        (format stream "~&~VToption (lisp_name) = \"~A:~A\";~%"
                (+ indentation 2) (package-name (symbol-package class)) (symbol-name class))))
    (when alias-for
      (format stream "~&~VToption (lisp_alias) = \"~A:~A\";~%"
              (+ indentation 2) (package-name (symbol-package alias-for)) (symbol-name alias-for)))
    (dolist (option options)
      (format stream "~&option ~:/protobuf-option/;~%" option))
    (loop for (value . more) on (proto-values enum) doing
      (write-protobuf-as type value stream :indentation (+ indentation 2) :more more))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defparameter *protobuf-enum-comment-column* 56)
(defmethod write-protobuf-as ((type (eql :proto)) (val protobuf-enum-value) stream
                              &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation index) (proto- val)
    (format stream "~&~@[~VT~]~A = ~D;~:[~*~*~;~VT// ~A~]~%"
            (and (not (zerop indentation)) indentation) name index
            documentation *protobuf-enum-comment-column* documentation)))


(defmethod write-protobuf-as ((type (eql :proto)) (message protobuf-message) stream
                              &key (indentation 0) more index arity)
  (declare (ignore more arity))
  (with-prefixed-accessors (name class alias-for message-type documentation options) (proto- message)
    (cond ((eql message-type :group)
           ;; If we've got a group, the printer for fields has already
           ;; printed a partial line (nice modularity, huh?)
           (format stream "group ~A = ~D {~%" name index)
           (let ((other (and class (not (string= name (class-name->proto class))) class)))
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
             (write-protobuf-as type enum stream :indentation (+ indentation 2) :more more))
           (loop for (field . more) on (proto-fields message) doing
             (write-protobuf-as type field stream
                                :indentation (+ indentation 2) :more more :message message))
           (format stream "~&~@[~VT~]}~%"
                   (and (not (zerop indentation)) indentation)))
          (t
           (when documentation
             (write-protobuf-documentation type documentation stream :indentation indentation))
           (format stream "~&~@[~VT~]~A ~A {~%"
                   (and (not (zerop indentation)) indentation)
                   (if (eql message-type :message) "message" "extend") name)
           (let ((other (and class (not (string= name (class-name->proto class))) class)))
             (when other
               (format stream "~&~VToption (lisp_name) = \"~A:~A\";~%"
                       (+ indentation 2) (package-name (symbol-package class)) (symbol-name class))))
           (when alias-for
             (format stream "~&~VToption (lisp_alias) = \"~A:~A\";~%"
                     (+ indentation 2) (package-name (symbol-package alias-for)) (symbol-name alias-for)))
           (dolist (option options)
             (format stream "~&~VToption ~:/protobuf-option/;~%"
                     (+ indentation 2) option))
           (cond ((eql message-type :extends)
                  (loop for (field . more) on (proto-fields message) doing
                    (when (eql (proto-message-type field) :extends)
                      (write-protobuf-as type field stream
                                         :indentation (+ indentation 2) :more more
                                         :message message))))
                 (t
                  (loop for (enum . more) on (proto-enums message) doing
                    (write-protobuf-as type enum stream :indentation (+ indentation 2) :more more))
                  (loop for (msg . more) on (proto-messages message) doing
                    (unless (eql (proto-message-type msg) :group)
                      (write-protobuf-as type msg stream :indentation (+ indentation 2) :more more)))
                  (loop for (field . more) on (proto-fields message) doing
                    (write-protobuf-as type field stream
                                       :indentation (+ indentation 2) :more more
                                       :message message))
                  (loop for (extension . more) on (proto-extensions message) doing
                    (write-protobuf-as type extension stream :indentation (+ indentation 2) :more more))))
           (format stream "~&~@[~VT~]}~%"
                   (and (not (zerop indentation)) indentation))))))

(defparameter *protobuf-field-comment-column* 56)
(defmethod write-protobuf-as ((type (eql :proto)) (field protobuf-field) stream
                              &key (indentation 0) more message)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation required index default packed) (proto- field)
    (let ((group (let ((msg (find-message message (proto-class field))))
                   (and msg (eql (proto-message-type msg) :group) msg)))
          (dflt  (if (stringp default)
                   (if (i= (length default) 0) nil default)
                   default)))
      (cond (group
             (format stream "~&~@[~VT~]~(~A~) "
                     (and (not (zerop indentation)) indentation) required)
             (write-protobuf-as type group stream :indentation indentation :index index :arity required))
            (t
             (format stream "~&~@[~VT~]~(~A~) ~A ~A = ~D~@[ [default = ~A]~]~@[ [packed=true]~*~];~:[~*~*~;~VT// ~A~]~%"
                     (and (not (zerop indentation)) indentation)
                     required (proto-type field) name index dflt packed
                     documentation *protobuf-field-comment-column* documentation))))))

(defmethod write-protobuf-as ((type (eql :proto)) (extension protobuf-extension) stream
                              &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (from to) (proto-extension- extension)
    (format stream "~&~@[~VT~]extensions ~D to ~D;~%"
            (and (not (zerop indentation)) indentation)
            from (if (eql to #.(1- (ash 1 29))) "max" to))))


(defmethod write-protobuf-as ((type (eql :proto)) (service protobuf-service) stream
                              &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation) (proto- service)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]service ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (loop for (method . more) on (proto-methods service) doing
      (write-protobuf-as type method stream :indentation (+ indentation 2) :more more))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defmethod write-protobuf-as ((type (eql :proto)) (method protobuf-method) stream
                              &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation input-name output-name options) (proto- method)
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
  (with-prefixed-accessors (name class documentation package lisp-package imports) (proto- protobuf)
    (let* ((optimize (let ((opt (find-option protobuf "optimize_for")))
                       (and opt (cond ((string= opt "SPEED") :speed)
                                      ((string= opt "CODE_SIZE") :space)
                                      (t nil)))))
           (options  (remove "optimize_for" (proto-options protobuf) :test #'string-equal :key #'proto-name))
           (pkg      (and package (if (stringp package) package (string package))))
           (lisp-pkg (and lisp-package (if (stringp lisp-package) lisp-package (string lisp-package))))
           (*protobuf-package* (or (find-package lisp-pkg)
                                   (find-package (string-upcase lisp-pkg))
                                   *package*))
           (*package* *protobuf-package*))
      (when (or lisp-pkg pkg)
        (format stream "~&(in-package \"~A\")~%~%" (string-upcase (or lisp-pkg pkg))))
      (when documentation
        (write-protobuf-documentation type documentation stream :indentation indentation))
      (format stream "~&(proto:define-proto ~(~A~)" (or class name))
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
      (loop for (enum . more) on (proto-enums protobuf) doing
        (write-protobuf-as type enum stream :indentation 2 :more more))
      (loop for (msg . more) on (proto-messages protobuf) doing
        (write-protobuf-as type msg stream :indentation 2 :more more))
      (loop for (svc . more) on (proto-services protobuf) doing
        (write-protobuf-as type svc stream :indentation 2 :more more)))
    (format stream ")~%")))

(defmethod write-protobuf-documentation ((type (eql :lisp)) docstring stream
                                         &key (indentation 0))
  (let ((lines (split-string docstring :separators '(#\newline #\return))))
    (dolist (line lines)
      (format stream "~&~@[~VT~];; ~A~%"
              (and (not (zerop indentation)) indentation) line))))

(defmethod write-protobuf-header ((type (eql :lisp)) stream)
  (declare (ignorable type stream))
  nil)

(defmethod write-protobuf-as ((type (eql :lisp)) (enum protobuf-enum) stream
                              &key (indentation 0) more)
  (declare (ignore more))
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
      (write-protobuf-as type value stream :indentation (+ indentation 2) :more more)
      (when more
        (terpri stream)))
    (format stream ")")))

(defmethod write-protobuf-as ((type (eql :lisp)) (val protobuf-enum-value) stream
                              &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (value index) (proto- val)
    (format stream "~&~@[~VT~](~(~A~) ~D)"
            (and (not (zerop indentation)) indentation) value index)))


(defmethod write-protobuf-as ((type (eql :lisp)) (message protobuf-message) stream
                              &key (indentation 0) more index arity)
  (declare (ignore more))
  (with-prefixed-accessors (name class alias-for conc-name message-type documentation) (proto- message)
    (cond ((eql message-type :group)
           (when documentation
             (write-protobuf-documentation type documentation stream :indentation indentation))
           (format stream "~&~@[~VT~](proto:define-group ~(~S~)"
                   (and (not (zerop indentation)) indentation) class)
           (let ((other (and name (not (string= name (class-name->proto class))) name)))
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
             (write-protobuf-as type enum stream :indentation (+ indentation 2) :more more)
             (when more
               (terpri stream)))
           (loop for (field . more) on (proto-fields message) doing
             (write-protobuf-as type field stream
                                :indentation (+ indentation 2) :more more
                                :message message)
             (when more
               (terpri stream))))
          (t
           (when documentation
             (write-protobuf-documentation type documentation stream :indentation indentation))
           (format stream "~&~@[~VT~](proto:define-~A ~(~S~)"
                   (and (not (zerop indentation)) indentation)
                   (if (eql message-type :message) "message" "extend") class)
           (let ((other (and name (not (string= name (class-name->proto class))) name)))
             (cond ((eql message-type :extends)
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
           (cond ((eql message-type :extends)
                  (loop for (field . more) on (proto-fields message) doing
                    (when (eql (proto-message-type field) :extends)
                      (write-protobuf-as type field stream
                                         :indentation (+ indentation 2) :more more
                                         :message message)
                      (when more
                        (terpri stream)))))
                 (t
                  (loop for (enum . more) on (proto-enums message) doing
                    (write-protobuf-as type enum stream :indentation (+ indentation 2) :more more)
                    (when more
                      (terpri stream)))
                  (loop for (msg . more) on (proto-messages message) doing
                    (unless (eql (proto-message-type msg) :group)
                      (write-protobuf-as type msg stream :indentation (+ indentation 2) :more more)
                      (when more
                        (terpri stream))))
                  (loop for (field . more) on (proto-fields message) doing
                    (write-protobuf-as type field stream
                                       :indentation (+ indentation 2) :more more
                                       :message message)
                    (when more
                      (terpri stream)))
                  (loop for (extension . more) on (proto-extensions message) doing
                    (write-protobuf-as type extension stream :indentation (+ indentation 2) :more more)
                    (when more
                      (terpri stream)))))))
    (format stream ")")))

(defparameter *protobuf-slot-comment-column* 56)
(defmethod write-protobuf-as ((type (eql :lisp)) (field protobuf-field) stream
                              &key (indentation 0) more message)
  (with-prefixed-accessors (value reader writer class required index documentation default) (proto- field)
    (let ((group (let ((msg (find-message message (proto-class field))))
                   (and msg (eql (proto-message-type msg) :group) msg)))
          (dflt  (protobuf-default-to-clos-init default class))
          (clss  (let ((cl (case class
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
      (cond (group
             (write-protobuf-as type group stream :indentation indentation :index index :arity required))
            (t
             (format stream (if (keywordp class)
                              ;; Keyword means a primitive type, print default with ~S
                              "~&~@[~VT~](~(~S~) :type ~(~S~)~@[ :default ~S~]~
                               ~@[ :reader ~(~S~)~]~@[ :writer ~(~S~)~])~:[~*~*~;~VT; ~A~]"
                              ;; Non-keyword must mean an enum type, print default with ~A
                              "~&~@[~VT~](~(~S~) :type ~(~S~)~@[ :default ~(:~A~)~]~
                               ~@[ :reader ~(~S~)~]~@[ :writer ~(~S~)~])~:[~*~*~;~VT; ~A~]")
                     (and (not (zerop indentation)) indentation)
                     value clss dflt reader writer
                     ;; Don't write the comment if we'll insert a close paren after it
                     (and more documentation) *protobuf-slot-comment-column* documentation))))))

(defmethod write-protobuf-as ((type (eql :lisp)) (extension protobuf-extension) stream
                              &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (from to) (proto-extension- extension)
    (format stream "~&~@[~VT~](define-extension ~D ~D)"
            (and (not (zerop indentation)) indentation)
            from (if (eql to #.(1- (ash 1 29))) "max" to))))


(defmethod write-protobuf-as ((type (eql :lisp)) (service protobuf-service) stream
                              &key (indentation 0) more)
  (declare (ignore more))
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
    (loop for (method . more) on (proto-methods service) doing
      (write-protobuf-as type method stream :indentation (+ indentation 2) :more more)
      (when more
        (terpri stream)))
    (format stream ")")))

(defmethod write-protobuf-as ((type (eql :lisp)) (method protobuf-method) stream
                              &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors
      (function documentation input-type output-type options) (proto- method)
    (when documentation
      (write-protobuf-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~](~(~S~) (~(~S~) ~(~S~))"
            (and (not (zerop indentation)) indentation)
            function input-type output-type)
    (when options
      (format stream "~%~VT:options (~{~@/protobuf-option/~^ ~})"
              (+ indentation 2) options))
    (format stream ")")))
