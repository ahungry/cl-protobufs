;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
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

(defgeneric write-schema-header (type schema stream)
  (:documentation
   "Writes a header for the schema onto the given stream 'stream'
    in the format given by 'type' (:proto, :text, etc)."))

(defgeneric write-schema-documentation (type docstring stream &key indentation)
  (:documentation
   "Writes the docstring as a \"block comment\" onto the given stream 'stream'
    in the format given by 'type' (:proto, :text, etc)."))


;;; Pretty print a schema as a .proto file

(defmethod write-schema-as ((type (eql :proto)) (schema protobuf-schema) stream
                            &key (indentation 0))
  (with-prefixed-accessors (documentation syntax package imports options) (proto- schema)
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
    (loop for (alias . more) on (proto-type-aliases schema) doing
      (write-schema-as type alias stream :indentation indentation :more more)
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

(defvar *option-types* '(("ctype"                 symbol)
                         ("deadline"               float)
                         ("deprecated"            symbol)
                         ("optimize_for"          symbol)
                         ("packed"               boolean)
                         ("protocol"              symbol)
                         ("stream_type"           string)
                         ;; Keep the rest of these in alphabetical order
                         ("cc_api_version"       integer)
                         ("cc_generic_services"   symbol)
                         ("go_api_version"       integer)
                         ("go_generic_services"   symbol)
                         ("go_package"            string)
                         ("java_api_version"     integer)
                         ("java_generic_services" symbol)
                         ("java_java5_enums"     boolean)
                         ("java_multiple_files"  boolean)
                         ("java_outer_classname"  string)
                         ("java_package"          string)
                         ("java_use_javaproto2"  boolean)
                         ("py_api_version"       integer)
                         ("py_generic_services"   symbol)))

(defmethod write-schema-header ((type (eql :proto)) (schema protobuf-schema) stream)
  (when (any-lisp-option schema)
    (format stream "~&import \"net/proto2/proto/descriptor.proto\";~%~%")
    (format stream "~&extend proto2.MessageOptions {~%")
    (loop for (option type index) in *lisp-options* doing
      (format stream "~&  optional ~(~A~) ~A = ~D;~%" type option index))
    (format stream "~&}~%~%")))

(defgeneric any-lisp-option (schema)
  (:documentation
   "Returns true iff there is anything in the schema that would require that
    the .proto file include and extend 'MessageOptions'.")
  (:method ((schema protobuf-schema))
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
      nil)))

(defun cl-user::protobuf-option (stream option colon-p atsign-p)
  (let* ((type (or (second (find (proto-name option) *option-types* :key #'first :test #'string=))
                   (proto-type option)))
         (value (proto-value option)))
    (cond (colon-p                              ;~:/protobuf-option/ -- .proto format
           (let ((fmt-control
                  (cond ((find (proto-name option) *lisp-options* :key #'first :test #'string=)
                         (case type
                           ((symbol) "(~A)~@[ = ~A~]")
                           ((boolean) "(~A)~@[ = ~(~A~)~]")
                           (otherwise
                            (cond ((typep value 'standard-object)
                                   ;; If the value is an instance of some class,
                                   ;; then it must be some sort of complex option,
                                   ;; so print the value using the text format
                                   (setq value
                                         (with-output-to-string (s)
                                           (print-text-format value nil
                                                              :stream s :print-name nil :suppress-line-breaks t)))
                                   "(~A)~@[ = ~A~]")
                                  (t
                                   "(~A)~@[ = ~S~]")))))
                        (t
                         (case type
                           ((symbol) "~A~@[ = ~A~]")
                           ((boolean) "~A~@[ = ~(~A~)~]")
                           (otherwise
                            (cond ((typep value 'standard-object)
                                   (setq value
                                         (with-output-to-string (s)
                                           (print-text-format value nil
                                                              :stream s :print-name nil :suppress-line-breaks t)))
                                   "~A~@[ = ~A~]")
                                  (t "~A~@[ = ~S~]"))))))))
             (format stream fmt-control (proto-name option) value)))
          (atsign-p                             ;~@/protobuf-option/ -- string/value format
           (let ((fmt-control (if (eq type 'symbol) "~(~S~) ~A" "~(~S~) ~S")))
             (format stream fmt-control (proto-name option) value)))
          (t                                    ;~/protobuf-option/  -- keyword/value format
           (let ((fmt-control (if (eq type 'symbol) "~(:~A~) ~A" "~(:~A~) ~S")))
             (format stream fmt-control (proto-name option) value))))))

(defun cl-user::source-location (stream location colon-p atsign-p)
  (declare (ignore colon-p atsign-p))
  (format stream "(~S ~D ~D)" 
          (source-location-pathname location)
          (source-location-start-pos location) (source-location-end-pos location)))

(defmethod write-schema-as ((type (eql :proto)) (enum protobuf-enum) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name class alias-for documentation options) (proto- enum)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]enum ~A {~%"
            (and (not (zerop indentation)) indentation)
            (maybe-qualified-name enum))
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
            (and (not (zerop indentation)) indentation)
            (maybe-qualified-name val) index
            documentation *protobuf-enum-comment-column* documentation)))

(defmethod write-schema-as ((type (eql :proto)) (alias protobuf-type-alias) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name lisp-type proto-type) (proto- alias)
    (let ((comment (format nil "Note: there is an alias ~A that maps Lisp ~(~S~) to Protobufs ~(~A~)"
                           name lisp-type proto-type)))
      (write-schema-documentation type comment stream :indentation indentation))
    (format stream "~&~@[~VT~]~%"
            (and (not (zerop indentation)) indentation))))

(defmethod write-schema-as ((type (eql :proto)) (message protobuf-message) stream
                            &key (indentation 0) more index arity)
  (declare (ignore more arity))
  (let ((*protobuf* message))
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
                     (if (eq message-type :message) "message" "extend") 
                     (maybe-qualified-name message))
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
                     (and (not (zerop indentation)) indentation)))))))

(defun maybe-qualified-name (x &optional name)
  "Given a message, return a fully qualified name if the short name
   is not sufficient to name the message in the current scope."
  (etypecase x
    ((or protobuf-message protobuf-enum  protobuf-enum-value
         protobuf-type-alias)
     (cond ((string= (make-qualified-name (proto-parent x) (proto-name x))
                     (proto-qualified-name x))
            (proto-name x))
           (t
            (proto-qualified-name x))))
    (null name)))

(defparameter *protobuf-field-comment-column* 56)
(defmethod write-schema-as ((type (eql :proto)) (field protobuf-field) stream
                            &key (indentation 0) more message)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation required type index packed options) (proto- field)
    (let* ((class (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
           (msg   (and (not (keywordp class))
                       (or (find-message message class)
                           (find-enum message class)
                           (find-type-alias message class)))))
      (cond ((and (typep msg 'protobuf-message)
                  (eq (proto-message-type msg) :group))
             (format stream "~&~@[~VT~]~(~A~) "
                     (and (not (zerop indentation)) indentation) required)
             (write-schema-as :proto msg stream :indentation indentation :index index :arity required))
            (t
             (let* ((defaultp (if (proto-alias-for message)
                                ;; Special handling for imported CLOS classes
                                (if (eq (proto-required field) :optional)
                                  nil
                                  (and (proto-default field)
                                       (not (equalp (proto-default field) #()))
                                       (not (empty-default-p field))))
                                (not (empty-default-p field))))
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
               (if (typep msg 'protobuf-type-alias)
                 (format stream "~&~@[~VT~]~(~A~) ~(~A~) ~A = ~D~
                                 ~:[~*~; [default = ~S]~]~@[ [packed = true]~*~]~{ [~:/protobuf-option/]~};~
                                 ~:[~*~*~;~VT// ~A~]~%"
                         (and (not (zerop indentation)) indentation)
                         required (proto-proto-type msg) name index
                         defaultp default packed options
                         t *protobuf-field-comment-column*
                         (format nil "alias maps Lisp ~(~S~) to Protobufs ~(~A~)"
                                 (proto-lisp-type msg) (proto-proto-type msg)))
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
                         required (maybe-qualified-name msg type) name index
                         defaultp default packed options
                         documentation *protobuf-field-comment-column* documentation))))))))

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
  (with-prefixed-accessors
      (name documentation input-name output-name streams-name options) (proto- method)
    (let* ((imsg (find-message *protobuf* input-name))
           (omsg (find-message *protobuf* output-name))
           (smsg (find-message *protobuf* streams-name))
           (iname (maybe-qualified-name imsg))
           (oname (maybe-qualified-name omsg))
           (sname (maybe-qualified-name smsg)))
      (when documentation
        (write-schema-documentation type documentation stream :indentation indentation))
      (format stream "~&~@[~VT~]rpc ~A (~@[~A~])~@[ streams (~A)~]~@[ returns (~A)~]"
              (and (not (zerop indentation)) indentation)
              name iname sname oname)
      (cond (options
             (format stream " {~%")
             (dolist (option options)
               (format stream "~&~@[~VT~]option ~:/protobuf-option/;~%"
                       (+ indentation 2) option))
             (format stream "~@[~VT~]}"
                     (and (not (zerop indentation)) indentation)))
            (t
             (format stream ";~%"))))))


;;; Pretty print a schema as a .lisp file

(defvar *show-lisp-enum-indexes*  t)
(defvar *show-lisp-field-indexes* t)
(defvar *use-common-lisp-package* nil)

(defmethod write-schema-as ((type (eql :lisp)) (schema protobuf-schema) stream
                            &key (indentation 0)
                                 (show-field-indexes *show-lisp-field-indexes*)
                                 (show-enum-indexes *show-lisp-enum-indexes*)
                                 (use-common-lisp *use-common-lisp-package*))
  (with-prefixed-accessors (name class documentation package lisp-package imports) (proto- schema)
    (let* ((optimize (let ((opt (find-option schema "optimize_for")))
                       (and opt (cond ((string= opt "SPEED") :speed)
                                      ((string= opt "CODE_SIZE") :space)
                                      (t nil)))))
           (options  (remove-if #'(lambda (x) (string= (proto-name x) "optimize_for"))
                                (proto-options schema)))
           (pkg      (and package (if (stringp package) package (string package))))
           (lisp-pkg (and lisp-package (if (stringp lisp-package) lisp-package (string lisp-package))))
           (*show-lisp-enum-indexes*  show-enum-indexes)
           (*show-lisp-field-indexes* show-field-indexes)
           (*use-common-lisp-package* use-common-lisp)
           (*protobuf-package* (or (find-proto-package lisp-pkg) *package*))
           (*package* *protobuf-package*))
      (when (or lisp-pkg pkg)
        (let ((pkg (string-upcase (or lisp-pkg pkg))))
          (format stream "~&(cl:eval-when (:execute :compile-toplevel :load-toplevel) ~
                          ~%  (unless (cl:find-package \"~A\") ~
                          ~%    (cl:defpackage ~A (:use~@[ ~(~S~)~])))) ~
                          ~%(cl:in-package \"~A\") ~
                          ~%(cl:export '(~{~A~^~%             ~}))~%~%"
                  pkg pkg (and *use-common-lisp-package* :common-lisp) pkg
                  (collect-exports schema))))
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
          (format stream "~A:options (~{~/protobuf-option/~^ ~})" spaces options)
          (when documentation
            (terpri stream))
          (setq spaces "     "))
        (when documentation
          (format stream "~A:documentation ~S" spaces documentation)))
      (format stream ")")
      (loop for (enum . more) on (proto-enums schema) doing
        (write-schema-as type enum stream :indentation 2 :more more))
      (loop for (alias . more) on (proto-type-aliases schema) doing
        (write-schema-as type alias stream :indentation 2 :more more))
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
  (with-prefixed-accessors (name class alias-for
                            documentation source-location) (proto- enum)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~@[~VT~](proto:define-enum ~(~S~)"
            (and (not (zerop indentation)) indentation) class)
    (let ((other (and name (not (string-equal name (class-name->proto class))) name)))
      (cond ((or other alias-for documentation source-location)
             (format stream "~%~@[~VT~](~:[~*~*~;:name ~(~S~)~@[~%~VT~]~]~
                                        ~:[~*~*~;:alias-for ~(~S~)~@[~%~VT~]~]~
                                        ~:[~*~;:documentation ~S~@[~%~VT~]~]~
                                        ~:[~*~;:source-location ~/source-location/~])"
                     (+ indentation 4)
                     other other (and (or alias-for documentation) (+ indentation 5))
                     alias-for alias-for (and (or documentation source-location) (+ indentation 5))
                     documentation documentation (and source-location (+ indentation 5))
                     source-location source-location))
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

(defmethod write-schema-as ((type (eql :lisp)) (alias protobuf-type-alias) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (terpri stream)
  (with-prefixed-accessors (class lisp-type proto-type serializer deserializer) (proto- alias)
    (format stream "~@[~VT~](proto:define-type-alias ~(~S~)"
            (and (not (zerop indentation)) indentation) class)
    (format stream " ()")                       ;no options yet
    (format stream "~%~@[~VT~]:lisp-type ~(~S~)~
                    ~%~@[~VT~]:proto-type ~(~A~)~
                    ~%~@[~VT~]:serializer ~(~S~)~
                    ~%~@[~VT~]:deserializer ~(~S~))"
            (+ indentation 2) lisp-type
            (+ indentation 2) proto-type
            (+ indentation 2) serializer
            (+ indentation 2) deserializer)))

(defmethod write-schema-as ((type (eql :lisp)) (message protobuf-message) stream
                            &key (indentation 0) more index arity)
  (declare (ignore more))
  (let ((*protobuf* message))
    (with-prefixed-accessors (name class alias-for conc-name message-type
                              documentation source-location) (proto- message)
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
                                          ~:[~*~;:documentation ~S~@[~%~VT~]~]~
                                          ~:[~*~;:source-location ~/source-location/~])"
                       (+ indentation 4)
                       index (+ indentation 5)
                       arity (and (or other alias-for conc-name documentation source-location) (+ indentation 5))
                       other other (and (or alias-for conc-name documentation source-location) (+ indentation 5))
                       alias-for alias-for (and (or conc-name documentation source-location) (+ indentation 5))
                       conc-name conc-name (and (or documentation source-location) (+ indentation 5))
                       documentation documentation (and source-location (+ indentation 5))
                       source-location source-location))
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
                     ((or other alias-for conc-name documentation source-location)
                      (format stream "~%~@[~VT~](~:[~*~*~;:name ~(~S~)~@[~%~VT~]~]~
                                                 ~:[~*~*~;:alias-for ~(~S~)~@[~%~VT~]~]~
                                                 ~:[~*~*~;:conc-name ~(~S~)~@[~%~VT~]~]~
                                                 ~:[~*~;:documentation ~S~@[~%~VT~]~]~
                                                 ~:[~*~;:source-location ~/source-location/~])"
                              (+ indentation 4)
                              other other (and (or alias-for conc-name documentation source-location) (+ indentation 5))
                              alias-for alias-for (and (or conc-name documentation source-location) (+ indentation 5))
                              conc-name conc-name (and (or documentation source-location) (+ indentation 5))
                              documentation documentation (and source-location (+ indentation 5))
                              source-location source-location))
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
      (format stream ")"))))

(defparameter *protobuf-slot-comment-column* 56)
(defmethod write-schema-as ((type (eql :lisp)) (field protobuf-field) stream
                            &key (indentation 0) more message)
  (with-prefixed-accessors (value required index packed options documentation) (proto- field)
    (let* ((class (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
           (msg   (and (not (keywordp class))
                       (or (find-message message class)
                           (find-enum message class)
                           (find-type-alias message class))))
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
                           (if (vector-field-p field)
                             `(vector-of ,cl)
                             `(list-of ,cl)))
                          (t cl)))))
      (cond ((and (typep msg 'protobuf-message)
                  (eq (proto-message-type msg) :group))
             (write-schema-as :lisp msg stream :indentation indentation :index index :arity required))
            (t
             (let* ((defaultp (if (proto-alias-for message)
                                (if (eq (proto-required field) :optional)
                                  nil
                                  (and (proto-default field)
                                       (not (equalp (proto-default field) #()))
                                       (not (empty-default-p field))))
                                (not (empty-default-p field))))
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
                    (conc-name (proto-conc-name message))
                    (reader (when (and (not (eq (proto-reader field) value))
                                       (not (string-equal (proto-reader field)
                                                          (format nil "~A~A" conc-name value))))
                              (proto-reader field)))
                    (writer (when (and (not (eq (proto-writer field) value))
                                       (not (string-equal (proto-writer field)
                                                          (format nil "~A~A" conc-name value))))
                              (proto-writer field)))
                    (slot-name (if *show-lisp-field-indexes*
                                 (format nil "(~(~S~) ~D)" value index)
                                 (format nil "~(~S~)" value))))
               (format stream (if (and (keywordp class) (not (eq class :bool)))
                                ;; Keyword class means a primitive type, print default with ~S
                                "~&~@[~VT~](~A :type ~(~S~)~:[~*~; :default ~S~]~
                                 ~@[ :reader ~(~S~)~]~@[ :writer ~(~S~)~]~@[ :packed ~(~S~)~]~
                                 ~@[ :options (~{~/protobuf-option/~^ ~})~])~
                                 ~:[~*~*~;~VT; ~A~]"
                                ;; Non-keyword class means an enum type, print default with ~(~S~)
                                "~&~@[~VT~](~A :type ~(~S~)~:[~*~; :default ~(~S~)~]~
                                 ~@[ :reader ~(~S~)~]~@[ :writer ~(~S~)~]~@[ :packed ~(~S~)~]~
                                 ~@[ :options (~{~/protobuf-option/~^ ~})~])~
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
  (with-prefixed-accessors (class documentation source-location) (proto- service)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~](proto:define-service ~(~S~)"
            (and (not (zerop indentation)) indentation) (proto-class service))
    (cond ((or documentation source-location)
           (format stream "~%~@[~VT~](~:[~*~;:documentation ~S~@[~%~VT~]~]~
                                      ~:[~*~;:source-location ~/source-location/~])"
                   (+ indentation 4)
                   documentation documentation (and source-location (+ indentation 5))
                   source-location source-location))
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
  (with-prefixed-accessors (class input-type output-type streams-type options
                            documentation source-location) (proto- method)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~](~(~S~) (~(~S~) => ~(~S~)~@[ :streams ~(~S~)~])"
            (and (not (zerop indentation)) indentation)
            class input-type output-type streams-type)
    (when options
      (format stream "~%~VT:options (~{~/protobuf-option/~^ ~})"
              (+ indentation 2) options))
    (format stream ")")))


;;; Collect symbols to be exported

(defgeneric collect-exports (schema)
  (:documentation
   "Collect all the symbols that should be exported from a Protobufs package"))

(defmethod collect-exports ((schema protobuf-schema))
  (delete-duplicates
   (delete-if #'null
    (append (mapcan #'collect-exports (proto-enums schema))
            (mapcan #'collect-exports (proto-messages schema))
            (mapcan #'collect-exports (proto-services schema))))
   :from-end t))

;; Export just the type name
(defmethod collect-exports ((enum protobuf-enum))
  (list (proto-class enum)))

;; Export the class name and all of the accessor names
(defmethod collect-exports ((message protobuf-message))
  (append (list (proto-class message))
          (mapcan #'collect-exports (proto-fields message))))

;; Export just the slot accessor name
(defmethod collect-exports ((field protobuf-field))
  (list (proto-slot field)))

;; Export the names of all the methods
(defmethod collect-exports ((service protobuf-service))
  (mapcan #'collect-exports (proto-methods service)))

;; Export just the method name
(defmethod collect-exports ((method protobuf-method))
  (list (proto-client-stub method) (proto-server-stub method)))
