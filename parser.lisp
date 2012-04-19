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


;;; .proto file parsing

;;; Parsing utilities

(declaim (inline proto-whitespace-char-p))
(defun proto-whitespace-char-p (ch)
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (and ch (member ch '(#\space #\tab #\return #\newline)))))

(declaim (inline proto-eol-char-p))
(defun proto-eol-char-p (ch)
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (and ch (member ch '(#\return #\newline)))))

(declaim (inline proto-token-char-p))
(defun proto-token-char-p (ch)
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (and ch (or (alpha-char-p ch)
                (digit-char-p ch)
                (member ch '(#\_ #\.))))))


(defun skip-whitespace (stream)
  "Skip all the whitespace characters that are coming up in the stream."
  (loop for ch = (peek-char nil stream nil)
        until (or (null ch) (not (proto-whitespace-char-p ch)))
        do (read-char stream nil)))

;;--- Collect the comment so we can attach it to its associated object
(defun maybe-skip-comments (stream)
  "If what appears next in the stream is a comment, skip it and any following comments,
   then skip any following whitespace."
  (loop
    (unless (eql (peek-char nil stream nil) #\/)
      (return)
      (read-char stream)
      (case (peek-char nil stream nil)
        ((#\/)
         (skip-line-comment stream))
        ((#\*)
         (skip-block-comment stream))
        (otherwise
         (error "Found a '~C' at position ~D to start a comment, but no following '~C' or '~C'"
                #\/ (file-position stream) #\/ #\*)))))
  (skip-whitespace stream))

(defun skip-line-comment (stream)
  "Skip to the end of a line comment, that is, to the end of the line.
   Then skip any following whitespace."
  (loop for ch = (read-char stream nil)
        until (or (null ch) (proto-eol-char-p ch)))
  (skip-whitespace stream))

(defun skip-block-comment (stream)
  "Skip to the end of a block comment, that is, until a '*/' is seen.
   Then skip any following whitespace."
  (loop for ch = (read-char stream nil)
        do (cond ((null ch)
                  (error "Premature end of file while skipping block comment"))
                 ((and (eql ch #\*)
                       (eql (peek-char nil stream nil) #\/))
                  (read-char stream nil)
                  (return))))
  (skip-whitespace stream))


(defun expect-char (stream ch &optional within)
  "Expect to see 'ch' as the next character in the stream; signal an error if it's not there.
   Then skip all of the following whitespace."
  (if (if (listp ch)
        (member (peek-char nil stream nil) ch)
        (eql (peek-char nil stream nil) ch))
    (read-char stream)
    (error "No '~C' found~@[ within '~A'~] at position ~D"
           ch within (file-position stream)))
  (skip-whitespace stream))


(defun parse-token (stream)
  "Parse the next token in the stream, then skip the following whitespace.
   The returned value is the token."
  (when (proto-token-char-p (peek-char nil stream nil))
    (loop for ch = (read-char stream nil)
          for ch1 = (peek-char nil stream nil)
          collect ch into token
          until (or (null ch1) (not (proto-token-char-p ch1)))
          finally (progn
                    (skip-whitespace stream)
                    (return (coerce token 'string))))))

(defun parse-parenthesized-token (stream)
  "Parse the next token in the stream, then skip the following whitespace.
   The token might be surrounded by parentheses.
   The returned value is the token."
  (let ((left (peek-char nil stream nil)))
    (when (eq left #\()
      (read-char stream))
    (when (proto-token-char-p (peek-char nil stream nil))
      (loop for ch = (read-char stream nil)
            for ch1 = (peek-char nil stream nil)
            collect ch into token
            until (or (null ch1) (not (proto-token-char-p ch1)))
            finally (progn
                      (skip-whitespace stream)
                      (when (eq left #\()
                        (expect-char stream #\) "option"))
                      (return (coerce token 'string)))))))

(defun parse-string (stream)
  "Parse the next quoted string in the stream, then skip the following whitespace.
   The returned value is the string, without the quotation marks."
  (loop with ch0 = (read-char stream nil)
        for ch = (read-char stream nil)
        until (or (null ch) (char= ch ch0))
        collect ch into string
        finally (progn
                  (skip-whitespace stream)
                  (return (coerce string 'string)))))

(defun parse-int (stream)
  "Parse the next token in the stream as an integer, then skip the following whitespace.
   The returned value is the integer."
  (when (digit-char-p (peek-char nil stream nil))
    (loop for ch = (read-char stream nil)
          for ch1 = (peek-char nil stream nil)
          collect ch into token
          until (or (null ch1) (not (digit-char-p ch1)))
          finally (progn
                    (skip-whitespace stream)
                    (return (parse-integer (coerce token 'string)))))))

(defun parse-float (stream)
  "Parse the next token in the stream as a float, then skip the following whitespace.                                     The returned value is the float."
  (when (let ((ch (peek-char nil stream nil)))
            (or (digit-char-p ch) (eql ch #\-)))
    (let ((token (parse-token stream)))
      (when token
        (skip-whitespace stream)
        (coerce (read-from-string token) 'float)))))


;;; The parser itself

(defun parse-protobuf-from-file (filename)
  "Parses the named file as a .proto file, and returns the Protobufs schema."
  (with-open-file (stream filename
                   :direction :input
                   :external-format :utf-8
                   :element-type 'character)
    (parse-protobuf-from-stream stream
                                :name  (class-name->proto (pathname-name (pathname stream)))
                                :class (pathname-name (pathname stream)))))

;; The syntax for Protocol Buffers is so simple that it doesn't seem worth
;; writing a sophisticated parser
;; Note that we don't put the result into *all-protobufs*; do that at a higher level
(defun parse-protobuf-from-stream (stream &key name class)
  "Parses a top-level .proto file from the stream 'stream'.
   Returns the protobuf schema that describes the .proto file."
  (let* ((protobuf (make-instance 'protobuf
                     :class class
                     :name  name))
         (*protobuf* protobuf)
         (*protobuf-package* nil))
    (loop
      (skip-whitespace stream)
      (maybe-skip-comments stream)
      (let ((char (peek-char nil stream nil)))
        (cond ((null char)
               (return-from parse-protobuf-from-stream protobuf))
              ((proto-token-char-p char)
               (let ((token (parse-token stream)))
                 (cond ((string= token "syntax")
                        (parse-proto-syntax stream protobuf))
                       ((string= token "package")
                        (parse-proto-package stream protobuf))
                       ((string= token "import")
                        (parse-proto-import stream protobuf))
                       ((string= token "option")
                        (let* ((option (parse-proto-option stream protobuf))
                               (name   (and option (proto-name option)))
                               (value  (and option (proto-value option))))
                          (when option
                            (cond ((string= name "optimize_for")
                                   (let ((value (cond ((string= value "SPEED") :speed)
                                                      ((string= value "CODE_SIZE") :space)
                                                      (t nil))))
                                     (setf (proto-optimize protobuf) value)))
                                  ((string= name "lisp_package")
                                   (let ((package (or (find-package value)
                                                      (find-package (string-upcase value)))))
                                     (setf (proto-lisp-package protobuf) value)
                                     (setq *protobuf-package* package)))))))
                       ((string= token "enum")
                        (parse-proto-enum stream protobuf))
                       ((string= token "extend")
                        (parse-proto-extend stream protobuf))
                       ((string= token "message")
                        (parse-proto-message stream protobuf))
                       ((string= token "service")
                        (parse-proto-service stream protobuf)))))
              (t
               (error "Syntax error at position ~D" (file-position stream))))))))

(defun parse-proto-syntax (stream protobuf &optional (terminator #\;))
  "Parse a Protobufs syntax line from 'stream'.
   Updates the 'protobuf' object to use the syntax."
  (let ((syntax (prog1 (parse-token stream)
                  (expect-char stream terminator "syntax")
                  (maybe-skip-comments stream))))
    (setf (proto-syntax protobuf) syntax)))

(defun parse-proto-package (stream protobuf &optional (terminator #\;))
  "Parse a Protobufs package line from 'stream'.
   Updates the 'protobuf' object to use the package."
  (check-type protobuf protobuf)
  (let* ((package  (prog1 (parse-token stream)
                     (expect-char stream terminator "package")
                     (maybe-skip-comments stream)))
         (lisp-pkg (or (proto-lisp-package protobuf)
                       (substitute #\- #\_ package))))
    (setf (proto-package protobuf) package)
    (unless (proto-lisp-package protobuf)
      (setf (proto-lisp-package protobuf) lisp-pkg))
    (let ((package (or (find-package lisp-pkg)
                       (find-package (string-upcase lisp-pkg)))))
      (setq *protobuf-package* package))))

(defun parse-proto-import (stream protobuf &optional (terminator #\;))
  "Parse a Protobufs import line from 'stream'.
   Updates the 'protobuf' object to use the package."
  (check-type protobuf protobuf)
  (let ((import (prog1 (parse-string stream)
                  (expect-char stream terminator "package")
                  (maybe-skip-comments stream))))
    ;;---*** This needs to parse the imported file(s)
    (setf (proto-imports protobuf) (nconc (proto-imports protobuf) (list import)))))

(defun parse-proto-option (stream protobuf &optional (terminator #\;))
  "Parse a Protobufs option line from 'stream'.
   Updates the 'protobuf' (or message, service, method) to have the option."
  (check-type protobuf (or null base-protobuf))
  (let* ((key (prog1 (parse-parenthesized-token stream)
                (expect-char stream #\= "option")))
         (val (prog1 (if (eql (peek-char nil stream nil) #\")
                       (parse-string stream)
                       (parse-token stream))
                (expect-char stream terminator "option")
                (maybe-skip-comments stream)))
         (option (make-instance 'protobuf-option
                   :name  key
                   :value val)))
    (cond (protobuf
           (setf (proto-options protobuf) (nconc (proto-options protobuf) (list option)))
           option)
          (t
           ;; If nothing to graft the option into, just return it as the value
           option))))


(defun parse-proto-enum (stream protobuf)
  "Parse a Protobufs 'enum' from 'stream'.
   Updates the 'protobuf' or 'protobuf-message' object to have the enum."
  (check-type protobuf (or protobuf protobuf-message))
  (let* ((name (prog1 (parse-token stream)
                 (expect-char stream #\{ "enum")
                 (maybe-skip-comments stream)))
         (enum (make-instance 'protobuf-enum
                 :class (proto->class-name name *protobuf-package*)
                 :name name)))
    (loop
      (let ((name (parse-token stream)))
        (when (null name)
          (expect-char stream #\} "enum")
          (maybe-skip-comments stream)
          (setf (proto-enums protobuf) (nconc (proto-enums protobuf) (list enum)))
          (let ((type (find-option enum "lisp_name")))
            (when type
              (setf (proto-class enum) (make-lisp-symbol type))))
          (let ((alias (find-option enum "lisp_alias")))
            (when alias
              (setf (proto-alias-for enum) (make-lisp-symbol alias))))
          (return-from parse-proto-enum))
        (if (string= name "option")
          (parse-proto-option stream enum #\;)
          (parse-proto-enum-value stream enum name))))))

(defun parse-proto-enum-value (stream enum name)
  "Parse a Protobufs enum value from 'stream'.
   Updates the 'protobuf-enum' object to have the enum value."
  (check-type enum protobuf-enum)
  (expect-char stream #\= "enum")
  (let* ((idx  (prog1 (parse-int stream)
                 (expect-char stream #\; "enum")
                 (maybe-skip-comments stream)))
         (value (make-instance 'protobuf-enum-value
                  :name  name
                  :index idx
                  :value (proto->enum-name name *protobuf-package*))))
    (setf (proto-values enum) (nconc (proto-values enum) (list value)))))


(defun parse-proto-message (stream protobuf)
  "Parse a Protobufs 'message' from 'stream'.
   Updates the 'protobuf' or 'protobuf-message' object to have the message."
  (check-type protobuf (or protobuf protobuf-message))
  (let* ((name (prog1 (parse-token stream)
                 (expect-char stream #\{ "message")
                 (maybe-skip-comments stream)))
         (message (make-instance 'protobuf-message
                    :class (proto->class-name name *protobuf-package*)
                    :name name
                    :parent protobuf))
         (*protobuf* message))
    (loop
      (let ((token (parse-token stream)))
        (when (null token)
          (expect-char stream #\} "message")
          (maybe-skip-comments stream)
          (setf (proto-messages protobuf) (nconc (proto-messages protobuf) (list message)))
          (let ((type (find-option message "lisp_name")))
            (when type
              (setf (proto-class message) (make-lisp-symbol type))))
          (let ((alias (find-option message "lisp_alias")))
            (when alias
              (setf (proto-alias-for message) (make-lisp-symbol alias))))
          (return-from parse-proto-message))
        (cond ((string= token "enum")
               (parse-proto-enum stream message))
              ((string= token "extend")
               (parse-proto-extend stream message))
              ((string= token "message")
               (parse-proto-message stream message))
              ((member token '("required" "optional" "repeated") :test #'string=)
               (parse-proto-field stream message token))
              ((string= token "option")
               (parse-proto-option stream message #\;))
              ((string= token "extensions")
               (parse-proto-extension stream message))
              (t
               (error "Unrecognized token ~A at position ~D"
                      token (file-position stream))))))))

(defun parse-proto-extend (stream protobuf)
  "Parse a Protobufs 'extend' from 'stream'.
   Updates the 'protobuf' or 'protobuf-message' object to have the message."
  (check-type protobuf (or protobuf protobuf-message))
  (let* ((name (prog1 (parse-token stream)
                 (expect-char stream #\{ "extend")
                 (maybe-skip-comments stream)))
         (message (find-message *protobuf* name))
         (extends (and message
                       (make-instance 'protobuf-message
                         :class  (proto->class-name name *protobuf-package*)
                         :name   name
                         :parent (proto-parent message)
                         :conc-name (proto-conc-name message)
                         :alias-for (proto-alias-for message)
                         :enums    (copy-list (proto-enums message))
                         :messages (copy-list (proto-messages message))
                         :fields   (copy-list (proto-fields message))
                         :extension-p t))))             ;this message is an extension
    (loop
      (let ((token (parse-token stream)))
        (when (null token)
          (expect-char stream #\} "extend")
          (maybe-skip-comments stream)
          (setf (proto-messages protobuf) (nconc (proto-messages protobuf) (list extends)))
          (setf (proto-extenders protobuf) (nconc (proto-extenders protobuf) (list extends)))
          (let ((type (find-option extends "lisp_name")))
            (when type
              (setf (proto-class extends) (make-lisp-symbol type))))
          (let ((alias (find-option extends "lisp_alias")))
            (when alias
              (setf (proto-alias-for extends) (make-lisp-symbol alias))))
          (return-from parse-proto-extend))
        (cond ((member token '("required" "optional" "repeated") :test #'string=)
               (parse-proto-field stream extends token))
              ((string= token "option")
               (parse-proto-option stream extends #\;))
              (t
               (error "Unrecognized token ~A at position ~D"
                      token (file-position stream))))))))

(defun parse-proto-field (stream message required)
  "Parse a Protobufs field from 'stream'.
   Updates the 'protobuf-message' object to have the field."
  (check-type message protobuf-message)
  (let* ((type (parse-token stream))
         (name (prog1 (parse-token stream)
                 (expect-char stream #\= "message")))
         (idx  (parse-int stream))
         (opts (prog1 (parse-proto-field-options stream)
                 (expect-char stream #\; "message")
                 (maybe-skip-comments stream)))
         (dflt   (find-option opts "default"))
         (packed (find-option opts "packed"))
         (ptype  (if (member type '("int32" "int64" "uint32" "uint64" "sint32" "sint64"
                                    "fixed32" "fixed64" "sfixed32" "sfixed64"
                                    "string" "bytes" "bool" "float" "double") :test #'string=)
                   (kintern type)
                   type))
         (class  (if (keywordp ptype) ptype (proto->class-name type *protobuf-package*)))
         (field  (make-instance 'protobuf-field
                   :name  name
                   :value (proto->slot-name name *protobuf-package*)
                   :type  type
                   :class class
                   ;; One of :required, :optional or :repeated
                   :required (kintern required)
                   :index idx
                   :default dflt
                   :packed  (and packed (string= packed "true"))
                   :extension-p (proto-extension-p message))))
    ;;--- Make sure extension field's index is allowable within 'proto-extensions'
    (let ((slot (find-option opts "lisp_name")))
      (when slot
        (setf (proto-value field) (make-lisp-symbol type))))
    (setf (proto-fields message) (nconc (proto-fields message) (list field)))))

(defun parse-proto-field-options (stream)
  "Parse any options in a Protobufs field from 'stream'.
   Returns a list of 'protobuf-option' objects."
  (with-collectors ((options collect-option))
    (loop
      (unless (eql (peek-char nil stream nil) #\[)
        (return-from parse-proto-field-options options))
      (expect-char stream #\[ "message")
      (collect-option (parse-proto-option stream nil #\])))
    options))

(defun parse-proto-extension (stream message)
  (check-type message protobuf-message)
  (let* ((from  (parse-int stream))
         (token (parse-token stream))
         (to    (if (digit-char-p (peek-char nil stream nil))
                  (parse-int stream)
                  (parse-token stream))))
    (expect-char stream #\; "message")
    (assert (string= token "to") ()
            "Expected 'to' in 'extensions' at position ~D" (file-position stream))
    (assert (or (integerp to) (string= to "max")) ()
            "Extension value is not an integer or 'max' as position ~D" (file-position stream))
    (setf (proto-extensions message)
          (nconc (proto-extensions message)
                 (list (make-instance 'protobuf-extension
                         :from from
                         :to   (if (integerp to) to #.(1- (ash 1 29)))))))))


(defun parse-proto-service (stream protobuf)
  "Parse a Protobufs 'service' from 'stream'.
   Updates the 'protobuf-protobuf' object to have the service."
  (check-type protobuf protobuf)
  (let* ((name (prog1 (parse-token stream)
                 (expect-char stream #\{ "service")
                 (maybe-skip-comments stream)))
         (service (make-instance 'protobuf-service
                    :class (proto->class-name name *protobuf-package*)
                    :name name)))
    (loop
      (let ((token (parse-token stream)))
        (when (null token)
          (expect-char stream #\} "service")
          (maybe-skip-comments stream)
          (setf (proto-services protobuf) (nconc (proto-services protobuf) (list service)))
          (return-from parse-proto-service))
        (cond ((string= token "option")
               (parse-proto-option stream service #\;))
              ((string= token "rpc")
               (parse-proto-method stream service))
              (t
               (error "Unrecognized token ~A at position ~D"
                      token (file-position stream))))))))

(defun parse-proto-method (stream service)
  "Parse a Protobufs method from 'stream'.
   Updates the 'protobuf-service' object to have the method."
  (check-type service protobuf-service)
  (let* ((name (parse-token stream))
         (in   (prog2 (expect-char stream #\( "service")
                   (parse-token stream)
                 (expect-char stream #\) "service")))
         (ret  (parse-token stream))
         (out  (prog2 (expect-char stream #\( "service")
                   (parse-token stream)
                 (expect-char stream #\) "service")))
         (opts (let ((opts (parse-proto-method-options stream)))
                 (when (or (null opts) (eql (peek-char nil stream nil) #\;))
                   (expect-char stream #\; "service"))
                 (maybe-skip-comments stream)
                 opts))
         (method (make-instance 'protobuf-method
                   :class (proto->class-name name *protobuf-package*)
                   :name  name
                   :input-type  (proto->class-name in *protobuf-package*)
                   :input-name  in
                   :output-type (proto->class-name out *protobuf-package*)
                   :output-name out
                   :options opts)))
    (let ((name (find-option method "lisp_name")))
      (when name
        (setf (proto-function method) (make-lisp-symbol name))))
    (assert (string= ret "returns") ()
            "Syntax error in 'message' at position ~D" (file-position stream))
    (setf (proto-methods service) (nconc (proto-methods service) (list method)))))

(defun parse-proto-method-options (stream)
  "Parse any options in a Protobufs method from 'stream'.
   Returns a list of 'protobuf-option' objects."
  (when (eql (peek-char nil stream nil) #\{)
    (expect-char stream #\{ "service")
    (maybe-skip-comments stream)
    (with-collectors ((options collect-option))
      (loop
        (when (eql (peek-char nil stream nil) #\})
          (return))
        (assert (string= (parse-token stream) "option") ()
                "Syntax error in 'message' at position ~D" (file-position stream))
        (collect-option (parse-proto-option stream nil #\;)))
      (expect-char stream #\} "service")
      (maybe-skip-comments stream)
      options)))
