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

(defun expect-char (stream char &optional chars within)
  "Expect to see 'char' as the next character in the stream; signal an error if it's not there.
   Then skip all of the following whitespace.
   The return value is the character that was eaten."
  (let (ch)
    (if (if (listp char)
          (member (peek-char nil stream nil) char)
          (eql (peek-char nil stream nil) char))
      (setq ch (read-char stream))
      (error "No '~C' found~@[ within '~A'~] at position ~D"
             char within (file-position stream)))
    (maybe-skip-chars stream chars)
    ch))

(defun maybe-skip-chars (stream chars)
  "Skip some optional characters in the stream,
   then skip all of the following whitespace."
  (skip-whitespace stream)
  (when chars
    (loop
      (let ((ch (peek-char nil stream nil)))
        (when (or (null ch) (not (member ch chars)))
          (skip-whitespace stream)
          (return-from maybe-skip-chars)))
      (read-char stream))))


;;--- Collect the comment so we can attach it to its associated object
(defun maybe-skip-comments (stream)
  "If what appears next in the stream is a comment, skip it and any following comments,
   then skip any following whitespace."
  (loop
    (let ((ch (peek-char nil stream nil)))
      (when (or (null ch) (not (eql ch #\/)))
        (return-from maybe-skip-comments))
      (read-char stream)
      (case (peek-char nil stream nil)
        ((#\/)
         (skip-line-comment stream))
        ((#\*)
         (skip-block-comment stream))
        ((nil)
         (return-from maybe-skip-comments))
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


(defun parse-token (stream &optional additional-chars)
  "Parse the next token in the stream, then skip the following whitespace.
   The returned value is the token."
  (when (let ((ch (peek-char nil stream nil)))
          (or (proto-token-char-p ch) (member ch additional-chars)))
    (loop for ch = (read-char stream nil)
          for ch1 = (peek-char nil stream nil)
          collect ch into token
          until (or (null ch1)
                    (and (not (proto-token-char-p ch1))
                         (not (member ch1 additional-chars))))
          finally (progn
                    (skip-whitespace stream)
                    (return (coerce token 'string))))))

(defun parse-parenthesized-token (stream)
  "Parse the next token in the stream, then skip the following whitespace.
   The token might be surrounded by parentheses.
   The returned value is the token."
  (let ((left (peek-char nil stream nil)))
    (when (eql left #\()
      (read-char stream))
    (when (proto-token-char-p (peek-char nil stream nil))
      (loop for ch = (read-char stream nil)
            for ch1 = (peek-char nil stream nil)
            collect ch into token
            until (or (null ch1) (not (proto-token-char-p ch1)))
            finally (progn
                      (skip-whitespace stream)
                      (when (eql left #\()
                        (expect-char stream #\)))
                      (return (coerce token 'string)))))))

(defun parse-token-or-string (stream)
  (if (eql (peek-char nil stream nil) #\")
    (values (parse-string stream) 'string)
    (values (parse-token stream) 'symbol)))

(defun parse-string (stream)
  "Parse the next quoted string in the stream, then skip the following whitespace.
   The returned value is the string, without the quotation marks."
  (loop with ch0 = (read-char stream nil)
        for ch = (read-char stream nil)
        until (or (null ch) (char= ch ch0))
        when (eql ch #\\)
          do (setq ch (unescape-char stream))
        collect ch into string
        finally (progn
                  (skip-whitespace stream)
                  (return (coerce string 'string)))))

(defun unescape-char (stream)
  "Parse the next \"escaped\" character from the stream."
  (let ((ch (read-char stream nil)))
    (assert (not (null ch)) ()
            "End of stream reached while reading escaped character")
    (case ch
      ((#\x)
       ;; Two hex digits
       (let* ((d1 (digit-char-p (read-char stream) 16))
              (d2 (digit-char-p (read-char stream) 16)))
         (code-char (+ (* d1 16) d2))))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (if (not (digit-char-p (peek-char nil stream nil)))
         #\null
         ;; Three octal digits
         (let* ((d1 (digit-char-p ch 8))
                (d2 (digit-char-p (read-char stream) 8))
                (d3 (digit-char-p (read-char stream) 8)))
           (code-char (+ (* d1 64) (* d2 8) d3)))))
      ((#\t) #\tab)
      ((#\n) #\newline)
      ((#\r) #\return)
      ((#\f) #\page)
      ((#\b) #\backspace)
      ((#\a) #\bell)
      ((#\e) #\esc)
      (otherwise ch))))

(defun escape-char (ch)
  "The inverse of 'unescape-char', for printing."
  (if (and (standard-char-p ch) (graphic-char-p ch))
    ch
    (case ch
      ((#\null)      "\\0")
      ((#\tab)       "\\t")
      ((#\newline)   "\\n")
      ((#\return)    "\\r")
      ((#\page)      "\\f")
      ((#\backspace) "\\b")
      ((#\bell)      "\\a")
      ((#\esc)       "\\e")
      (otherwise
       (format nil "\\x~2,'0X" (char-code ch))))))

(defun parse-signed-int (stream)
  "Parse the next token in the stream as an integer, then skip the following whitespace.
   The returned value is the integer."
  (let* ((sign (if (eql (peek-char nil stream nil) #\-)
                 (progn (read-char stream) -1)
                 1))
         (int  (parse-unsigned-int stream)))
    (* int sign)))

(defun parse-unsigned-int (stream)
  "Parse the next token in the stream as an integer, then skip the following whitespace.
   The returned value is the integer."
  (when (digit-char-p (peek-char nil stream nil))
    (loop for ch = (read-char stream nil)
          for ch1 = (peek-char nil stream nil)
          collect ch into token
          until (or (null ch1) (and (not (digit-char-p ch1)) (not (eql ch #\x))))
          finally (progn
                    (skip-whitespace stream)
                    (let ((token (coerce token 'string)))
                      (if (starts-with token "0x")
                        (let ((*read-base* 16))
                          (return (parse-integer (subseq token 2))))
                        (return (parse-integer token))))))))

(defun parse-float (stream)
  "Parse the next token in the stream as a float, then skip the following whitespace.
   The returned value is the float."
  (let ((number (parse-number stream)))
    (when number
      (coerce number 'float))))

(defun parse-number (stream)
  (when (let ((ch (peek-char nil stream nil)))
          (or (digit-char-p ch) (member ch '(#\- #\+ #\.))))
    (let ((token (parse-token stream '(#\- #\+ #\.))))
      (when token
        (skip-whitespace stream)
        (cond ((starts-with token "0x")
               (parse-integer (subseq token 2) :radix 16))
              ((starts-with token "-0x")
               (- (parse-integer (subseq token 3) :radix 16)))
              (t
               (read-from-string token)))))))


;;; The parser itself

(defun parse-protobuf-from-file (filename)
  "Parses the named file as a .proto file, and returns the Protobufs schema."
  (with-open-file (stream filename
                   :direction :input
                   :external-format :utf-8
                   :element-type 'character)
    (let ((*compile-file-pathname* (pathname stream))
          (*compile-file-truename* (truename stream)))
      (parse-protobuf-from-stream stream
                                  :name  (class-name->proto (pathname-name (pathname stream)))
                                  :class (kintern (pathname-name (pathname stream)))))))

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
         (*protobuf-package* *package*))
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
                          (when (and option (option-name= name "lisp_package"))
                            (let ((package (or (find-package value)
                                               (find-package (string-upcase value))
                                               *protobuf-package*)))
                              (setf (proto-lisp-package protobuf) value)
                              (setq *protobuf-package* package)))))
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
  (let ((syntax (prog2 (expect-char stream #\= () "syntax")
                    (parse-string stream)
                  (expect-char stream terminator () "syntax")
                  (maybe-skip-comments stream))))
    (setf (proto-syntax protobuf) syntax)))

(defun parse-proto-package (stream protobuf &optional (terminator #\;))
  "Parse a Protobufs package line from 'stream'.
   Updates the 'protobuf' object to use the package."
  (check-type protobuf protobuf)
  (let* ((package  (prog1 (parse-token stream)
                     (expect-char stream terminator () "package")
                     (maybe-skip-comments stream)))
         (lisp-pkg (or (proto-lisp-package protobuf)
                       (substitute #\- #\_ package))))
    (setf (proto-package protobuf) package)
    (unless (proto-lisp-package protobuf)
      (setf (proto-lisp-package protobuf) lisp-pkg))
    (let ((package (or (find-package lisp-pkg)
                       (find-package (string-upcase lisp-pkg))
                       *protobuf-package*)))
      (setq *protobuf-package* package))))

(defun parse-proto-import (stream protobuf &optional (terminator #\;))
  "Parse a Protobufs import line from 'stream'.
   Updates the 'protobuf' object to use the package."
  (check-type protobuf protobuf)
  (let ((import (prog1 (parse-string stream)
                  (expect-char stream terminator () "package")
                  (maybe-skip-comments stream))))
    (process-imports protobuf import)
    (setf (proto-imports protobuf) (nconc (proto-imports protobuf) (list import)))))

(defun parse-proto-option (stream protobuf &optional (terminators '(#\;)))
  "Parse a Protobufs option line from 'stream'.
   Updates the 'protobuf' (or message, service, method) to have the option."
  (check-type protobuf (or null base-protobuf))
  (let* (terminator
         (key (prog1 (parse-parenthesized-token stream)
                (expect-char stream #\= () "option")))
         (val (prog1 (let ((ch (peek-char nil stream nil)))
                       (cond ((eql ch #\")
                              (parse-string stream))
                             ((or (digit-char-p ch) (member ch '(#\- #\+ #\.)))
                              (parse-number stream))
                             (t (parse-token stream))))
                (setq terminator (expect-char stream terminators () "option"))
                (maybe-skip-comments stream)))
         (option (make-instance 'protobuf-option
                   :name  key
                   :value val)))
    (cond (protobuf
           (setf (proto-options protobuf) (nconc (proto-options protobuf) (list option)))
           (values option terminator))
          (t
           ;; If nothing to graft the option into, just return it as the value
           (values option terminator)))))


(defun parse-proto-enum (stream protobuf)
  "Parse a Protobufs 'enum' from 'stream'.
   Updates the 'protobuf' or 'protobuf-message' object to have the enum."
  (check-type protobuf (or protobuf protobuf-message))
  (let* ((name (prog1 (parse-token stream)
                 (expect-char stream #\{ () "enum")
                 (maybe-skip-comments stream)))
         (enum (make-instance 'protobuf-enum
                 :class (proto->class-name name *protobuf-package*)
                 :name name)))
    (loop
      (let ((name (parse-token stream)))
        (when (null name)
          (expect-char stream #\} '(#\;) "enum")
          (maybe-skip-comments stream)
          (setf (proto-enums protobuf) (nconc (proto-enums protobuf) (list enum)))
          (let ((type (find-option enum "lisp_name")))
            (when type
              (setf (proto-class enum) (make-lisp-symbol type))))
          (let ((alias (find-option enum "lisp_alias")))
            (when alias
              (setf (proto-alias-for enum) (make-lisp-symbol alias))))
          (return-from parse-proto-enum enum))
        (if (string= name "option")
          (parse-proto-option stream enum)
          (parse-proto-enum-value stream enum name))))))

(defun parse-proto-enum-value (stream enum name)
  "Parse a Protobufs enum value from 'stream'.
   Updates the 'protobuf-enum' object to have the enum value."
  (check-type enum protobuf-enum)
  (expect-char stream #\= () "enum")
  (let* ((idx  (prog1 (parse-signed-int stream)
                 (expect-char stream #\; () "enum")
                 (maybe-skip-comments stream)))
         (value (make-instance 'protobuf-enum-value
                  :name  name
                  :index idx
                  :value (proto->enum-name name *protobuf-package*))))
    (setf (proto-values enum) (nconc (proto-values enum) (list value)))
    value))


(defun parse-proto-message (stream protobuf &optional name)
  "Parse a Protobufs 'message' from 'stream'.
   Updates the 'protobuf' or 'protobuf-message' object to have the message."
  (check-type protobuf (or protobuf protobuf-message))
  (let* ((name (prog1 (or name (parse-token stream))
                 (expect-char stream #\{ () "message")
                 (maybe-skip-comments stream)))
         (message (make-instance 'protobuf-message
                    :class (proto->class-name name *protobuf-package*)
                    :name name
                    :parent protobuf))
         (*protobuf* message))
    (loop
      (let ((token (parse-token stream)))
        (when (null token)
          (expect-char stream #\} '(#\;) "message")
          (maybe-skip-comments stream)
          (setf (proto-messages protobuf) (nconc (proto-messages protobuf) (list message)))
          (let ((type (find-option message "lisp_name")))
            (when type
              (setf (proto-class message) (make-lisp-symbol type))))
          (let ((alias (find-option message "lisp_alias")))
            (when alias
              (setf (proto-alias-for message) (make-lisp-symbol alias))))
          (return-from parse-proto-message message))
        (cond ((string= token "enum")
               (parse-proto-enum stream message))
              ((string= token "extend")
               (parse-proto-extend stream message))
              ((string= token "message")
               (parse-proto-message stream message))
              ((member token '("required" "optional" "repeated") :test #'string=)
               (parse-proto-field stream message token))
              ((string= token "option")
               (parse-proto-option stream message))
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
                 (expect-char stream #\{ () "extend")
                 (maybe-skip-comments stream)))
         (message (find-message protobuf name))
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
                         :extensions (copy-list (proto-extensions message))
                         :message-type :extends)))      ;this message is an extension
         (*protobuf* extends))
    (loop
      (let ((token (parse-token stream)))
        (when (null token)
          (expect-char stream #\} '(#\;) "extend")
          (maybe-skip-comments stream)
          (setf (proto-messages protobuf) (nconc (proto-messages protobuf) (list extends)))
          (setf (proto-extenders protobuf) (nconc (proto-extenders protobuf) (list extends)))
          (let ((type (find-option extends "lisp_name")))
            (when type
              (setf (proto-class extends) (make-lisp-symbol type))))
          (let ((alias (find-option extends "lisp_alias")))
            (when alias
              (setf (proto-alias-for extends) (make-lisp-symbol alias))))
          (return-from parse-proto-extend extends))
        (cond ((member token '("required" "optional" "repeated") :test #'string=)
               (let ((field (parse-proto-field stream extends token message)))
                 (setf (proto-extended-fields extends) (nconc (proto-extended-fields extends) (list field)))))
              ((string= token "option")
               (parse-proto-option stream extends))
              (t
               (error "Unrecognized token ~A at position ~D"
                      token (file-position stream))))))))

(defun parse-proto-field (stream message required &optional extended-from)
  "Parse a Protobufs field from 'stream'.
   Updates the 'protobuf-message' object to have the field."
  (check-type message protobuf-message)
  (let ((type (parse-token stream)))
    (if (string= type "group")
      (parse-proto-group stream message required extended-from)
      (let* ((name (prog1 (parse-token stream)
                     (expect-char stream #\= () "message")))
             (idx  (parse-unsigned-int stream))
             (opts (prog1 (parse-proto-field-options stream)
                     (expect-char stream #\; () "message")
                     (maybe-skip-comments stream)))
             (default (find-option opts "default"))
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
                       :default default
                       :packed  (and packed (boolean-true-p packed))
                       :message-type (proto-message-type message)
                       :options opts)))
        (when extended-from
          (assert (index-within-extensions-p idx extended-from) ()
                  "The index ~D is not in range for extending ~S"
                  idx (proto-class extended-from)))
        (let ((slot (find-option opts "lisp_name")))
          (when slot
            (setf (proto-value field) (make-lisp-symbol type))))
        (setf (proto-fields message) (nconc (proto-fields message) (list field)))
        field))))

(defun parse-proto-group (stream message required &optional extended-from)
  "Parse a (deprecated) Protobufs group from 'stream'.
   Updates the 'protobuf-message' object to have the group type and field."
  (check-type message protobuf-message)
  (let* ((type (prog1 (parse-token stream)
                 (expect-char stream #\= () "message")))
         (name (slot-name->proto (proto->slot-name type)))
         (idx  (parse-unsigned-int stream))
         (msg  (parse-proto-message stream message type))
         (class  (proto->class-name type *protobuf-package*))
         (field  (make-instance 'protobuf-field
                   :name  name
                   :value (proto->slot-name name *protobuf-package*)
                   :type  type
                   :class class
                   ;; One of :required, :optional or :repeated
                   :required (kintern required)
                   :index idx
                   :message-type :group)))
    (setf (proto-message-type msg) :group)
    (when extended-from
      (assert (index-within-extensions-p idx extended-from) ()
              "The index ~D is not in range for extending ~S"
              idx (proto-class extended-from)))
    (setf (proto-fields message) (nconc (proto-fields message) (list field)))
    field))

(defun parse-proto-field-options (stream)
  "Parse any options in a Protobufs field from 'stream'.
   Returns a list of 'protobuf-option' objects."
  (with-collectors ((options collect-option))
    (let ((terminator nil))
      (loop
        (cond ((eql (peek-char nil stream nil) #\[)
               (expect-char stream #\[ () "message"))
              ((eql terminator #\,))
              (t
               (return-from parse-proto-field-options options)))
        (multiple-value-bind (option term)
            (parse-proto-option stream nil '(#\] #\,))
          (setq terminator term)
          (collect-option option))))
    options))

(defun parse-proto-extension (stream message)
  (check-type message protobuf-message)
  (let* ((from  (parse-unsigned-int stream))
         (token (parse-token stream))
         (to    (let ((ch (peek-char nil stream nil)))
                  (cond ((digit-char-p (peek-char nil stream nil))
                         (parse-unsigned-int stream))
                        ((eql ch #\;) from)
                        (t (parse-token stream))))))
    (expect-char stream #\; () "message")
    (assert (or (null token) (string= token "to")) ()
            "Expected 'to' in 'extensions' at position ~D" (file-position stream))
    (assert (or (integerp to) (string= to "max")) ()
            "Extension value is not an integer or 'max' as position ~D" (file-position stream))
    (let ((extension (make-instance 'protobuf-extension
                       :from from
                       :to   (if (integerp to) to #.(1- (ash 1 29))))))
      (setf (proto-extensions message)
            (nconc (proto-extensions message)
                   (list extension)))
      extension)))


(defun parse-proto-service (stream protobuf)
  "Parse a Protobufs 'service' from 'stream'.
   Updates the 'protobuf-protobuf' object to have the service."
  (check-type protobuf protobuf)
  (let* ((name (prog1 (parse-token stream)
                 (expect-char stream #\{ () "service")
                 (maybe-skip-comments stream)))
         (service (make-instance 'protobuf-service
                    :class (proto->class-name name *protobuf-package*)
                    :name name)))
    (loop
      (let ((token (parse-token stream)))
        (when (null token)
          (expect-char stream #\} '(#\;) "service")
          (maybe-skip-comments stream)
          (setf (proto-services protobuf) (nconc (proto-services protobuf) (list service)))
          (return-from parse-proto-service service))
        (cond ((string= token "option")
               (parse-proto-option stream service))
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
         (in   (prog2 (expect-char stream #\( () "service")
                   (parse-token stream)
                 (expect-char stream #\) () "service")))
         (ret  (parse-token stream))
         (out  (prog2 (expect-char stream #\( () "service")
                   (parse-token stream)
                 (expect-char stream #\) () "service")))
         (opts (let ((opts (parse-proto-method-options stream)))
                 (when (or (null opts) (eql (peek-char nil stream nil) #\;))
                   (expect-char stream #\; () "service"))
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
    (setf (proto-methods service) (nconc (proto-methods service) (list method)))
    method))

(defun parse-proto-method-options (stream)
  "Parse any options in a Protobufs method from 'stream'.
   Returns a list of 'protobuf-option' objects."
  (when (eql (peek-char nil stream nil) #\{)
    (expect-char stream #\{ () "service")
    (maybe-skip-comments stream)
    (with-collectors ((options collect-option))
      (loop
        (when (eql (peek-char nil stream nil) #\})
          (return))
        (assert (string= (parse-token stream) "option") ()
                "Syntax error in 'message' at position ~D" (file-position stream))
        (collect-option (parse-proto-option stream nil)))
      (expect-char stream #\} '(#\;) "service")
      (maybe-skip-comments stream)
      options)))
