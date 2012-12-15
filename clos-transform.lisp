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


;;; Protocol buffer generation from ordinary CLOS classes

;; Controls whether or not to use ':alias-for' for the Protobuf generated
;; for an existing Lisp class
;; The default is presently true because, at least initially, we'll be using
;; the generated Protobufs code in the Lisp world that includes that classes
;; from which the code was generated
(defvar *alias-existing-classes* t)

;; Doing this can't really work perfectly, there's not enough information
;;  - How do we decide if there's an ownership hierarchy that should produce embedded messages?
;;  - How do we decide if there are volatile slots that should not be included in the message?
(defun write-schema-for-classes (classes
                                 &key (stream *standard-output*) (type :proto)
                                      name package lisp-package install
                                      slot-filter type-filter enum-filter value-filter
                                      (alias-existing-classes *alias-existing-classes*))
  "Given a set of CLOS classes, generates a Protobufs schema for the classes
   and pretty prints the schema to the stream.
   The return value is the schema."
  (let ((schema (generate-schema-for-classes classes
                  :name name
                  :package package
                  :lisp-package (or lisp-package package)
                  :install install
                  :slot-filter slot-filter
                  :type-filter type-filter
                  :enum-filter enum-filter
                  :value-filter value-filter
                  :alias-existing-classes alias-existing-classes)))
    (fresh-line stream)
    (write-schema schema :stream stream :type type)
    (terpri stream)
    schema))

(defun generate-schema-for-classes (classes
                                    &key name package lisp-package install
                                         slot-filter type-filter enum-filter value-filter
                                         (alias-existing-classes *alias-existing-classes*))
  "Given a set of CLOS classes, generates a Protobufs schema for the classes.
   The return value is the schema."
  (let* ((*alias-existing-classes* alias-existing-classes)
         (package  (and package (if (stringp package) package (string-downcase (string package)))))
         (lisp-pkg (string (or lisp-package package)))
         (schema   (make-instance 'protobuf-schema
                     :name name
                     :package package
                     :lisp-package lisp-pkg
                     :syntax "proto2"))
         (*protobuf* schema)
         (*protobuf-package* (or (find-proto-package lisp-pkg) *package*))
         (messages (mapcar #'(lambda (c)
                               (class-to-protobuf-message c schema
                                :slot-filter slot-filter
                                :type-filter type-filter
                                :enum-filter enum-filter
                                :value-filter value-filter))
                           classes)))
    (setf (proto-messages schema) messages)
    (when install
      (record-protobuf schema)
      (with-collectors ((messages collect-message))
        (labels ((collect-messages (message)
                   (collect-message message)
                   (map () #'collect-messages (proto-messages message))))
          (map () #'collect-messages (proto-messages schema)))
        (map () #'record-protobuf messages)))
    schema))


(defun class-to-protobuf-message (class schema
                                  &key slot-filter type-filter enum-filter value-filter)
  "Given a CLOS class, return a Protobufs model object for it."
  (let* ((class (let ((c (find-class class)))
                  (unless (class-finalized-p c)
                    (finalize-inheritance c))           ;so the rest of the MOP will work
                  c))
         (slots (class-slots class)))
    (with-collectors ((enums  collect-enum)
                      (msgs   collect-msg)
                      (fields collect-field))
      (loop with index = 1
            for s in slots doing
        (multiple-value-bind (field msg enum)
            (slot-to-protobuf-field class s index slots
              :slot-filter slot-filter
              :type-filter type-filter
              :enum-filter enum-filter
              :value-filter value-filter)
          (when enum
            (collect-enum enum))
          (when msg
            (collect-msg msg))
          (when field
            (incf index 1)                              ;don't worry about the 19000-19999 restriction
            (collect-field field))))
      (let* ((cname (class-name class))
             (pname (class-name->proto cname))
             (message
              ;;--- Making the message this late means its children won't
              ;;--- have the right qualified names
              (make-instance 'protobuf-message
                :class cname
                :name  pname
                :qualified-name (make-qualified-name *protobuf* pname)
                :parent schema
                :alias-for (and *alias-existing-classes* cname)
                :enums    (delete-duplicates enums :key #'proto-name :test #'string=)
                :messages (delete-duplicates msgs :key #'proto-name :test #'string=)
                :fields   fields))
             (*protobuf* message))
        ;; Give every child a proper parent
        (dolist (enum (proto-enums message))
          (setf (proto-parent enum) message))
        (dolist (msg (proto-messages message))
          (setf (proto-parent msg) message))
        (dolist (field (proto-fields message))
          (setf (proto-parent field) message))
        message))))

;; Returns a field, (optionally) an inner message, and (optionally) an inner enum
(defun slot-to-protobuf-field (class slot index slots
                               &key slot-filter type-filter enum-filter value-filter)
  "Given a CLOS slot, return a Protobufs model object for it."
  (when (or (null slot-filter)
            (funcall slot-filter slot slots))
    (multiple-value-bind (expanded-type unexpanded-type)
       (find-slot-definition-type class slot)
      (multiple-value-bind (type pclass packed enums)
          (clos-type-to-protobuf-type expanded-type type-filter enum-filter)
        (multiple-value-bind (reqd vectorp)
            (clos-type-to-protobuf-required (find-slot-definition-type class slot) type-filter)
          (let* ((ename (and enums
                             (if (and unexpanded-type (symbolp unexpanded-type))
                               (symbol-name unexpanded-type)
                               (format nil "~A-~A" 'enum (slot-definition-name slot)))))
                 (etype (and enums
                             (if (and unexpanded-type (symbolp unexpanded-type))
                               unexpanded-type
                               (intern ename (symbol-package (slot-definition-name slot))))))
                 (enum  (and enums
                             (let* ((names (mapcar #'enum-name->proto enums))
                                    (prefix (and (> (length names) 1)
                                                 (subseq (first names)
                                                         0 (mismatch (first names) (car (last names)))))))
                               (when (and prefix (> (length prefix) 2)
                                          (every #'(lambda (name) (starts-with name prefix)) names))
                                 (setq names (mapcar #'(lambda (name) (subseq name (length prefix))) names)))
                               (unless (and unexpanded-type (symbolp unexpanded-type))
                                 #+ignore         ;this happens constantly, the warning is not useful
                                 (protobufs-warn "Use DEFTYPE to define a MEMBER type instead of directly using ~S"
                                                 expanded-type))
                               (let* ((pname (class-name->proto ename))
                                      (enum
                                       (make-instance 'protobuf-enum
                                         :class etype
                                         :name  pname
                                         :qualified-name (make-qualified-name *protobuf* pname)
                                         :parent *protobuf*))
                                      (values
                                       (loop for name in names
                                             for val in enums
                                             for index upfrom 0
                                             collect (make-instance 'protobuf-enum-value
                                                       :name name
                                                       :qualified-name (make-qualified-name enum name)
                                                       :index index
                                                       :value val
                                                       :parent enum))))
                                 (setf (proto-values enum) values)
                                 enum))))
                 (default (if (slot-definition-initfunction slot)
                            (clos-init-to-protobuf-default
                             (slot-definition-initform slot) expanded-type value-filter)
                            (if (eq reqd :repeated)
                              (if vectorp $empty-vector $empty-list)
                              $empty-default)))
                 (field   (make-instance 'protobuf-field
                            :name  (slot-name->proto (slot-definition-name slot))
                            :type  (if enum (class-name->proto ename) type)
                            :class (if enum etype pclass)
                            :required reqd
                            :index  index
                            :value  (slot-definition-name slot)
                            :reader (let ((reader (find-slot-definition-reader class slot)))
                                      ;; Only use the reader if it is "interesting"
                                      (unless (string= (symbol-name reader)
                                                       (format nil "~A-~A" 
                                                               (class-name class) (slot-definition-name slot)))
                                        reader))
                            :default default
                            :packed  packed)))
            (values field nil enum)))))))

(defun list-of-list-of ()
  (let ((list-of-package (find-package 'list-of)))
    (and list-of-package (find-symbol (string 'list-of) list-of-package))))

(defun find-slot-definition-type (class slotd)
  "Given a class and a slot descriptor, find the \"best\" type definition for the slot."
  (let* ((slot-name    (slot-definition-name slotd))
         (direct-slotd (some #'(lambda (c)
                                 (find slot-name (class-direct-slots c) :key #'slot-definition-name))
                             (class-precedence-list class))))
    (if direct-slotd
      ;; The direct slotd will have an unexpanded definition
      ;; Prefer it for 'list-of' so we can get the base type
      (let ((type (slot-definition-type direct-slotd)))
        (values (if (and (listp type)
                         (or (member (car type) '(list-of vector-of))
                             (let ((list-of-list-of (list-of-list-of)))
                               (and list-of-list-of (eq (car type) list-of-list-of)))))
                  type
                  (slot-definition-type slotd))
                (if (symbolp type)
                  type
                  (when (and (listp type)
                             (eq (car type) 'or)
                             (member 'null (cdr type)))
                    (find-if-not #'(lambda (s) (eq s 'null)) (cdr type))))))
      (values (slot-definition-type slotd) nil))))

(defun find-slot-definition-reader (class slotd)
  "Given a class and a slot descriptor, find the name of a reader method for the slot."
  (let* ((slot-name    (slot-definition-name slotd))
         (direct-slotd (some #'(lambda (c)
                                 (find slot-name (class-direct-slots c) :key #'slot-definition-name))
                             (class-precedence-list class))))
    (and direct-slotd (first (slot-definition-readers direct-slotd)))))

(defun satisfies-list-of-p (type)
  (and (consp type)
       (eq (car type) 'satisfies)
       (consp (cdr type))
       (null (cddr type))
       (let ((function (cadr type)))
         (and (symbolp function)
              (string= "LIST-OF" (package-name (symbol-package function)))
              (let ((name (symbol-name function)))
                (and (<= #.(length "LIST-OF-_-P") (length name))
                     (starts-with name "LIST-OF-")
                     (ends-with name "-P")
                     (let* ((typestring (subseq name #.(length "LIST-OF-") (- (length name) 2)))
                            (type (ignore-errors
                                    (with-standard-io-syntax
                                        (let ((*package* (find-package :cl)))
                                          (read-from-string typestring))))))
                         (and (typep type 'symbol) type))))))))

(defun clos-type-to-protobuf-type (type &optional type-filter enum-filter)
  "Given a Lisp type, returns a Protobuf type, a class or primitive type,
   whether or not to pack the field, and (optionally) a set of enum values."
  (let* ((type (if type-filter (funcall type-filter type) type))
         (list-of-list-of (list-of-list-of))
         (type-enum (when (and *protobuf* (symbolp type))
                      (find-enum *protobuf* type)))
         (expanded-type (type-expand type)))
    (cond
      ((listp type)
        (destructuring-bind (head &rest tail) type
          (case head
            ((or)
             (when (or (> (length tail) 2)
                       (not (member 'null tail)))
               (protobufs-warn "The OR type ~S is too complicated, proceeding anyway" type))
             (if (eq (first tail) 'null)
               (clos-type-to-protobuf-type (second tail))
               (clos-type-to-protobuf-type (first tail))))
            ((and)
             ;; Special knowledge of 'list-of:list-of', which uses (and list (satisfies list-of::FOO-p))
             (let ((satisfies-list-of
                    (and list-of-list-of (find-if #'satisfies-list-of-p tail))))
               (if satisfies-list-of
                 (multiple-value-bind (type class)
                     (lisp-type-to-protobuf-type satisfies-list-of)
                   (values type class (packed-type-p class)))
                 (let ((new-tail
                        (remove-if #'(lambda (x) (and (listp x) (eq (car x) 'satisfies))) tail)))
                   (when (> (length new-tail) 1)
                     (protobufs-warn "The AND type ~S is too complicated, proceeding anyway" type))
                   (lisp-type-to-protobuf-type (first tail))))))
            ((member)                           ;maybe generate an enum type
             (if (or (equal type '(member t nil))
                     (equal type '(member nil t)))
               (values "bool" :bool)
               (let ((values (if enum-filter (funcall enum-filter tail) tail)))
                 (cond ((every #'(lambda (x)
                                   (or (null x) (characterp x) (stringp x))) values)
                        (values "string" :string))
                       ((every #'(lambda (x)
                                   (or (null x) (and (integerp x) (>= x 0)))) values)
                        (values "uint32" :uint32))
                       ((every #'(lambda (x)
                                   (or (null x) (integerp x))) values)
                        (values "int32" :int32))
                       ((every #'(lambda (x) (symbolp x)) values)
                        (let ((values (remove-if #'null values)))
                          (values (class-name->proto (format nil "~A" type))
                                  type
                                  nil           ;don't pack enums
                                  (if enum-filter (funcall enum-filter values) values))))
                       (t
                        (error "The MEMBER type ~S is too complicated" type))))))
            ((list-of vector-of)
             (multiple-value-bind (type class)
                 (lisp-type-to-protobuf-type (first tail))
               (values type class (packed-type-p class))))
            ((integer)
             (let ((lo (or (first tail) '*))
                   (hi (or (second tail) '*)))
               (if (or (eq lo '*) (< lo 0))
                 (if (eq hi '*)
                   (values "int64" :int64)
                   (if (<= (integer-length hi) 32)
                     (values "int32" :int32)
                     (values "int64" :int64)))
                 (if (eq hi '*)
                   (values "uint64" :uint64)
                   (if (<= (integer-length hi) 32)
                     (values "uint32" :uint32)
                     (values "uint64" :uint64))))))
            ((signed-byte)
             (let ((len (first tail)))
               (if (<= len 32)
                 (values "int32" :int32)
                 (values "int64" :int64))))
            ((unsigned-byte)
             (let ((len (first tail)))
               (if (<= len 32)
                 (values "uint32" :uint32)
                 (values "uint64" :uint64))))
            ((float single-float double-float)
             (lisp-type-to-protobuf-type head))
            (otherwise
             (if (eq head list-of-list-of)
               (multiple-value-bind (type class)
                   (lisp-type-to-protobuf-type (first tail))
                 (values type class (packed-type-p class)))
               (lisp-type-to-protobuf-type type))))))
      ((not (or type-enum (equal type expanded-type)))
       (clos-type-to-protobuf-type expanded-type))
      (t
       (lisp-type-to-protobuf-type type)))))

(defun lisp-type-to-protobuf-type (type)
  (case type
    ((int32)    (values "int32" :int32))
    ((int64)    (values "int64" :int64))
    ((uint32)   (values "uint32" :uint32))
    ((uint64)   (values "uint64" :uint64))
    ((sint32)   (values "sint32" :sint32))
    ((sint64)   (values "sint64" :sint64))
    ((fixed32)  (values "fixed32" :fixed32))
    ((fixed64)  (values "fixed64" :fixed64))
    ((sfixed32) (values "sfixed32" :sfixed32))
    ((sfixed64) (values "sfixed64" :sfixed64))
    ((integer)  (values "int64" :int64))
    ((single-float float)
     (values "float" :float))
    ((double-float)
     (values "double" :double))
    ((boolean)
     (values "bool" :bool))
    ((symbol keyword)
     (values "string" :symbol))
    (otherwise
     (cond ((ignore-errors
             (or (eql type 'symbol)
                 (subtypep type '(or string character))))
            (values "string" :string))
           ((ignore-errors
             (subtypep type 'byte-vector))
            (values "bytes" :bytes))
           (t
            (values (class-name->proto type) type))))))

(defun packed-type-p (type)
  "Returns true if the given Protobufs type can use a packed field."
  (not (null (member type '(:int32 :int64 :uint32 :uint64 :sint32 :sint64
                            :fixed32 :fixed64 :sfixed32 :sfixed64
                            :bool :float :double)))))

(defun clos-type-to-protobuf-required (type &optional type-filter)
  "Given a Lisp type, returns a \"cardinality\": :required, :optional or :repeated.
   If the sceond returned value is true, it's a repeated field that should use a vector."
  (let ((type (if type-filter (funcall type-filter type) type))
        (list-of-list-of (list-of-list-of)))
    (if (listp type)
      (destructuring-bind (head &rest tail) type
        (case head
          ((or)
           (let ((optional (member 'null tail))
                 (repeated (find-if #'(lambda (r)
                                        (eq (clos-type-to-protobuf-required r) :repeated)) tail)))
             (if repeated
               (clos-type-to-protobuf-required repeated)
               (values (if optional :optional :required) nil))))
          ((and)
           (cond ((and (subtypep type 'list)
                       (not (subtypep type 'null)))
                  (values :repeated nil))
                 ((subtypep type '(vector-of t))
                  (values :repeated t))
                 (t
                  (values :required nil))))
          ((member)
           (if (or (equal type '(member t nil))
                   (equal type '(member nil t)))
             (values :required nil)
             (values (if (member nil tail) :optional :required) nil)))
          ((list-of)
           (values :repeated nil))
          ((vector-of)
           (values :repeated t))
          (otherwise
           (if (eq head list-of-list-of)
             (values :repeated nil)
             (values :required nil)))))
      (values :required nil))))

(defun clos-init-to-protobuf-default (value type &optional value-filter)
  "Given an initform and a Lisp type, returns a plausible default value.
   Don't call this if the default is empty, because that will confuse 'nil' with 'unbound'."
  (let ((value (if value-filter (funcall value-filter value) value)))
    (and (constantp value)
         (ignore-errors (typep value type))
         (values value t))))

(defun protobuf-default-to-clos-init (default type)
  "Given a Protobufs type and default, return a CLOS initform value.
   Don't call this if the default is empty, because that will confuse 'nil' with 'unbound'."
  (cond ((ignore-errors (typep default type))
         default)
        ((symbolp default)
         (cond ((eq type :bool)
                (boolean-true-p default))
               ;; If we've got a symbol, it must be to initialize an enum type
               ;; whose values are represented by keywords in Lisp
               (t (kintern (symbol-name default)))))
        ((stringp default)
         (cond ((eq type :bool)
                (boolean-true-p default))
               ((member type '(:int32 :uint32 :int64 :uint64 :sint32 :sint64
                               :fixed32 :sfixed32 :fixed64 :sfixed64))
                (let ((default (read-from-string default)))
                  (and (integerp default) default)))
               ((member type '(:float :double))
                (let ((default (read-from-string default)))
                  (and (floatp default) default)))
               (t default)))))

(defun boolean-true-p (x)
  "Returns t or nil given a value that might be a boolean."
  (etypecase x
    ((member t nil) x)
    (integer   (not (eql x 0)))
    (character (char-equal x #\t))
    (string    (or (string-equal x "true")
                   (string-equal x "yes")
                   (string-equal x "t")
                   (string-equal x "1")))
    (symbol    (string-equal (string x) "true"))))
