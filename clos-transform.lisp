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


;;; Protocol buffer generation from ordinary CLOS classes

;; Doing this can't really work perfectly, there's not enough information
;;  - How do we decide if there's an ownership hierarchy that should produce embedded messages?
;;  - How do we decide if there are volatile slots that should not be included in the message?
(defun write-protobuf-schema-for-classes (classes
                                          &key (stream *standard-output*) (type :proto) proto-name
                                               package slot-filter type-filter enum-filter value-filter)
  "Given a set of CLOS classes, generates a Protobufs schema for the classes
   and pretty prints the schema to the stream.
   The return value is the schema."
  (let* ((messages (mapcar #'(lambda (c)
                               (class-to-protobuf-message c :slot-filter slot-filter
                                                            :type-filter type-filter
                                                            :enum-filter enum-filter
                                                            :value-filter value-filter))
                           classes))
         (protobuf (make-instance 'protobuf
                     :name proto-name
                     :package (and package (if (stringp package) package (string-downcase (string package))))
                     :messages messages)))
    (when stream
      (fresh-line stream)
      (write-protobuf protobuf :stream stream :type type)
      (terpri stream))
    protobuf))


(defun class-to-protobuf-message (class
                                  &key slot-filter type-filter enum-filter value-filter)
  (let* ((class (find-class class))
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
      (make-instance 'protobuf-message
        :class (class-name class)
        :name  (class-name->proto (class-name class))
        :enums (delete-duplicates enums :key #'proto-name :test #'string-equal)
        :messages (delete-duplicates msgs :key #'proto-name :test #'string-equal)
        :fields fields))))

;; Returns a field, (optionally) an inner message, and (optionally) an inner enum
(defun slot-to-protobuf-field (class slot index slots
                               &key slot-filter type-filter enum-filter value-filter)
  (when (or (null slot-filter)
            (funcall slot-filter slot slots))
    (multiple-value-bind (type pclass packed enums)
        (clos-type-to-protobuf-type (find-slot-definition-type class slot) type-filter enum-filter)
      (let* ((ename (and enums
                         (format nil "~A-~A" 'enum (slot-definition-name slot))))
             (enum  (and enums
                         (let* ((names (mapcar #'enum-name->proto enums))
                                (prefix (and (> (length names) 1)
                                             (subseq (first names)
                                                     0 (mismatch (first names) (second names))))))
                           (when (and prefix (> (length prefix) 2)
                                      (every #'(lambda (name) (starts-with name prefix)) names))
                             (setq names (mapcar #'(lambda (name) (subseq name (length prefix))) names)))
                           (make-instance 'protobuf-enum
                             :class  (intern ename (symbol-package (slot-definition-name slot)))
                             :name   (class-name->proto ename)
                             :values (loop for name in names
                                           for val in enums
                                           for index upfrom 1
                                           collect (make-instance 'protobuf-enum-value
                                                     :name name
                                                     :index index
                                                     :value val))))))
             (reqd  (clos-type-to-protobuf-required (find-slot-definition-type class slot) type-filter))
             (field (make-instance 'protobuf-field
                      :name  (slot-name->proto (slot-definition-name slot))
                      :type  (if enum (class-name->proto ename) type)
                      :class (if enum (intern ename (symbol-package (slot-definition-name slot))) pclass)
                      :required reqd
                      :index index
                      :value   (slot-definition-name slot)
                      :reader  (find-slot-definition-reader class slot)
                      :default (clos-init-to-protobuf-default (slot-definition-initform slot) value-filter)
                      :packed  packed)))
        (values field nil enum)))))

;; Given a class and a slot descriptor, find the unexpanded type definition for the slot
(defun find-slot-definition-type (class slotd)
  (let* ((slot-name    (slot-definition-name slotd))
         (direct-slotd (some #'(lambda (c)
                                 (find slot-name (class-direct-slots c) :key #'slot-definition-name))
                             (class-precedence-list class))))
    (or (and direct-slotd (slot-definition-type slotd))
        (slot-definition-type slotd))))

;; Given a class and a slot descriptor, find the name of a reader method for the slot
(defun find-slot-definition-reader (class slotd)
  (let* ((slot-name    (slot-definition-name slotd))
         (direct-slotd (some #'(lambda (c)
                                 (find slot-name (class-direct-slots c) :key #'slot-definition-name))
                             (class-precedence-list class))))
    (and direct-slotd (first (slot-definition-readers direct-slotd)))))

;; Returns Protobuf type, a class or primitive type, whether or not to pack the field,
;; and (optionally) a set of enum values
(defun clos-type-to-protobuf-type (type &optional type-filter enum-filter)
  (let ((type (if type-filter (funcall type-filter type) type)))
    (flet ((type->protobuf-type (type)
             (case type
               ((boolean)
                (values "bool" :bool))
               ((integer)
                (values "int64" :int64))
               ((float)
                (values "float" :float))
               ((double-float)
                (values "double" :double))
               ((symbol keyword)
                (values "string" :symbol))
               (otherwise
                (if (ignore-errors
                      (subtypep type '(or string character)))
                  (values "string" :string)
                  (values (class-name->proto type) type))))))
      (if (listp type)
        (destructuring-bind (head &rest tail) type
          (case head
            ((or)
             (when (or (> (length tail) 2)
                       (not (member 'null tail)))
               (protobufs-warn "Can't handle the complicated OR type ~S" type))
             (if (eq (first tail) 'null)
               (clos-type-to-protobuf-type (second tail))
               (clos-type-to-protobuf-type (first tail))))
            ((and)
             (if (subtypep type '(list-of t))   ;special knowledge of Quux list-of
               (let ((satisfies (find 'satisfies tail :key #'car)))
                 (let* ((pred (second satisfies))
                        (type (if (starts-with (string pred) "LIST-OF-")
                                (intern (subseq (string pred) #.(length "LIST-OF-")) (symbol-package pred))
                                pred)))
                   (multiple-value-bind (type class)
                       (type->protobuf-type type)
                     (values type class (packed-type-p class)))))
               (let ((new-tail (remove-if #'(lambda (x) (and (listp x) (eq (car x) 'satisfies))) tail)))
                 (assert (= (length new-tail) 1) ()
                         "Can't handle the complicated AND type ~S" type)
                 (type->protobuf-type (first tail)))))
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
                       (t
                        (protobufs-warn "Use DEFTYPE to define a MEMBER type instead of directly using ~S" type)
                        (let ((values (remove-if #'null values)))
                          (values (class-name->proto type)
                                  type
                                  nil           ;don't pack enums
                                  (if enum-filter (funcall enum-filter values) values))))))))
            ((list-of)                          ;special knowledge of Quux list-of
             (multiple-value-bind (type class)
                 (type->protobuf-type (first tail))
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
            ((float double-float)
             (type->protobuf-type head))
            (otherwise
             (if (subtypep head '(or string character))
               (values "string" :string)
               (error "Don't know how to translate the type ~S" head)))))
        (type->protobuf-type type)))))

(defun packed-type-p (class)
  (not (null (member class '(:int32 :int64 :uint32 :uint64 :sint32 :sint64
                             :fixed32 :fixed64 :sfixed32 :sfixed64
                             :float :double)))))

(defun clos-type-to-protobuf-required (type &optional type-filter)
  (let ((type (if type-filter (funcall type-filter type) type)))
    (if (listp type)
      (destructuring-bind (head &rest tail) type
        (case head
          ((or)
           (let ((optional (member 'null (cdr type))))
             (if (loop for r in tail
                       thereis (eq (clos-type-to-protobuf-required r) :repeated))
               :repeated
               (if optional :optional :required))))
          ((and)
           (if (subtypep type '(list-of t))     ;special knowledge of Quux list-of
             :repeated
             :required))
          ((member)
           (if (or (equal type '(member t nil))
                   (equal type '(member nil t)))
             :required
             (if (member nil tail) :optional :required)))
          (list-of
           :repeated)
          (otherwise
           :required)))
      :required)))

(defun clos-init-to-protobuf-default (value &optional value-filter)
  (let ((value (if value-filter (funcall value-filter value) value)))
    (and value (constantp value)
         (format nil "~A" value))))

(defun protobuf-default-to-clos-init (default type)
  (cond ((or (null default)
             (and (stringp default) (string-empty-p default)))
         nil)
        ((member type '(:int32 :uint32 :int64 :uint64 :sint32 :sint64
                        :fixed32 :sfixed32 :fixed64 :sfixed64
                        :single :double))
         (read-from-string default))
        ((eq type :bool)
         (if (string= default "true") t nil))
        (t default)))
