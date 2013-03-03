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


;;; Can a version of a Protobufs schema be upgraded to a new version

(defvar *upgrade-warnings* nil
  "Bound to the list of upgrade warning messages.")

(defun upgrade-warn (format-string &rest format-args)
  "Collect an upgrade warning into *upgrade-warnings*.
   Returns the list of warnings."
  (push (apply #'format nil format-string format-args) *upgrade-warnings*))

(defmacro upgrade-assert ((predicate old new) format-string &optional name)
  "Assert that the condition is true, otherwise issue an upgrade warning."
  (with-gensyms (vold vnew)
    `(let* ((,vold ,old)
            (,vnew ,new))
       (cond ((,predicate ,vold ,vnew)
              t)
             (t
              ;; Note that this returns the non-NIL value of *upgrade-warnings*,
              ;; so the upgradable check will continue to collect warnings
              (upgrade-warn ,format-string ,@(if name (list name vold vnew) (list vold vnew))))))))


(defgeneric schema-upgradable (old new &optional old-parent new-parent)
  (:documentation
   "Returns true if and only if the old Protobufs schema can be upgraded to
    the new schema.
    'old' is the old object (schema, enum, message, etc), 'new' is the new one.
    'old-parent' is the \"parent\" of 'old', 'new-parent' is the parent of 'new'.
    If the schema is not upgradable, the second value is a list of warnings."))

(defmethod schema-upgradable ((old protobuf-schema) (new protobuf-schema)
                              &optional old-parent new-parent)
  (declare (ignore old-parent new-parent))
  (let ((*upgrade-warnings* ()))
    (and
     ;; Are they named the same?
     (upgrade-assert (string= (proto-name old) (proto-name new))
                     "Protobuf schema name changed from '~A' to '~A'")
     (upgrade-assert (string= (proto-package old) (proto-package new))
                     "Protobuf schema package changed from '~A' to '~A'")
     ;; Is every enum in 'old' upgradable to an enum in 'new'?
     (loop for old-enum in (proto-enums old)
           as new-enum = (find (proto-name old-enum) (proto-enums new)
                               :key #'proto-name :test #'string=)
           always (and new-enum (schema-upgradable old-enum new-enum old new)))
     ;; Is every message in 'old' upgradable to a message in 'new'?
     (loop for old-msg in (proto-messages old)
           as new-msg = (find (proto-name old-msg) (proto-messages new)
                              :key #'proto-name :test #'string=)
           always (and new-msg (schema-upgradable old-msg new-msg old new)))
     ;; Is every service in 'old' upgradable to a service in 'new'?
     (loop for old-svc in (proto-services old)
           as new-svc = (find (proto-name old-svc) (proto-services new)
                              :key #'proto-name :test #'string=)
           always (and new-svc (schema-upgradable old-svc new-svc old new))))
    (values (null *upgrade-warnings*) (nreverse *upgrade-warnings*))))


(defmethod schema-upgradable ((old protobuf-enum) (new protobuf-enum)
                              &optional old-parent new-parent)
  (declare (ignore old-parent new-parent))
  ;; No need to check that the names are equal, our caller did that already
  (loop for old-val in (proto-values old)
        as new-val = (find (proto-name old-val) (proto-values new)
                           :key #'proto-name :test #'string=)
        always (and new-val (schema-upgradable old-val new-val old new))))

(defmethod schema-upgradable ((old protobuf-enum-value) (new protobuf-enum-value)
                              &optional old-enum new-enum)
  (declare (ignore new-enum))
  ;; No need to check that the names are equal, our caller did that already
  ;; Do they have the same index?
  (upgrade-assert (= (proto-index old) (proto-index new))
                  "Enum index for '~A' changed from ~D to ~D"
                  (format nil "~A.~A" (proto-name old-enum) (proto-name old))))


(defmethod schema-upgradable ((old protobuf-message) (new protobuf-message)
                              &optional old-parent new-parent)
  (declare (ignore old-parent new-parent))
  ;; No need to check that the names are equal, our caller did that already
  (and
   ;; Is every enum in 'old' upgradable to an enum in 'new'?
   (loop for old-enum in (proto-enums old)
         as new-enum = (find (proto-name old-enum) (proto-enums new)
                             :key #'proto-name :test #'string=)
         always (and new-enum (schema-upgradable old-enum new-enum old new)))
   ;; Is every message in 'old' upgradable to a message in 'new'?
   (loop for old-msg in (proto-messages old)
         as new-msg = (find (proto-name old-msg) (proto-messages new)
                            :key #'proto-name :test #'string=)
         always (and new-msg (schema-upgradable old-msg new-msg old new)))
   ;; Is every required field in 'old' upgradable to a field in 'new'?
   ;; (Optional fields are safe to remove)
   (loop for old-fld in (proto-fields old)
         as new-fld = (find (proto-name old-fld) (proto-fields new)
                            :key #'proto-name :test #'string=)
         always (if new-fld
                  (schema-upgradable old-fld new-fld old new)
                  ;; If there's no new field, the old one must not be required
                  (or (member (proto-required old-fld) '(:optional :repeated))
                      (upgrade-warn "Old field '~A.~A' was required, and is now missing"
                                    (proto-name old) (proto-name old-fld)))))))

(defmethod schema-upgradable ((old protobuf-field) (new protobuf-field)
                              &optional old-message new-message)
  (flet ((arity-upgradable (old-arity new-arity)
           (or (eq old-arity new-arity)
               ;; Don't add new required fields
               (not (eq new-arity :required))
               ;; Optional fields and extensions are compatible
               (and (eq old-arity :optional)
                    (index-within-extensions-p (proto-index new) new-message))
               (and (eq new-arity :optional)
                    (index-within-extensions-p (proto-index old) old-message))))
         (type-upgradable (old-type new-type)
           ;;--- Handle conversions between embedded messages and bytes
           (or
            (string= old-type new-type)
            ;; These varint types are all compatible
            (and (member old-type '("int32" "uint32" "int64" "uint64" "bool") :test #'string=)
                 (member new-type '("int32" "uint32" "int64" "uint64" "bool") :test #'string=))
            ;; The two signed integer types are compatible
            (and (member old-type '("sint32" "sint64") :test #'string=)
                 (member new-type '("sint32" "sint64") :test #'string=))
            ;; Fixed integers are compatible with each other
            (and (member old-type '("fixed32" "sfixed32") :test #'string=)
                 (member new-type '("fixed32" "sfixed32") :test #'string=))
            (and (member old-type '("fixed64" "sfixed64") :test #'string=)
                 (member new-type '("fixed64" "sfixed64") :test #'string=))
            ;; Strings and bytes are compatible, assuming UTF-8 encoding
            (and (member old-type '("string" "bytes") :test #'string=)
                 (member new-type '("string" "bytes") :test #'string=)))))
    ;; No need to check that the names are equal, our caller did that already
    (and
     ;; Do they have the same index?
     (upgrade-assert (= (proto-index old) (proto-index new))
                     "Field index for '~A' changed from ~D to ~D"
                     (format nil "~A.~A" (proto-name old-message) (proto-name old)))
     ;; Are the arity and type upgradable?
     (upgrade-assert (arity-upgradable (proto-required old) (proto-required new))
                     "Arity of ~A, ~S, is not upgradable to ~S"
                     (format nil  "~A.~A" (proto-name old-message) (proto-name old)))
     (upgrade-assert (type-upgradable (proto-type old) (proto-type new))
                     "Type of '~A', ~A, is not upgradable to ~A"
                     (format nil  "~A.~A" (proto-name old-message) (proto-name old)))
     ;; Is the default the same?
     (upgrade-assert (equal (proto-default old) (proto-default new))
                     "Old default for ~A, ~S, is not equal to new default ~S"
                     (format nil  "~A.~A" (proto-name old-message) (proto-name old))))))


(defmethod schema-upgradable ((old protobuf-service) (new protobuf-service)
                              &optional old-parent new-parent)
  (declare (ignore old-parent new-parent))
  ;; No need to check that the names are equal, our caller did that already
  ;; Is every method in 'old' upgradable to a method in 'new'?
  (loop for old-method in (proto-methods old)
        as new-method = (find (proto-name old-method) (proto-methods new)
                              :key #'proto-name :test #'string=)
        always (and new-method (schema-upgradable old-method new-method old new))))

(defmethod schema-upgradable ((old protobuf-method) (new protobuf-method)
                              &optional old-service new-service)
  (declare (ignore new-service))
  ;; No need to check that the names are equal, our caller did that already
  (and
   ;; Are their inputs and outputs the same?
   (upgrade-assert (string= (proto-input-name old) (proto-input-name new))
                   "Input type for ~A, ~A, is not upgradable to ~A"
                   (format nil  "~A.~A" (proto-name old-service) (proto-name old)))
   (upgrade-assert (string= (proto-output-name old) (proto-output-name new))
                   "Output type for ~A, ~A, is not upgradable to ~A"
                   (format nil  "~A.~A" (proto-name old-service) (proto-name old)))))


;;; Are two protobuf schemas equal?

;; This is useful for testing purposes, but not much else
(defgeneric schemas-equal (schema1 proto2)
  (:documentation
   "Returns true if and only if the two Protobufs schemas are equal."))

;; These methods are pretty similar to the 'schema-upgradable' methods above
(defmethod schemas-equal ((schema1 protobuf-schema) (schema2 protobuf-schema))
  (and
   ;; If the name(s) are null, don't worry about them
   (or (null (proto-class schema1)) (null (proto-class schema2))
       (eql (proto-class schema1) (proto-class schema2)))
   (or (null (proto-name schema1)) (null (proto-name schema2))
       (equal (proto-name schema1) (proto-name schema2)))
   (equal (proto-syntax schema1) (proto-syntax schema2))
   (equal (proto-package schema1) (proto-package schema2))
   (equal (proto-lisp-package schema1) (proto-lisp-package schema2))
   (equal (proto-imports schema1) (proto-imports schema2))
   (= (length (proto-options schema1)) (length (proto-options schema2)))
   (loop for option1 in (proto-options schema1)
         as option2 = (find (proto-name option1) (proto-options schema2)
                            :key #'proto-name :test #'string=)
         always (and option2 (schemas-equal option1 option2)))
   (= (length (proto-enums schema1)) (length (proto-enums schema2)))
   (loop for enum1 in (proto-enums schema1)
         as enum2 = (find (proto-name enum1) (proto-enums schema2)
                          :key #'proto-name :test #'string=)
         always (and enum2 (schemas-equal enum1 enum2)))
   (= (length (proto-messages schema1)) (length (proto-messages schema2)))
   (loop for msg1 in (proto-messages schema1)
         as msg2 = (find-if #'(lambda (msg2)
                                (and (string= (proto-name msg1) (proto-name msg2))
                                     (eql (proto-message-type msg1) (proto-message-type msg2))))
                            (proto-messages schema2))
         always (and msg2 (schemas-equal msg1 msg2)))
   (= (length (proto-services schema1)) (length (proto-services schema2)))
   (loop for svc1 in (proto-services schema1)
         as svc2 = (find (proto-name svc1) (proto-services schema2)
                         :key #'proto-name :test #'string=)
         always (and svc2 (schemas-equal svc1 svc2)))))

(defmethod schemas-equal ((option1 protobuf-option) (option2 protobuf-option))
  (and
   (string= (proto-name option1) (proto-name option2))
   (equal   (proto-value option1) (proto-value option2))
   (equal   (proto-type option1) (proto-type option2))))

(defmethod schemas-equal ((enum1 protobuf-enum) (enum2 protobuf-enum))
  (and
   (eql   (proto-class enum1) (proto-class enum2))
   (equal (proto-name enum1) (proto-name enum2))
   (equal (proto-alias-for enum1) (proto-alias-for enum2))
   (= (length (proto-options enum1)) (length (proto-options enum2)))
   (loop for option1 in (proto-options enum1)
         as option2 = (find (proto-name option1) (proto-options enum2)
                            :key #'proto-name :test #'string=)
         always (and option2 (schemas-equal option1 option2)))
   (= (length (proto-values enum1)) (length (proto-values enum2)))
   (loop for val1 in (proto-values enum1)
         as val2 = (find (proto-name val1) (proto-values enum2)
                         :key #'proto-name :test #'string=)
         always (and val2 (schemas-equal val1 val2)))))

(defmethod schemas-equal ((value1 protobuf-enum-value) (value2 protobuf-enum-value))
  (and 
   (eql   (proto-class value1) (proto-class value2))
   (equal (proto-name value1) (proto-name value2))
   (eql   (proto-index value1) (proto-index value2))
   (equal (proto-value value1) (proto-value value2))))

(defmethod schemas-equal ((message1 protobuf-message) (message2 protobuf-message))
  (and
   (eql   (proto-class message1) (proto-class message2))
   (equal (proto-name message1) (proto-name message2))
   (equal (proto-alias-for message1) (proto-alias-for message2))
   (eql   (proto-message-type message1) (proto-message-type message2))
   (= (length (proto-options message1)) (length (proto-options message2)))
   (loop for option1 in (proto-options message1)
         as option2 = (find (proto-name option1) (proto-options message2)
                            :key #'proto-name :test #'string=)
         always (and option2 (schemas-equal option1 option2)))
   (= (length (proto-enums message1)) (length (proto-enums message2)))
   (loop for enum1 in (proto-enums message1)
         as enum2 = (find (proto-name enum1) (proto-enums message2)
                          :key #'proto-name :test #'string=)
         always (and enum2 (schemas-equal enum1 enum2)))
   (= (length (proto-messages message1)) (length (proto-messages message2)))
   (loop for msg1 in (proto-messages message1)
         as msg2 = (find-if #'(lambda (msg2)
                                (and (string= (proto-name msg1) (proto-name msg2))
                                     (eql (proto-message-type msg1) (proto-message-type msg2))))
                            (proto-messages message2))
         always (and msg2 (schemas-equal msg1 msg2)))
   (= (length (proto-fields message1)) (length (proto-fields message2)))
   (loop for fld1 in (proto-fields message1)
         as fld2 = (find (proto-name fld1) (proto-fields message2)
                         :key #'proto-name :test #'string=)
         always (and fld2 (schemas-equal fld1 fld2)))
   (= (length (proto-extensions message1)) (length (proto-extensions message2)))
   (loop for ext1 in (proto-extensions message1)
         for ext2 in (proto-extensions message2)
         always (schemas-equal ext1 ext2))))

(defmethod schemas-equal ((field1 protobuf-field) (field2 protobuf-field))
  (and
   (eql   (proto-class field1) (proto-class field2))
   (equal (proto-name field1) (proto-name field2))
   (equal (proto-type field1) (proto-type field2))
   (eql   (proto-required field1) (proto-required field2))
   (eql   (proto-value field1) (proto-value field2))
   (eql   (proto-index field1) (proto-index field2))
   (eql   (proto-reader field1) (proto-reader field2))
   (eql   (proto-writer field1) (proto-writer field2))
   (equal (proto-default field1) (proto-default field2))
   (eql   (proto-packed field1) (proto-packed field2))
   (eql   (proto-message-type field1) (proto-message-type field2))
   (= (length (proto-options field1)) (length (proto-options field2)))
   (loop for option1 in (proto-options field1)
         as option2 = (find (proto-name option1) (proto-options field2)
                            :key #'proto-name :test #'string=)
         always (and option2 (schemas-equal option1 option2)))))

(defmethod schemas-equal ((extension1 protobuf-extension) (extension2 protobuf-extension))
  (and
   (eql (proto-extension-from extension1) (proto-extension-from extension2))
   (eql (proto-extension-to extension1) (proto-extension-to extension2))))

(defmethod schemas-equal ((service1 protobuf-service) (service2 protobuf-service))
  (and
   (eql   (proto-class service1) (proto-class service2))
   (equal (proto-name service1) (proto-name service2))
   (= (length (proto-options service1)) (length (proto-options service2)))
   (loop for option1 in (proto-options service1)
         as option2 = (find (proto-name option1) (proto-options service2)
                            :key #'proto-name :test #'string=)
         always (and option2 (schemas-equal option1 option2)))
   (= (length (proto-methods service1)) (length (proto-methods service2)))
   (loop for method1 in (proto-methods service1)
         as method2 = (find (proto-name method1) (proto-methods service2)
                            :key #'proto-name :test #'string=)
         always (and method2 (schemas-equal method1 method2)))))

(defmethod schemas-equal ((method1 protobuf-method) (method2 protobuf-method))
  (and
   (eql   (proto-class method1) (proto-class method2))
   (equal (proto-name method1) (proto-name method2))
   (eql   (proto-input-type method1) (proto-input-type method2))
   (eql   (proto-output-type method1) (proto-output-type method2))
   (equal (proto-input-name method1) (proto-input-name method2))
   (equal (proto-output-name method1) (proto-output-name method2))
   (eql   (proto-index method1) (proto-index method2))
   (= (length (proto-options method1)) (length (proto-options method2)))
   (loop for option1 in (proto-options method1)
         as option2 = (find (proto-name option1) (proto-options method2)
                            :key #'proto-name :test #'string=)
         always (and option2 (schemas-equal option1 option2)))))
