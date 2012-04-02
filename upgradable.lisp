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


;;; Can a version of a Protobufs schema be upgraded to a new version

(defgeneric protobuf-upgradable (old new &optional what)
  (:documentation
   "Returns true if and only if the old protobuf schema (enum, message, etc)
    can be upgraded to the new schema.
    If the schema is not upgradable, the second value is a list of warnings"))

(defvar *upgrade-warnings*)
(defmacro upgrade-warn ((predicate old new) format-string &optional name)
  "Collect an upgrade warning into *upgrade-warnings*."
  (with-gensyms (vold vnew)
    `(let* ((,vold ,old)
            (,vnew ,new))
       (cond ((,predicate ,vold ,vnew)
              t)
             (t
              ;; Note that this returns the non-NIL value of *upgrade-warnings*,
              ;; so the upgradable check will continue to collect warnings
              (push (format nil ,format-string
                            ,@(if name (list name vold vnew) (list vold vnew)))
                    *upgrade-warnings*))))))

(defmethod protobuf-upgradable ((old protobuf) (new protobuf) &optional what)
  (declare (ignore what))
  (let ((*upgrade-warnings* ()))
    (and
     ;; Are they named the same?
     (upgrade-warn (string= (proto-name old) (proto-name new))
                   "Protobuf schema name changed from ~A to ~A")
     (upgrade-warn (string= (proto-package old) (proto-package new))
                   "Protobuf schema package changed from ~A to ~A")
     ;; Is every enum in 'old' upgradable to an enum in 'new'?
     (loop for old-enum in (proto-enums old)
           as new-enum = (find (proto-name old-enum) (proto-enums new)
                               :key #'proto-name :test #'string=)
           always (and new-enum (protobuf-upgradable old-enum new-enum)))
     ;; Is every message in 'old' upgradable to a message in 'new'?
     (loop for old-msg in (proto-messages old)
           as new-msg = (find (proto-name old-msg) (proto-messages new)
                              :key #'proto-name :test #'string=)
           always (and new-msg (protobuf-upgradable old-msg new-msg)))
     ;; Is every service in 'old' upgradable to a service in 'new'?
     (loop for old-svc in (proto-services old)
           as new-svc = (find (proto-name old-svc) (proto-services new)
                              :key #'proto-name :test #'string=)
           always (and new-svc (protobuf-upgradable old-svc new-svc))))
    (values (null *upgrade-warnings*) (nreverse *upgrade-warnings*))))


(defmethod protobuf-upgradable ((old protobuf-enum) (new protobuf-enum) &optional what)
  (declare (ignore what))
  ;; No need to check that the names are equal, our caller did that already
  (loop for old-val in (proto-values old)
        as new-val = (find (proto-name old-val) (proto-values new)
                           :key #'proto-name :test #'string=)
        always (and new-val (protobuf-upgradable old-val new-val old))))

(defmethod protobuf-upgradable ((old protobuf-enum-value) (new protobuf-enum-value) &optional enum)
  ;; No need to check that the names are equal, our caller did that already
  ;; Do they have the same index?
  (upgrade-warn (= (proto-index old) (proto-index new))
                "Enum index for ~A changed from ~D to ~D"
                (format nil "~A within ~A" (proto-name old) (proto-name enum))))


(defmethod protobuf-upgradable ((old protobuf-message) (new protobuf-message) &optional what)
  (declare (ignore what))
  ;; No need to check that the names are equal, our caller did that already
  (and
   ;; Is every enum in 'old' upgradable to an enum in 'new'?
   (loop for old-enum in (proto-enums old)
         as new-enum = (find (proto-name old-enum) (proto-enums new)
                             :key #'proto-name :test #'string=)
         always (and new-enum (protobuf-upgradable old-enum new-enum)))
   ;; Is every message in 'old' upgradable to a message in 'new'?
   (loop for old-msg in (proto-messages old)
         as new-msg = (find (proto-name old-msg) (proto-messages new)
                            :key #'proto-name :test #'string=)
         always (and new-msg (protobuf-upgradable old-msg new-msg)))
   ;; Is every required field in 'old' upgradable to a field in 'new'?
   ;; (Optional fields are safe to remove)
   (loop for old-fld in (proto-fields old)
         as new-fld = (find (proto-name old-fld) (proto-fields new)
                            :key #'proto-name :test #'string=)
         always (if new-fld
                  (protobuf-upgradable old-fld new-fld old)
                  ;; If there's no new field, the old one must not be required
                  (or (member (proto-required old-fld) '(:optional :repeated))
                      (push (format nil "Old field ~A was required, and is now missing"
                                    (proto-name old-fld))
                            *upgrade-warnings*))))))

(defmethod protobuf-upgradable ((old protobuf-field) (new protobuf-field) &optional message)
  (flet ((arity-upgradable (old new)
           ;;--- Handle conversions between non-required fields and extensions
           (or (eq old new)
               (not (eq new :required))))
         (type-upgradable (old new)
           ;;--- Handle conversions between embedded messages and bytes
           (or
            (string= old new)
            ;; These varint types are all compatible
            (and (member old '("int32" "uint32" "int64" "uint64" "bool") :test #'string=)
                 (member new '("int32" "uint32" "int64" "uint64" "bool") :test #'string=))
            ;; The two signed integer types are compatible
            (and (member old '("sint32" "sint64") :test #'string=)
                 (member new '("sint32" "sint64") :test #'string=))
            ;; Fixed integers are compatible with each other
            (and (member old '("fixed32" "sfixed32") :test #'string=)
                 (member new '("fixed32" "sfixed32") :test #'string=))
            (and (member old '("fixed64" "sfixed64") :test #'string=)
                 (member new '("fixed64" "sfixed64") :test #'string=))
            ;; Strings and bytes are compatible, assuming UTF-8 encoding
            (and (member old '("string" "bytes") :test #'string=)
                 (member new '("string" "bytes") :test #'string=))))
         (default-upgradable (old new)
           (declare (ignore old new))
           t))
    ;; No need to check that the names are equal, our caller did that already
    (and
     ;; Do they have the same index?
     (upgrade-warn (= (proto-index old) (proto-index new))
                   "Field index for ~A changed from ~D to ~D"
                   (format nil "~A within ~A" (proto-name old) (proto-name message)))
     ;; Are the arity and type upgradable?
     (upgrade-warn (arity-upgradable (proto-required old) (proto-required new))
                   "Arity of ~A, ~S, is not upgradable to ~S"
                   (format nil "~A within ~A" (proto-name old) (proto-name message)))
     (upgrade-warn (type-upgradable (proto-type old) (proto-type new))
                   "Type of ~A, ~A, is not upgradable to ~A"
                   (format nil "~A within ~A" (proto-name old) (proto-name message))))))


(defmethod protobuf-upgradable ((old protobuf-service) (new protobuf-service) &optional what)
  (declare (ignore what))
  ;; No need to check that the names are equal, our caller did that already
  ;; Is every RPC in 'old' upgradable to an RPC in 'new'?
  (loop for old-rpc in (proto-rpcs old)
        as new-rpc = (find (proto-name old-rpc) (proto-rpcs new)
                           :key #'proto-name :test #'string=)
        always (and new-rpc (protobuf-upgradable old-rpc new-rpc old))))

(defmethod protobuf-upgradable ((old protobuf-rpc) (new protobuf-rpc) &optional service)
  (declare (ignore service))
  ;; No need to check that the names are equal, our caller did that already
  (and
   ;; Are their inputs and outputs the same?
   (upgrade-warn (string= (proto-input-type old) (proto-input-type new))
                 "Input type for ~A, ~A, is not upgradable to ~A" (proto-name old))
   (upgrade-warn (string= (proto-output-type old) (proto-output-type new))
                 "Output type for ~A, ~A, is not upgradable to ~A" (proto-name old))))
