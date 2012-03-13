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


;;; Can a version of a protobuf be upgraded to a new version

;;--- This should return (a set of) reason(s) if the upgrade will fail
(defgeneric protobuf-upgradable (new old)
  (:documentation
   "Returns true if and only if the old protobuf schema (enum, message, etc)
    can be upgraded to the new schema."))

(defmethod protobuf-upgradable ((old protobuf) (new protobuf))
  (and
   ;; Are they named the same?
   (string= (proto-name old) (proto-name new))
   (string= (proto-package old) (proto-package new))
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
         always (and new-svc (protobuf-upgradable old-svc new-svc)))))


(defmethod protobuf-upgradable ((old protobuf-enum) (new protobuf-enum))
  (and
   ;; Are they named the same?
   (string= (proto-name old) (proto-name new))
   ;; Is every value in 'old' upgradable to a value in 'new'?
   (loop for old-val in (proto-values old)
         as new-val = (find (proto-name old-val) (proto-values new)
                            :key #'proto-name :test #'string=)
         always (and new-val (protobuf-upgradable old-val new-val)))))

(defmethod protobuf-upgradable ((old protobuf-enum-value) (new protobuf-enum-value))
  (and
   ;; Are they named the same?
   (string= (proto-name old) (proto-name new))
   ;; Do they have the same index?
   (= (proto-index old) (proto-index new))))


(defmethod protobuf-upgradable ((old protobuf-message) (new protobuf-message))
  (and
   ;; Are they named the same?
   (string= (proto-name old) (proto-name new))
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
                  (protobuf-upgradable old-fld new-fld)
                  (not (eq (proto-required old) :required))))))

(defmethod protobuf-upgradable ((old protobuf-field) (new protobuf-field))
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
    (and
     ;; Are they named the same?
     (string= (proto-name old) (proto-name new))
     ;; Do they have the same index?
     (= (proto-index old) (proto-index new))
     ;; Is the type and arity upgradable
     (arity-upgradable (proto-required old) (proto-required new))
     (type-upgradable (proto-type old) (proto-type new))
     (arity-upgradable (proto-required old) (proto-required new)))))


(defmethod protobuf-upgradable ((old protobuf-service) (new protobuf-service))
  (and
   ;; Are they named the same?
   (string= (proto-name old) (proto-name new))
   ;; Is every RPC in 'old' upgradable to an RPC in 'new'?
   (loop for old-rpc in (proto-rpcs old)
         as new-rpc = (find (proto-name old-rpc) (proto-rpcs new)
                            :key #'proto-name :test #'string=)
         always (and new-rpc (protobuf-upgradable old-rpc new-rpc)))))

(defmethod protobuf-upgradable ((old protobuf-rpc) (new protobuf-rpc))
    (and
     ;; Are they named the same?
     (string= (proto-name old) (proto-name new))
     ;; Are their inputs and outputs the same
     (string= (proto-input-type old) (proto-input-type new))
     (string= (proto-output-type old) (proto-output-type new))))
