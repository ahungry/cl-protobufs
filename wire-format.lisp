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


;;; Protocol buffers wire format

;;; Serializers

;; Serialize 'val' of primitive type 'type' into the buffer
(defun serialize-prim (val type field buffer index)
  "Serializes a Protobufs primitive (scalar) value into the buffer at the given index.
   The value is given by 'val', the primitive type by 'type'.
   'field' is the protobuf-field describing the value.
   Modifies the buffer in place, and returns the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (ecase type
      ((:int32 :uint32)
       (let* ((tag (ilogior $wire-type-varint (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-uint32 val buffer idx)))
      ((:int64 :uint64)
       (let* ((tag (ilogior $wire-type-varint (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-uint64 val buffer idx)))
      ((:sint32)
       (let* ((tag (ilogior $wire-type-varint (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-uint32 (zig-zag-encode32 val) buffer idx)))
      ((:sint64)
       (let* ((tag (ilogior $wire-type-varint (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-uint64 (zig-zag-encode64 val) buffer idx)))
      ((:fixed32)
       (let* ((tag (ilogior $wire-type-32bit (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-fixed32 val buffer idx)))
      ((:sfixed32)
       (let* ((tag (ilogior $wire-type-32bit (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-sfixed32 val buffer idx)))
      ((:fixed64)
       (let* ((tag (ilogior $wire-type-64bit (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-fixed64 val buffer idx)))
      ((:sfixed64)
       (let* ((tag (ilogior $wire-type-64bit (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-sfixed64 val buffer idx)))
      ((:string)
       (let* ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-octets (babel:string-to-octets val :encoding :utf-8) buffer idx)))
      ((:bytes)
       (let* ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-octets val buffer idx)))
      ((:bool)
       (let* ((tag (ilogior $wire-type-varint (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-uint32 (if val 1 0) buffer idx)))
      ((:float)
       (let* ((tag (ilogior $wire-type-32bit (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-single val buffer idx)))
      ((:double)
       (let* ((tag (ilogior $wire-type-64bit (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-double val buffer idx)))
      ;; A few of our homegrown types
      ((:symbol)
       (let* ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index))
              (val (format nil "~A:~A" (package-name (symbol-package val)) (symbol-name val))))
         ;; Call 'string' in case we are trying to serialize a symbol name
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-octets (babel:string-to-octets val :encoding :utf-8) buffer idx)))
      ((:date :time :datetime :timestamp)
       (let* ((tag (ilogior $wire-type-64bit (iash (proto-index field) 3)))
              (idx (encode-uint32 tag buffer index)))
         (declare (type (unsigned-byte 32) tag)
                  (type fixnum idx))
         (encode-uint64 val buffer idx))))))

(defun serialize-packed (values type field buffer index)
  "Serializes a set of packed values into the buffer at the given index.
   The values are given by 'values', the primitive type by 'type'.
   'field' is the protobuf-field describing the value.
   Modifies the buffer in place, and returns the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let* ((wtype (ecase type
                    ((:int32 :int64)   $wire-type-varint)
                    ((:uint32 :uint64) $wire-type-varint)
                    ((:sint32 :sint64) $wire-type-varint)
                    ((:fixed32 :sfixed32) $wire-type-32bit)
                    ((:fixed64 :sfixed64) $wire-type-64bit)
                    ((:float)  $wire-type-32bit)
                    ((:double) $wire-type-64bit)))
           (tag (ilogior wtype (iash (proto-index field) 3)))
           (idx (encode-uint32 tag buffer index)))
      (declare (type (unsigned-byte 32) wtype tag)
               (type fixnum idx))
      (multiple-value-bind (full-len len)
          (packed-size values type field)
        (declare (type fixnum len) (ignore full-len))
        (setq idx (encode-uint32 len buffer idx)))
      (ecase type
        ((:int32 :uint32)
         (dolist (val values idx)
           (setq idx (encode-uint32 val buffer idx))))
        ((:int64 :uint64)
         (dolist (val values idx)
           (setq idx (encode-uint64 val buffer idx))))
        ((:sint32)
         (dolist (val values idx)
           (setq idx (encode-uint32 (zig-zag-encode32 val) buffer idx))))
        ((:sint64)
         (dolist (val values idx)
           (setq idx (encode-uint64 (zig-zag-encode64 val) buffer idx))))
        ((:fixed32)
         (dolist (val values idx)
           (setq idx (encode-fixed32 val buffer idx))))
        ((:sfixed32)
         (dolist (val values idx)
           (setq idx (encode-sfixed32 val buffer idx))))
        ((:fixed64)
         (dolist (val values idx)
           (setq idx (encode-fixed64 val buffer idx))))
        ((:sfixed64)
         (dolist (val values idx)
           (setq idx (encode-sfixed64 val buffer idx))))
        ((:float)
         (dolist (val values idx)
           (setq idx (encode-single val buffer idx))))
        ((:double)
         (dolist (val values idx)
           (setq idx (encode-double val buffer idx))))))))

;; Serialize 'val' of enum type 'type' into the buffer
(defun serialize-enum (val enum field buffer index)
  "Serializes a Protobufs enum value into the buffer at the given index.
   The value is given by 'val', the enum type by 'enum'.
   'field' is the protobuf-field describing the value.
   Modifies the buffer in place, and returns the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let* ((val (let ((e (find val (proto-values enum) :key #'proto-value)))
                  (and e (proto-index e))))
           (tag (ilogior $wire-type-varint (iash (proto-index field) 3)))
           (idx (encode-uint32 tag buffer index)))
      (declare (type (unsigned-byte 32) val tag)
               (type fixnum idx))
      (encode-uint32 val buffer idx))))


;;; Deserializers

;; Deserialize the next object of type 'type', described by the protobuf field 'field'
(defun deserialize-prim (type field buffer index)
  "Deserializes the next object of primitive type 'type', described by the protobuf-field 'field'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (declare (ignore field))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (ecase type
      ((:int32 :uint32)
       (decode-uint32 buffer index))
      ((:int64 :uint64)
       (decode-uint64 buffer index))
      ((:sint32)
       (multiple-value-bind (val idx)
           (decode-uint32 buffer index)
         (values (zig-zag-decode32 val) idx)))
      ((:sint64)
       (multiple-value-bind (val idx)
           (decode-uint64 buffer index)
         (values (zig-zag-decode64 val) idx)))
      ((:fixed32)
       (decode-fixed32 buffer index))
      ((:sfixed32)
       (decode-sfixed32 buffer index))
      ((:fixed64)
       (decode-fixed64 buffer index))
      ((:sfixed64)
       (decode-sfixed64 buffer index))
      ((:string)
       (multiple-value-bind (val idx)
           (decode-octets buffer index)
         (values (babel:octets-to-string val :encoding :utf-8) idx)))
      ((:bytes)
       (decode-octets buffer index))
      ((:bool)
       (multiple-value-bind (val idx)
           (decode-uint32 buffer index)
         (values (if (zerop val) nil t) idx)))
      ((:float)
       (decode-single buffer index))
      ((:double)
       (decode-double buffer index))
      ;; A few of our homegrown types
      ((:symbol)
       (multiple-value-bind (val idx)
           (decode-octets buffer index)
         (let* ((val   (babel:octets-to-string val :encoding :utf-8))
                (colon (position #\: val))
                (pkg   (subseq val 0 colon))
                (sym   (subseq val (i+ colon 1))))
           (values (intern sym pkg) idx))))
      ((:date :time :datetime :timestamp)
       (decode-uint64 buffer index)))))

(defun deserialize-packed (type field buffer index)
  "Deserializes the next packed values of type 'type', described by the protobuf-field 'field'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (declare (ignore field))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (multiple-value-bind (len idx)
        (decode-uint32 buffer index)
      (declare (type (unsigned-byte 32) len)
               (type fixnum idx))
      (let ((end (i+ idx len)))
        (declare (type (unsigned-byte 32) end))
        (with-collectors ((values collect-value))
          (loop
            (when (>= idx end)
              (return-from deserialize-packed (values values idx)))
            (multiple-value-bind (val nidx)
                (ecase type
                  ((:int32 :uint32)
                   (decode-uint32 buffer idx))
                  ((:int64 :uint64)
                   (decode-uint64 buffer idx))
                  ((:sint32)
                   (multiple-value-bind (val idx)
                       (decode-uint32 buffer idx)
                     (values (zig-zag-decode32 val) idx)))
                  ((:sint64)
                   (multiple-value-bind (val idx)
                       (decode-uint64 buffer idx)
                     (values (zig-zag-decode64 val) idx)))
                  ((:fixed32)
                   (decode-fixed32 buffer idx))
                  ((:sfixed32)
                   (decode-sfixed32 buffer idx))
                  ((:fixed64)
                   (decode-fixed64 buffer idx))
                  ((:sfixed64)
                   (decode-sfixed64 buffer idx))
                  ((:float)
                   (decode-single buffer idx))
                  ((:double)
                   (decode-double buffer idx)))
              (collect-value val)
              (setq idx nidx))))))))

(defun deserialize-enum (enum field buffer index)
  "Deserializes the next enum of type 'type', described by the protobuf-field 'field'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (declare (ignore field))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (multiple-value-bind (val idx)
        (decode-uint32 buffer index)
      (let ((val (let ((e (find val (proto-values enum) :key #'proto-index)))
                   (and e (proto-value e)))))
        (values val idx)))))


;;; Object sizing

(defun prim-size (val type field)
  "Returns the size in bytes that the primitive object will take when serialized."
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (ecase type
      ((:int32 :uint32)
       (let ((tag (ilogior $wire-type-varint (iash (proto-index field) 3))))
         (i+ (length32 tag) (length32 val))))
      ((:int64 :uint64)
       (let ((tag (ilogior $wire-type-varint (iash (proto-index field) 3))))
         (i+ (length32 tag) (length64 val))))
      ((:sint32)
       (let ((tag (ilogior $wire-type-varint (iash (proto-index field) 3))))
         (i+ (length32 tag) (length32 (zig-zag-encode32 val)))))
      ((:sint64)
       (let ((tag (ilogior $wire-type-varint (iash (proto-index field) 3))))
         (i+ (length32 tag) (length64 (zig-zag-encode64 val)))))
      ((:fixed32 :sfixed32)
       (let ((tag (ilogior $wire-type-32bit (iash (proto-index field) 3))))
         (i+ (length32 tag) 4)))
      ((:fixed64 :sfixed64)
       (let ((tag (ilogior $wire-type-64bit (iash (proto-index field) 3))))
         (i+ (length32 tag) 8)))
      ((:string)
       (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
             (len (babel:string-size-in-octets val :encoding :utf-8)))
         (i+ (length32 tag) (length32 len) len)))
      ((:bytes)
       (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
               (len (length val)))
           (i+ (length32 tag) (length32 len) len))))
      ((:bool)
       (let ((tag (ilogior $wire-type-varint (iash (proto-index field) 3))))
         (i+ (length32 tag) 1)))
      ((:float)
       (let ((tag (ilogior $wire-type-32bit (iash (proto-index field) 3))))
         (i+ (length32 tag) 4)))
      ((:double)
       (let ((tag (ilogior $wire-type-64bit (iash (proto-index field) 3))))
         (i+ (length32 tag) 8)))
      ;; A few of our homegrown types
      ((:symbol)
       (let* ((tag (ilogior $wire-type-string (iash (proto-index field) 3)))
              (len (i+ (length (package-name (symbol-package val))) 1 (length (symbol-name val)))))
         (i+ (length32 tag) (length32 len) len)))
      ((:date :time :datetime :timestamp)
       (let ((tag (ilogior $wire-type-64bit (iash (proto-index field) 3))))
         (i+ (length32 tag) 8))))))

(defun packed-size (values type field)
  "Returns the size in bytes that the packed object will take when serialized."
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((tag (ilogior $wire-type-varint (iash (proto-index field) 3)))
          (len (loop for val in values
                     summing (ecase type
                               ((:int32 :uint32) (length32 val))
                               ((:int64 :uint64) (length64 val))
                               ((:sint32) (length32 (zig-zag-encode32 val)))
                               ((:sint64) (length64 (zig-zag-encode64 val)))
                               ((:fixed32 :sfixed32) 4)
                               ((:fixed64 :sfixed64) 8)
                               ((:float) 4)
                               ((:double) 8)))))
      (declare (type (unsigned-byte 32) tag len))
      ;; Two value: the full size of the packed object, and the size
      ;; of just the payload
      (values (i+ (length32 tag) (length32 len) len) len))))

(defun enum-size (val enum field)
  "Returns the size in bytes that the enum object will take when serialized."
  (let ((val (let ((e (find val (proto-values enum) :key #'proto-value)))
               (and e (proto-index e))))
        (tag (ilogior $wire-type-varint (iash (proto-index field) 3))))
    (declare (type (unsigned-byte 32) tag val))
    (i+ (length32 tag) (length32 val))))


;;; Raw encoders

(defun encode-uint32 (val buffer index)
  "Encodes the unsigned 32-bit integer 'val' as a varint into the buffer
   at the given index.
   Modifies the buffer, and returns the new index into the buffer."
  (declare (type (unsigned-byte 32) val)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    ;; Seven bits at a time, least significant bits first
    (loop do (let ((bits (ldb #.(byte 7 0) val)))
               (declare (type (unsigned-byte 8) bits))
               (setq val (ash val -7))
               (setf (aref buffer index) (ilogior bits (if (zerop val) 0 128)))
               (iincf index))
          until (zerop val)))
  (values index buffer))                        ;return the buffer to improve 'trace'

(defun encode-uint64 (val buffer index)
  "Encodes the unsigned 64-bit integer 'val' as a varint into the buffer
   at the given index.
   Modifies the buffer, and returns the new index into the buffer."
  (declare (type (unsigned-byte 64) val)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop do (let ((bits (ldb #.(byte 7 0) val)))
               (declare (type (unsigned-byte 8) bits))
               (setq val (ash val -7))
               (setf (aref buffer index) (ilogior bits (if (zerop val) 0 128)))
               (iincf index))
          until (zerop val)))
  (values index buffer))

(defun encode-fixed32 (val buffer index)
  "Encodes the unsigned 32-bit integer 'val' as a fixed int into the buffer
   at the given index.
   Modifies the buffer, and returns the new index into the buffer."
  (declare (type (unsigned-byte 32) val)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop repeat 4 doing
      (let ((byte (ldb #.(byte 8 0) val)))
        (declare (type (unsigned-byte 8) byte))
        (setq val (ash val -8))
        (setf (aref buffer index) byte)
        (iincf index))))
  (values index buffer))

(defun encode-fixed64 (val buffer index)
  "Encodes the unsigned 64-bit integer 'val' as a fixed int into the buffer
   at the given index.
   Modifies the buffer, and returns the new index into the buffer."
  (declare (type (unsigned-byte 64) val)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop repeat 8 doing
      (let ((byte (ldb #.(byte 8 0) val)))
        (declare (type (unsigned-byte 8) byte))
        (setq val (ash val -8))
        (setf (aref buffer index) byte)
        (iincf index))))
  (values index buffer))

(defun encode-sfixed32 (val buffer index)
  "Encodes the signed 32-bit integer 'val' as a fixed int into the buffer
   at the given index.
   Modifies the buffer, and returns the new index into the buffer."
  (declare (type (signed-byte 32) val)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop repeat 4 doing
      (let ((byte (ldb #.(byte 8 0) val)))
        (declare (type (unsigned-byte 8) byte))
        (setq val (ash val -8))
        (setf (aref buffer index) byte)
        (iincf index))))
  (values index buffer))

(defun encode-sfixed64 (val buffer index)
  "Encodes the signed 32-bit integer 'val' as a fixed int into the buffer
   at the given index.
   Modifies the buffer, and returns the new index into the buffer."
  (declare (type (signed-byte 64) val)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop repeat 8 doing
      (let ((byte (ldb #.(byte 8 0) val)))
        (declare (type (unsigned-byte 8) byte))
        (setq val (ash val -8))
        (setf (aref buffer index) byte)
        (iincf index))))
  (values index buffer))

(defun encode-single (val buffer index)
  "Encodes the single float 'val' into the buffer at the given index.
   Modifies the buffer, and returns the new index into the buffer."
  (declare (type single-float val)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((bits (single-float-bits val)))
      (loop repeat 4 doing
        (let ((byte (ldb #.(byte 8 0) bits)))
          (declare (type (unsigned-byte 8) byte))
          (setq bits (ash bits -8))
          (setf (aref buffer index) byte)
          (iincf index)))))
  (values index buffer))

(defun encode-double (val buffer index)
  "Encodes the double float 'val' into the buffer at the given index.
   Modifies the buffer, and returns the new index into the buffer."
  (declare (type double-float val)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (multiple-value-bind (low high)
        (double-float-bits val)
      (loop repeat 4 doing
        (let ((byte (ldb #.(byte 8 0) low)))
          (declare (type (unsigned-byte 8) byte))
          (setq low (ash low -8))
          (setf (aref buffer index) byte)
          (iincf index)))
      (loop repeat 4 doing
        (let ((byte (ldb #.(byte 8 0) high)))
          (declare (type (unsigned-byte 8) byte))
          (setq high (ash high -8))
          (setf (aref buffer index) byte)
          (iincf index)))))
  (values index buffer))

(defun encode-octets (octets buffer index)
  "Encodes the octets into the buffer at the given index.
   Modifies the buffer, and returns the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let* ((len (length octets))
           (idx (encode-uint32 len buffer index)))
      (declare (type fixnum len)
               (type (unsigned-byte 32) idx))
      (replace buffer octets :start1 idx)
      (values (i+ idx len) buffer))))

(defun zig-zag-encode32 (val)
  (check-type val (signed-byte 32))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (logxor (ash val 1) (ash val -31))))

(defun zig-zag-encode64 (val)
  (check-type val (signed-byte 64))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (logxor (ash val 1) (ash val -63))))


;;; Raw decoders

;; Decode the value from the buffer at the given index,
;; then return the value and new index into the buffer
(defun decode-uint32 (buffer index)
  "Decodes the next 32-bit varint integer in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    ;; Seven bits at a time, least significant bits first
    (loop with val = 0
          for places fixnum upfrom 0 by 7
          for byte fixnum = (prog1 (aref buffer index) (iincf index))
          do (setq val (logior val (ash (ldb #.(byte 7 0) byte) places)))
          until (i< byte 128)
          finally (progn
                    (assert (< val #.(ash 1 32)) ()
                            "The value ~D is longer than 32 bits" val)
                    (return (values val index))))))

(defun decode-uint64 (buffer index)
  "Decodes the next 64-bit varint integer in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    ;; Seven bits at a time, least significant bits first
    (loop with val = 0
          for places fixnum upfrom 0 by 7
          for byte fixnum = (prog1 (aref buffer index) (iincf index))
          do (setq val (logior val (ash (ldb #.(byte 7 0) byte) places)))
          until (i< byte 128)
          finally (return (values val index)))))

(defun decode-fixed32 (buffer index)
  "Decodes the next 32-bit unsigned fixed integer in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    ;; Eight bits at a time, least significant bits first
    (let ((val 0))
      (loop repeat 4
            for places fixnum upfrom 0 by 8
            for byte fixnum = (prog1 (aref buffer index) (iincf index))
            do (setq val (logior val (ash byte places))))
      (values val index))))

(defun decode-sfixed32 (buffer index)
  "Decodes the next 32-bit signed fixed integer in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    ;; Eight bits at a time, least significant bits first
    (let ((val 0))
      (loop repeat 4
            for places fixnum upfrom 0 by 8
            for byte fixnum = (prog1 (aref buffer index) (iincf index))
            do (setq val (logior val (ash byte places))))
      (when (i= (ldb #.(byte 1 31) val) 1)              ;sign bit set, so negative value
        (decf val #.(ash 1 32)))
      (values val index))))

(defun decode-fixed64 (buffer index)
  "Decodes the next unsigned 64-bit fixed integer in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    ;; Eight bits at a time, least significant bits first
    (let ((val 0))
      (loop repeat 8
            for places fixnum upfrom 0 by 8
            for byte fixnum = (prog1 (aref buffer index) (iincf index))
            do (setq val (logior val (ash byte places))))
      (values val index))))

(defun decode-sfixed64 (buffer index)
  "Decodes the next signed 64-bit fixed integer in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    ;; Eight bits at a time, least significant bits first
    (let ((val 0))
      (loop repeat 8
            for places fixnum upfrom 0 by 8
            for byte fixnum = (prog1 (aref buffer index) (iincf index))
            do (setq val (logior val (ash byte places))))
      (when (i= (ldb #.(byte 1 63) val) 1)             ;sign bit set, so negative value
        (decf val #.(ash 1 64)))
      (values val index))))

(defun decode-single (buffer index)
  "Decodes the next single float in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    ;; Eight bits at a time, least significant bits first
    (let ((bits 0))
      (loop repeat 4
            for places fixnum upfrom 0 by 8
            for byte fixnum = (prog1 (aref buffer index) (iincf index))
            do (setq bits (logior bits (ash byte places))))
      (when (i= (ldb #.(byte 1 31) bits) 1)             ;sign bit set, so negative value
        (decf bits #.(ash 1 32)))
      (values (make-single-float bits) index))))

(defun decode-double (buffer index)
  "Decodes the next double float in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    ;; Eight bits at a time, least significant bits first
    (let ((low  0)
          (high 0))
      (loop repeat 4
            for places fixnum upfrom 0 by 8
            for byte fixnum = (prog1 (aref buffer index) (iincf index))
            do (setq low (logior low (ash byte places))))
      (loop repeat 4
            for places fixnum upfrom 0 by 8
            for byte fixnum = (prog1 (aref buffer index) (iincf index))
            do (setq high (logior high (ash byte places))))
      ;; High bits are signed, but low bits are unsigned
      (when (i= (ldb #.(byte 1 31) high) 1)             ;sign bit set, so negative value
        (decf high #.(ash 1 32)))
      (values (make-double-float low high) index))))

(defun decode-octets (buffer index)
  "Decodes the next octets in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (multiple-value-bind (len idx)
        (decode-uint32 buffer index)
      (declare (type (unsigned-byte 32) len)
               (type fixnum idx))
      (values (subseq buffer idx (i+ idx len)) (i+ idx len)))))

(defun zig-zag-decode32 (val)
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (logxor (ash val -1) (- (logand val 1)))))

(defun zig-zag-decode64 (val)
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (logxor (ash val -1) (- (logand val 1)))))


;;; Raw lengths

(defun length32 (val)
  "Returns the length that 'val' will take when encoded as a 32-bit integer."
  (assert (< val #.(ash 1 32)) ()
          "The value ~D is longer than 32 bits" val)
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((size 0))
      (declare (type fixnum size))
      (loop do (progn
                 (setq val (ash val -7))
                 (iincf size))
            until (zerop val))
      size)))

(defun length64 (val)
  "Returns the length that 'val' will take when encoded as a 64-bit integer."
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((size 0))
      (declare (type fixnum size))
      (loop do (progn
                 (setq val (ash val -7))
                 (iincf size))
            until (zerop val))
      size)))
