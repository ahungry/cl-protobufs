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

;;; Utilities

(defconstant $wire-type-varint 0)
(defconstant $wire-type-64bit  1)
(defconstant $wire-type-string 2)
(defconstant $wire-type-32bit  5)

(defun make-tag (type index)
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (if (typep type 'fixnum)
      type
      (let ((type (ecase type
                    ((:int32 :uint32) $wire-type-varint)
                    ((:int64 :uint64) $wire-type-varint)
                    ((:sint32 :sint64) $wire-type-varint)
                    ((:fixed32 :sfixed32) $wire-type-32bit)
                    ((:fixed64 :sfixed64) $wire-type-64bit)
                    ((:string :bytes) $wire-type-string)
                    ((:bool) $wire-type-varint)
                    ((:float) $wire-type-32bit)
                    ((:double) $wire-type-64bit)
                    ;; A few of our homegrown types
                    ((:symbol) $wire-type-string)
                    ((:date :time :datetime :timestamp) $wire-type-64bit))))
        (ilogior type (iash index 3))))))

(define-compiler-macro make-tag (&whole form type index)
  (cond ((typep type 'fixnum)
         `(ilogior ,type (iash ,index 3)))
        ((keywordp type)
         (let ((type (ecase type
                       ((:int32 :uint32) $wire-type-varint)
                       ((:int64 :uint64) $wire-type-varint)
                       ((:sint32 :sint64) $wire-type-varint)
                       ((:fixed32 :sfixed32) $wire-type-32bit)
                       ((:fixed64 :sfixed64) $wire-type-64bit)
                       ((:string :bytes) $wire-type-string)
                       ((:bool) $wire-type-varint)
                       ((:float) $wire-type-32bit)
                       ((:double) $wire-type-64bit)
                       ;; A few of our homegrown types
                       ((:symbol) $wire-type-string)
                       ((:date :time :datetime :timestamp) $wire-type-64bit))))
           `(ilogior ,type (iash ,index 3))))
        (t form)))


;;; Serializers

;; Serialize 'val' of primitive type 'type' into the buffer
(defun serialize-prim (val type tag buffer index)
  "Serializes a Protobufs primitive (scalar) value into the buffer at the given index.
   The value is given by 'val', the primitive type by 'type'.
   Modifies the buffer in place, and returns the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type (unsigned-byte 32) tag)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((idx (encode-uint32 tag buffer index)))
      (declare (type fixnum idx))
      (ecase type
        ((:int32 :uint32)
         (encode-uint32 val buffer idx))
        ((:int64 :uint64)
         (encode-uint64 val buffer idx))
        ((:sint32)
         (encode-uint32 (zig-zag-encode32 val) buffer idx))
        ((:sint64)
         (encode-uint64 (zig-zag-encode64 val) buffer idx))
        ((:fixed32)
         (encode-fixed32 val buffer idx))
        ((:sfixed32)
         (encode-sfixed32 val buffer idx))
        ((:fixed64)
         (encode-fixed64 val buffer idx))
        ((:sfixed64)
         (encode-sfixed64 val buffer idx))
        ((:string)
         (encode-octets (babel:string-to-octets val :encoding :utf-8) buffer idx))
        ((:bytes)
         (encode-octets val buffer idx))
        ((:bool)
         (encode-uint32 (if val 1 0) buffer idx))
        ((:float)
         (encode-single val buffer idx))
        ((:double)
         (encode-double val buffer idx))
        ;; A few of our homegrown types
        ((:symbol)
         (let ((val (format nil "~A:~A" (package-name (symbol-package val)) (symbol-name val))))
           ;; Call 'string' in case we are trying to serialize a symbol name
           (encode-octets (babel:string-to-octets val :encoding :utf-8) buffer idx)))
        ((:date :time :datetime :timestamp)
         (encode-uint64 val buffer idx))))))

(define-compiler-macro serialize-prim (&whole form val type tag buffer index)
  (case type
    ((:int32 :uint32)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (let ((idx (encode-uint32 ,tag ,buffer ,index)))
          (declare (type fixnum idx))
          (encode-uint32 ,val ,buffer idx))))
    ((:int64 :uint64)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (let ((idx (encode-uint32 ,tag ,buffer ,index)))
          (declare (type fixnum idx))
          (encode-uint64 ,val ,buffer idx))))
   ((:sint32)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-uint32 (zig-zag-encode32 ,val) ,buffer idx))))
   ((:sint64)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-uint64 (zig-zag-encode64 ,val) ,buffer idx))))
   ((:fixed32)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-fixed32 ,val ,buffer idx))))
   ((:sfixed32)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-sfixed32 ,val ,buffer idx))))
   ((:fixed64)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-fixed64 ,val ,buffer idx))))
   ((:sfixed64)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-sfixed64 ,val ,buffer idx))))
   ((:string)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-octets (babel:string-to-octets ,val :encoding :utf-8) ,buffer idx))))
   ((:bytes)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-octets ,val ,buffer idx))))
   ((:bool)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-uint32 (if ,val 1 0) ,buffer idx))))
   ((:float)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-single ,val ,buffer idx))))
   ((:double)
    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (encode-double ,val ,buffer idx))))
   (otherwise form)))

(defun serialize-packed (values type tag buffer index)
  "Serializes a set of packed values into the buffer at the given index.
   The values are given by 'values', the primitive type by 'type'.
   Modifies the buffer in place, and returns the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type (unsigned-byte 32) tag)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((idx (encode-uint32 tag buffer index)))
      (declare (type fixnum idx))
      (multiple-value-bind (full-len len)
          (packed-size values type tag)
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

(defun serialize-enum (val values tag buffer index)
  "Serializes a Protobufs enum value into the buffer at the given index.
   The value is given by 'val', the enum values are in 'values'.
   Modifies the buffer in place, and returns the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type (unsigned-byte 32) tag)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let* ((val (let ((e (find val values :key #'proto-value)))
                  (and e (proto-index e))))
           (idx (encode-uint32 tag buffer index)))
      (declare (type (unsigned-byte 32) val)
               (type fixnum idx))
      (encode-uint32 val buffer idx))))


;;; Deserializers

;; Deserialize the next object of type 'type'
(defun deserialize-prim (type buffer index)
  "Deserializes the next object of primitive type 'type'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
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

(defun deserialize-packed (type buffer index)
  "Deserializes the next packed values of type 'type'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
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

(defun deserialize-enum (values buffer index)
  "Deserializes the next enum value take from 'values'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and and the new index into the buffer."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (multiple-value-bind (val idx)
        (decode-uint32 buffer index)
      (let ((val (let ((e (find val values :key #'proto-index)))
                   (and e (proto-value e)))))
        (values val idx)))))


;;; Object sizing

(defun prim-size (val type tag)
  "Returns the size in bytes that the primitive object will take when serialized."
  (declare (type (unsigned-byte 32) tag))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (ecase type
      ((:int32 :uint32)
       (i+ (length32 tag) (length32 val)))
      ((:int64 :uint64)
       (i+ (length32 tag) (length64 val)))
      ((:sint32)
       (i+ (length32 tag) (length32 (zig-zag-encode32 val))))
      ((:sint64)
       (i+ (length32 tag) (length64 (zig-zag-encode64 val))))
      ((:fixed32 :sfixed32)
       (i+ (length32 tag) 4))
      ((:fixed64 :sfixed64)
       (i+ (length32 tag) 8))
      ((:string)
       (let ((len (babel:string-size-in-octets val :encoding :utf-8)))
         (i+ (length32 tag) (length32 len) len)))
      ((:bytes)
       (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let ((len (length val)))
           (i+ (length32 tag) (length32 len) len))))
      ((:bool)
       (i+ (length32 tag) 1))
      ((:float)
       (i+ (length32 tag) 4))
      ((:double)
       (i+ (length32 tag) 8))
      ;; A few of our homegrown types
      ((:symbol)
       (let* ((len (i+ (length (package-name (symbol-package val))) 1 (length (symbol-name val)))))
         (i+ (length32 tag) (length32 len) len)))
      ((:date :time :datetime :timestamp)
       (i+ (length32 tag) 8)))))

(define-compiler-macro prim-size (&whole form val type tag)
  (case type
    ((:int32 :uint32)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (i+ (length32 ,tag) (length32 ,val))))
    ((:int64 :uint64)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (i+ (length32 ,tag) (length64 ,val))))
    ((:sint32)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (i+ (length32 ,tag) (length32 (zig-zag-encode32 ,val)))))
    ((:sint64)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (i+ (length32 ,tag) (length64 (zig-zag-encode64 ,val)))))
    ((:fixed32 :sfixed32)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (i+ (length32 ,tag) 4)))
    ((:fixed64 :sfixed64)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (i+ (length32 ,tag) 8)))
    ((:string)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (let ((len (babel:string-size-in-octets ,val :encoding :utf-8)))
          (i+ (length32 ,tag) (length32 len) len))))
    ((:bytes)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (let ((len (length ,val)))
          (i+ (length32 ,tag) (length32 len) len))))
    ((:bool)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (i+ (length32 ,tag) 1)))
    ((:float)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (i+ (length32 ,tag) 4)))
    ((:double)
     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
        (i+ (length32 ,tag) 8)))
    (otherwise form)))

(defun packed-size (values type tag)
  "Returns the size in bytes that the packed object will take when serialized."
  (declare (type (unsigned-byte 32) tag))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((len (loop for val in values
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

(defun enum-size (val values tag)
  "Returns the size in bytes that the enum object will take when serialized."
  (declare (type (unsigned-byte 32) tag))
  (let ((val (let ((e (find val values :key #'proto-value)))
               (and e (proto-index e)))))
    (declare (type (unsigned-byte 32) val))
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
  (declare (type (signed-byte 32) val))
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (logxor (ash val 1) (ash val -31))))

(defun zig-zag-encode64 (val)
  (declare (type (signed-byte 64) val))
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
