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


;;; Protocol buffers wire format

;;; Utilities

(eval-when (:compile-toplevel :load-toplevel :execute)

;; If you need to debug the (de)serializer, (pushnew :debug-serialization *features*)
;; Otherwise, we try to make (de)serialization as fast as possible,
;; risking life and limb to do so
(defparameter $optimize-serialization
  #+debug-serialization $optimize-default
  #-debug-serialization $optimize-fast-unsafe)

(defconstant $wire-type-varint 0)
(defconstant $wire-type-64bit  1)
(defconstant $wire-type-string 2)
(defconstant $wire-type-start-group 3)          ;supposedly deprecated, but no such luck
(defconstant $wire-type-end-group   4)          ;supposedly deprecated
(defconstant $wire-type-32bit  5)

)       ;eval-when


(defun make-tag (type index)
  "Given a wire type or the name of a Protobufs type and a field index,
   return the tag that encodes both of them."
  (locally (declare #.$optimize-serialization)
    (if (typep type 'fixnum)
      (ilogior type (iash index 3))
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
  (setq type (fold-symbol type))
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

(defun fold-symbol (x)
  "Given an expression 'x', constant-fold it until it can be folded no more."
  (let ((last '#:last))
    (loop
      (cond ((eq x last) (return x))
            ((and (listp x)
                  (eq (first x) 'quote)
                  (constantp (second x)))
             (shiftf last x (second x)))
            ((and (symbolp x)
                  (boundp x))
             (shiftf last x (symbol-value x)))
            (t (return x))))))


(defmacro gen-zig-zag (bits)
  "Generate 32- or 64-bit versions of zig-zag encoder/decoder."
  (assert (and (plusp bits) (zerop (mod bits 8))))
  (let* ((zig-zag-encode (fintern "~A~A" 'zig-zag-encode bits))
         (zig-zag-decode (fintern "~A~A" 'zig-zag-decode bits))
         (zig-zag-shift (1+ (- bits))))
    `(progn
       (defun ,zig-zag-encode (val)
         (declare #.$optimize-serialization)
         (declare (type (signed-byte ,bits) val))
         (logxor (ash val 1) (ash val ,zig-zag-shift)))
       (define-compiler-macro ,zig-zag-encode (&whole form val)
         (if (atom val)
           `(locally (declare #.$optimize-serialization
                              (type (signed-byte ,',bits) ,val))
              (logxor (ash ,val 1) (ash ,val ,',zig-zag-shift)))
           form))
       (defun ,zig-zag-decode (val)
         (declare #.$optimize-serialization)
         (declare (type (unsigned-byte ,bits) val))
         (logxor (ash val -1) (- (logand val 1))))
       (define-compiler-macro ,zig-zag-decode (&whole form val)
         (if (atom val)
           `(locally (declare #.$optimize-serialization
                              (type (unsigned-byte ,',bits) ,val))
              (logxor (ash ,val -1) (- (logand ,val 1))))
           form)))))

(gen-zig-zag 32)
(gen-zig-zag 64)


;;; Serializers

;; Serialize 'val' of primitive type 'type' into the buffer
(defun serialize-prim (val type tag buffer index)
  "Serializes a Protobufs primitive (scalar) value into the buffer at the given index.
   The value is given by 'val', the primitive type by 'type'.
   Modifies the buffer in place, and returns the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (type (unsigned-byte 32) tag)
           (type fixnum index))
  (locally (declare #.$optimize-serialization)
    (let ((idx (encode-uint32 tag buffer index)))
      (declare (type fixnum idx))
      (ecase type
        ((:int32 :int64)
         (encode-int val buffer idx))
        ((:uint32)
         (encode-uint32 val buffer idx))
        ((:uint64)
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
         (encode-string val buffer idx))
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
         (let ((val (if (keywordp val)
                      (string val)
                      ;; Non-keyword symbols are consy, avoid them if possible
                      (format nil "~A:~A" (package-name (symbol-package val)) (symbol-name val)))))
           (encode-string val buffer idx)))
        ((:date :time :datetime :timestamp)
         (encode-uint64 val buffer idx))))))

(define-compiler-macro serialize-prim (&whole form val type tag buffer index)
  (setq type (fold-symbol type)
        tag  (fold-symbol tag))
  (if (member type '(:int32 :uint32 :int64 :uint64 :sint32 :sint64
                     :fixed32 :sfixed32 :fixed64 :sfixed64
                     :string :bytes :bool :float :double))
    `(locally (declare #.$optimize-serialization
                       (type (simple-array (unsigned-byte 8)) ,buffer)
                       ;; 'tag' is a constant, no need to declare its type
                       (type fixnum ,index))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         ,(ecase type
            ((:int32 :int64)
             `(encode-int ,val ,buffer idx))
            ((:uint32)
             `(encode-uint32 ,val ,buffer idx))
            ((:uint64)
             `(encode-uint64 ,val ,buffer idx))
            ((:sint32)
             `(encode-uint32 (zig-zag-encode32 ,val) ,buffer idx))
            ((:sint64)
             `(encode-uint64 (zig-zag-encode64 ,val) ,buffer idx))
            ((:fixed32)
             `(encode-fixed32 ,val ,buffer idx))
            ((:sfixed32)
             `(encode-sfixed32 ,val ,buffer idx))
            ((:fixed64)
             `(encode-fixed64 ,val ,buffer idx))
            ((:sfixed64)
             `(encode-sfixed64 ,val ,buffer idx))
            ((:string)
             `(encode-string ,val ,buffer idx))
            ((:bytes)
             `(encode-octets ,val ,buffer idx))
            ((:bool)
             `(encode-uint32 (if ,val 1 0) ,buffer idx))
            ((:float)
             `(encode-single ,val ,buffer idx))
            ((:double)
             `(encode-double ,val ,buffer idx)))))
    form))

(defun serialize-packed (values type tag buffer index &optional vectorp)
  "Serializes a set of packed values into the buffer at the given index.
   The values are given by 'values', the primitive type by 'type'.
   Modifies the buffer in place, and returns the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (ignore vectorp)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type (unsigned-byte 32) tag)
           (type fixnum index))
  (locally (declare #.$optimize-serialization)
    (let ((idx (encode-uint32 tag buffer index)))
      (declare (type fixnum idx))
      (multiple-value-bind (full-len len)
          (packed-size values type tag)
        (declare (type fixnum len) (ignore full-len))
        (setq idx (encode-uint32 len buffer idx)))
      (ecase type
        ((:int32 :int64)
         (map () #'(lambda (val) (setq idx (encode-int val buffer idx))) values))
        ((:uint32)
         (map () #'(lambda (val) (setq idx (encode-uint32 val buffer idx))) values))
        ((:uint64)
         (map () #'(lambda (val) (setq idx (encode-uint64 val buffer idx))) values))
        ((:sint32)
         (map () #'(lambda (val) (setq idx (encode-uint32 (zig-zag-encode32 val) buffer idx))) values))
        ((:sint64)
         (map () #'(lambda (val) (setq idx (encode-uint64 (zig-zag-encode64 val) buffer idx))) values))
        ((:fixed32)
         (map () #'(lambda (val) (setq idx (encode-fixed32 val buffer idx))) values))
        ((:sfixed32)
         (map () #'(lambda (val) (setq idx (encode-sfixed32 val buffer idx))) values))
        ((:fixed64)
         (map () #'(lambda (val) (setq idx (encode-fixed64 val buffer idx))) values))
        ((:sfixed64)
         (map () #'(lambda (val) (setq idx (encode-sfixed64 val buffer idx))) values))
        ((:bool)
         (map () #'(lambda (val) (setq idx (encode-uint32 (if val 1 0) buffer idx))) values))
        ((:float)
         (map () #'(lambda (val) (setq idx (encode-single val buffer idx))) values))
        ((:double)
         (map () #'(lambda (val) (setq idx (encode-double val buffer idx))) values)))
      idx)))

;; The optimized serializers supply 'vectorp' so we can generate better code
(define-compiler-macro serialize-packed (&whole form values type tag buffer index
                                         &optional (vectorp nil vectorp-p))
  (setq type (fold-symbol type)
        tag  (fold-symbol tag))
  (if (and vectorp-p
           `(member type '(:int32 :uint32 :int64 :uint64 :sint32 :sint64
                           :fixed32 :sfixed32 :fixed64 :sfixed64
                           :bool :float :double)))
    `(locally (declare #.$optimize-serialization
                       (type (simple-array (unsigned-byte 8)) ,buffer)
                       ;; 'tag' is a constant, no need to declare its type
                       (type fixnum ,index))
       (let ((idx (encode-uint32 ,tag ,buffer ,index)))
         (declare (type fixnum idx))
         (multiple-value-bind (full-len len)
             (packed-size ,values ,type ,tag)
           (declare (type fixnum len) (ignore full-len))
           (setq idx (encode-uint32 len ,buffer idx)))
         (,(if vectorp 'dovector 'dolist) (val ,values)
            ,(ecase type
               ((:int32 :int64)
                `(setq idx (encode-int val ,buffer idx)))
               ((:uint32)
                `(setq idx (encode-uint32 val ,buffer idx)))
               ((:uint64)
                `(setq idx (encode-uint64 val ,buffer idx)))
               ((:sint32)
                `(setq idx (encode-uint32 (zig-zag-encode32 val) ,buffer idx)))
               ((:sint64)
                `(setq idx (encode-uint64 (zig-zag-encode64 val) ,buffer idx)))
               ((:fixed32)
                `(setq idx (encode-fixed32 val ,buffer idx)))
               ((:sfixed32)
                `(setq idx (encode-sfixed32 val ,buffer idx)))
               ((:fixed64)
                `(setq idx (encode-fixed64 val ,buffer idx)))
               ((:sfixed64)
                `(setq idx (encode-sfixed64 val ,buffer idx)))
               ((:bool)
                `(setq idx (encode-uint32 (if val 1 0) ,buffer idx)))
               ((:float)
                `(setq idx (encode-single val ,buffer idx)))
               ((:double)
                `(setq idx (encode-double val ,buffer idx)))))
         idx))
    form))

(defun serialize-enum (val enum-values tag buffer index)
  "Serializes a Protobufs enum value into the buffer at the given index.
   The value is given by 'val', the enum values are in 'enum-values'.
   Modifies the buffer in place, and returns the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (type list enum-values)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type (unsigned-byte 32) tag)
           (type fixnum index))
  (locally (declare #.$optimize-serialization)
    (let* ((val (let ((e (find val enum-values :key #'proto-value)))
                  (and e (proto-index e))))
           (idx (encode-uint32 tag buffer index)))
      (declare (type (unsigned-byte 32) val)
               (type fixnum idx))
      (encode-uint32 val buffer idx))))

(defun serialize-packed-enum (values enum-values tag buffer index)
  "Serializes Protobufs enum values into the buffer at the given index.
   The values are given by 'values', the enum values are in 'enum-values'.
   Modifies the buffer in place, and returns the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (type list enum-values)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type (unsigned-byte 32) tag)
           (type fixnum index))
  (locally (declare #.$optimize-serialization)
    (let ((idx (encode-uint32 tag buffer index)))
      (declare (type fixnum idx))
      (multiple-value-bind (full-len len)
          (packed-enum-size values enum-values tag)
        (declare (type fixnum len) (ignore full-len))
        (setq idx (encode-uint32 len buffer idx)))
      (map () #'(lambda (val)
                  (let ((val (let ((e (find val enum-values :key #'proto-value)))
                               (and e (proto-index e)))))
                    (declare (type (unsigned-byte 32) val))
                    (setq idx (encode-uint32 val buffer idx)))) values)
      idx)))


;;; Deserializers

;; Deserialize the next object of type 'type'
(defun deserialize-prim (type buffer index)
  "Deserializes the next object of primitive type 'type'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and and the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare #.$optimize-serialization)
    (ecase type
      ((:int32 :int64)
       (decode-int buffer index))
      ((:uint32)
       (decode-uint32 buffer index))
      ((:uint64)
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
       (decode-string buffer index))
      ((:bytes)
       (decode-octets buffer index))
      ((:bool)
       (multiple-value-bind (val idx)
           (decode-uint32 buffer index)
         (values (if (i= val 0) nil t) idx)))
      ((:float)
       (decode-single buffer index))
      ((:double)
       (decode-double buffer index))
      ;; A few of our homegrown types
      ((:symbol)
       ;; Note that this is consy, avoid it if possible
       (multiple-value-bind (val idx)
           (decode-string buffer index)
         (values (make-lisp-symbol val) idx)))
      ((:date :time :datetime :timestamp)
       (decode-uint64 buffer index)))))

(define-compiler-macro deserialize-prim (&whole form type buffer index)
  (setq type (fold-symbol type))
  (if (member type '(:int32 :uint32 :int64 :uint64 :sint32 :sint64
                     :fixed32 :sfixed32 :fixed64 :sfixed64
                     :string :bytes :bool :float :double))
    `(locally (declare #.$optimize-serialization
                       (type (simple-array (unsigned-byte 8)) ,buffer)
                       (type fixnum ,index))
       ,(ecase type
          ((:int32 :int64)
           `(decode-int ,buffer ,index))
          ((:uint32)
           `(decode-uint32 ,buffer ,index))
          ((:uint64)
           `(decode-uint64 ,buffer ,index))
          ((:sint32)
           `(multiple-value-bind (val idx)
                (decode-uint32 ,buffer ,index)
              (values (zig-zag-decode32 val) idx)))
          ((:sint64)
           `(multiple-value-bind (val idx)
               (decode-uint64 ,buffer ,index)
             (values (zig-zag-decode64 val) idx)))
          ((:fixed32)
           `(decode-fixed32 ,buffer ,index))
          ((:sfixed32)
           `(decode-sfixed32 ,buffer ,index))
          ((:fixed64)
           `(decode-fixed64 ,buffer ,index))
          ((:sfixed64)
           `(decode-sfixed64 ,buffer ,index))
          ((:string)
           `(decode-string ,buffer ,index))
          ((:bytes)
           `(decode-octets ,buffer ,index))
          ((:bool)
           `(multiple-value-bind (val idx)
                (decode-uint32 ,buffer ,index)
              (values (if (i= val 0) nil t) idx)))
          ((:float)
           `(decode-single ,buffer ,index))
          ((:double)
           `(decode-double ,buffer ,index))))
    form))

(defun deserialize-packed (type buffer index)
  "Deserializes the next packed values of type 'type'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and and the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare #.$optimize-serialization)
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
                  ((:int32 :int64)
                   (decode-int buffer idx))
                  ((:uint32)
                   (decode-uint32 buffer idx))
                  ((:uint64)
                   (decode-uint64 buffer idx))
                  ((:sint32)
                   (multiple-value-bind (val nidx)
                       (decode-uint32 buffer idx)
                     (values (zig-zag-decode32 val) nidx)))
                  ((:sint64)
                   (multiple-value-bind (val nidx)
                       (decode-uint64 buffer idx)
                     (values (zig-zag-decode64 val) nidx)))
                  ((:fixed32)
                   (decode-fixed32 buffer idx))
                  ((:sfixed32)
                   (decode-sfixed32 buffer idx))
                  ((:fixed64)
                   (decode-fixed64 buffer idx))
                  ((:sfixed64)
                   (decode-sfixed64 buffer idx))
                  ((:bool)
                   (multiple-value-bind (val nidx)
                       (decode-uint32 buffer idx)
                     (values (if (i= val 0) nil t) nidx)))
                  ((:float)
                   (decode-single buffer idx))
                  ((:double)
                   (decode-double buffer idx)))
              (collect-value val)
              (setq idx nidx))))))))

(define-compiler-macro deserialize-packed (&whole form type buffer index)
  (setq type (fold-symbol type))
  (if (member type '(:int32 :uint32 :int64 :uint64 :sint32 :sint64
                     :fixed32 :sfixed32 :fixed64 :sfixed64
                     :bool :float :double))
    `(locally (declare #.$optimize-serialization
                       (type (simple-array (unsigned-byte 8)) ,buffer)
                       (type fixnum ,index))
       (block deserialize-packed
         (multiple-value-bind (len idx)
             (decode-uint32 ,buffer ,index)
           (declare (type (unsigned-byte 32) len)
                    (type fixnum idx))
           (let ((end (i+ idx len)))
             (declare (type (unsigned-byte 32) end))
             (with-collectors ((values collect-value))
               (loop
                 (when (>= idx end)
                   (return-from deserialize-packed (values values idx)))
                 (multiple-value-bind (val nidx)
                     ,(ecase type
                        ((:int32 :int64)
                         `(decode-int ,buffer idx))
                        ((:uint32)
                         `(decode-uint32 ,buffer idx))
                        ((:uint64)
                         `(decode-uint64 ,buffer idx))
                        ((:sint32)
                         `(multiple-value-bind (val nidx)
                              (decode-uint32 ,buffer idx)
                            (values (zig-zag-decode32 val) nidx)))
                        ((:sint64)
                         `(multiple-value-bind (val nidx)
                              (decode-uint64 ,buffer idx)
                            (values (zig-zag-decode64 val) nidx)))
                        ((:fixed32)
                         `(decode-fixed32 ,buffer idx))
                        ((:sfixed32)
                         `(decode-sfixed32 ,buffer idx))
                        ((:fixed64)
                         `(decode-fixed64 ,buffer idx))
                        ((:sfixed64)
                         `(decode-sfixed64 ,buffer idx))
                        ((:bool)
                         `(multiple-value-bind (val nidx)
                              (decode-uint32 ,buffer idx)
                            (values (if (i= val 0) nil t) nidx)))
                        ((:float)
                         `(decode-single ,buffer idx))
                        ((:double)
                         `(decode-double ,buffer idx)))
                   (collect-value val)
                   (setq idx nidx))))))))
    form))

(defun deserialize-enum (enum-values buffer index)
  "Deserializes the next enum value take from 'enum-values'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and and the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (type list enum-values)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare #.$optimize-serialization)
    (multiple-value-bind (val idx)
        (decode-int buffer index)
      (let ((val (let ((e (find val enum-values :key #'proto-index)))
                   (and e (proto-value e)))))
        (values val idx)))))

(defun deserialize-packed-enum (enum-values buffer index)
  "Deserializes the next packed enum values given in 'enum-values'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and and the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (type list enum-values)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (locally (declare #.$optimize-serialization)
    (multiple-value-bind (len idx)
        (decode-uint32 buffer index)
      (declare (type (unsigned-byte 32) len)
               (type fixnum idx))
      (let ((end (i+ idx len)))
        (declare (type (unsigned-byte 32) end))
        (with-collectors ((values collect-value))
          (loop
            (when (>= idx end)
              (return-from deserialize-packed-enum (values values idx)))
            (multiple-value-bind (val nidx)
                (decode-int buffer idx)
              (let ((val (let ((e (find val enum-values :key #'proto-index)))
                           (and e (proto-value e)))))
                (collect-value val)
                (setq idx nidx)))))))))


;;; Object sizing

(defun prim-size (val type tag)
  "Returns the size in bytes that the primitive object will take when serialized.
   Watch out, this function turns off most type checking."
  (declare (type (unsigned-byte 32) tag))
  (locally (declare #.$optimize-serialization)
    (ecase type
      ((:int32 :uint32 :int64 :uint64)
       (i+ (varint-length tag) (varint-length val)))
      ((:sint32)
       (i+ (varint-length tag) (varint-length (zig-zag-encode32 val))))
      ((:sint64)
       (i+ (varint-length tag) (varint-length (zig-zag-encode64 val))))
      ((:fixed32 :sfixed32)
       (i+ (varint-length tag) 4))
      ((:fixed64 :sfixed64)
       (i+ (varint-length tag) 8))
      ((:string)
       (let ((len (babel:string-size-in-octets val :encoding :utf-8)))
         (i+ (varint-length tag) (varint-length len) len)))
      ((:bytes)
       (let ((len (length val)))
         (i+ (varint-length tag) (varint-length len) len)))
      ((:bool)
       (i+ (varint-length tag) 1))
      ((:float)
       (i+ (varint-length tag) 4))
      ((:double)
       (i+ (varint-length tag) 8))
      ;; A few of our homegrown types
      ((:symbol)
       (let ((len (if (keywordp val)
                    (length (symbol-name val))
                    (i+ (length (package-name (symbol-package val))) 1 (length (symbol-name val))))))
         (i+ (varint-length tag) (varint-length len) len)))
      ((:date :time :datetime :timestamp)
       (i+ (varint-length tag) 8)))))

(define-compiler-macro prim-size (&whole form val type tag)
  (setq type (fold-symbol type)
        tag  (fold-symbol tag))
  (if (member type '(:int32 :uint32 :int64 :uint64 :sint32 :sint64
                     :fixed32 :sfixed32 :fixed64 :sfixed64
                     :string :bytes :bool :float :double))
    `(locally (declare #.$optimize-serialization)
       ,(ecase type
          ((:int32 :int64 :uint32 :uint64)
           `(i+ (varint-length ,tag) (varint-length ,val)))
          ((:sint32)
           `(i+ (varint-length ,tag) (varint-length (zig-zag-encode32 ,val))))
          ((:sint64)
           `(i+ (varint-length ,tag) (varint-length (zig-zag-encode64 ,val))))
          ((:fixed32 :sfixed32)
           `(i+ (varint-length ,tag) 4))
          ((:fixed64 :sfixed64)
           `(i+ (varint-length ,tag) 8))
          ((:string)
           `(let ((len (babel:string-size-in-octets ,val :encoding :utf-8)))
              (i+ (varint-length ,tag) (varint-length len) len)))
          ((:bytes)
           `(let ((len (length ,val)))
              (i+ (varint-length ,tag) (varint-length len) len)))
          ((:bool)
           `(i+ (varint-length ,tag) 1))
          ((:float)
           `(i+ (varint-length ,tag) 4))
          ((:double)
           `(i+ (varint-length ,tag) 8))))
    form))

(defun packed-size (values type tag &optional vectorp)
  "Returns the size in bytes that the packed object will take when serialized.
   Watch out, this function turns off most type checking."
  (declare (ignore vectorp)
           (type (unsigned-byte 32) tag))
  (locally (declare #.$optimize-serialization)
    (let ((len (let ((len 0))
                 (declare (type fixnum len))
                 (map () #'(lambda (val)
                             (iincf len (ecase type
                                          ((:int32 :uint32 :int64 :uint64) (varint-length val))
                                          ((:sint32) (varint-length (zig-zag-encode32 val)))
                                          ((:sint64) (varint-length (zig-zag-encode64 val)))
                                          ((:fixed32 :sfixed32) 4)
                                          ((:fixed64 :sfixed64) 8)
                                          ((:bool)   1)
                                          ((:float)  4)
                                          ((:double) 8)))) values)
                 len)))
      (declare (type (unsigned-byte 32) len))
      ;; Two value: the full size of the packed object, and the size
      ;; of just the payload
      (values (i+ (varint-length tag) (varint-length len) len) len))))

;; The optimized serializers supply 'vectorp' so we can generate better code
(define-compiler-macro packed-size (&whole form values type tag
                                    &optional (vectorp nil vectorp-p))
  (setq type (fold-symbol type)
        tag  (fold-symbol tag))
  (if (and vectorp-p
           (member type '(:int32 :uint32 :int64 :uint64 :sint32 :sint64
                          :fixed32 :sfixed32 :fixed64 :sfixed64
                          :bool :float :double)))
    `(locally (declare #.$optimize-serialization)
       (let ((len (let ((len 0))
                    (declare (type fixnum len))
                    (,(if vectorp 'dovector 'dolist) (val ,values)
                       (iincf len ,(ecase type
                                     ((:int32 :uint32 :int64 :uint64) `(varint-length val))
                                     ((:sint32) `(varint-length (zig-zag-encode32 val)))
                                     ((:sint64) `(varint-length (zig-zag-encode64 val)))
                                     ((:fixed32 :sfixed32) `4)
                                     ((:fixed64 :sfixed64) `8)
                                     ((:bool)   `1)
                                     ((:float)  `4)
                                     ((:double) `8))))
                    len)))
         (declare (type (unsigned-byte 32) len))
         (values (i+ (varint-length (the (unsigned-byte 32) ,tag)) (varint-length len) len) len)))
    form))

(defun enum-size (val enum-values tag)
  "Returns the size in bytes that the enum object will take when serialized."
  (declare (type list enum-values)
           (type (unsigned-byte 32) tag))
  (let ((idx (let ((e (find val enum-values :key #'proto-value)))
               (and e (proto-index e)))))
    (unless idx
      (serialization-error "There is no enum value for ~S" val))
    (i+ (varint-length tag) (varint-length (ldb (byte 32 0) idx)))))

(defun packed-enum-size (values enum-values tag)
  "Returns the size in bytes that the enum values will take when serialized."
  (declare (type list enum-values)
           (type (unsigned-byte 32) tag))
  (let ((len (let ((len 0))
               (declare (type fixnum len))
               (map () #'(lambda (val)
                           (let ((idx (let ((e (find val enum-values :key #'proto-value)))
                                        (and e (proto-index e)))))
                             (unless idx
                               (serialization-error "There is no enum value for ~S" val))
                             (iincf len (varint-length (ldb (byte 32 0) idx))))) values)
               len)))
    (declare (type (unsigned-byte 32) len))
    ;; Two value: the full size of the packed object, and the size
    ;; of just the payload
    (values (i+ (varint-length tag) (varint-length len) len) len)))


;;; Wire-level encoders
;;; These are called at the lowest level, so arg types are assumed to be correct

(defmacro generate-integer-encoders (bits)
  "Generate 32- or 64-bit versions of integer encoders."
  (assert (and (plusp bits) (zerop (mod bits 8))))
  (let* ((encode-uint   (fintern "~A~A" 'encode-uint bits))
         (encode-fixed  (fintern "~A~A" 'encode-fixed bits))
         (encode-sfixed (fintern "~A~A" 'encode-sfixed bits))
         (bytes (/ bits 8))
         ;; Given bits, can we use fixnums safely?
         (fixnump (<= bits (integer-length most-negative-fixnum)))
         (ldb (if fixnump 'ildb 'ldb))
         (ash (if fixnump 'iash 'ash))
         (zerop-val (if fixnump '(i= val 0) '(zerop val))))
    `(progn
       (defun ,encode-uint (val buffer index)
         ,(format nil
                  "Encodes the unsigned ~A-bit integer 'val' as a varint into the buffer at the given index.~
                   ~&    Modifies the buffer, and returns the new index into the buffer.~
                   ~&    Watch out, this function turns off all type checking and array bounds checking." bits)
         (declare #.$optimize-serialization)
         (let ((val (ldb (byte ,bits 0) val)))
           (declare (type (unsigned-byte ,bits) val)
                    (type (simple-array (unsigned-byte 8)) buffer)
                    (type fixnum index))
           ;; Seven bits at a time, least significant bits first
           (loop do (let ((bits (,ldb (byte 7 0) val)))
                      (declare (type (unsigned-byte 8) bits))
                      (setq val (,ash val -7))
                      (setf (aref buffer index)
                            (ilogior bits (if ,zerop-val 0 128)))
                      (iincf index))
                 until ,zerop-val))
         (values index buffer))                 ;return the buffer to improve 'trace'
       (defun ,encode-fixed (val buffer index)
         ,(format nil
                  "Encodes the unsigned ~A-bit integer 'val' as a fixed int into the buffer at the given index.~
                   ~&    Modifies the buffer, and returns the new index into the buffer.~
                   ~&    Watch out, this function turns off all type checking and array bounds checking." bits)
         (declare #.$optimize-serialization)
         (declare (type (unsigned-byte ,bits) val)
                  (type (simple-array (unsigned-byte 8)) buffer)
                  (type fixnum index))
         (loop repeat ,bytes doing
           (let ((byte (,ldb (byte 8 0) val)))
             (declare (type (unsigned-byte 8) byte))
             (setq val (,ash val -8))
             (setf (aref buffer index) byte)
             (iincf index)))
         (values index buffer))
       (defun ,encode-sfixed (val buffer index)
         ,(format nil
                  "Encodes the signed ~A-bit integer 'val' as a fixed int into the buffer at the given index.~
                   ~&    Modifies the buffer, and returns the new index into the buffer.~
                   ~&    Watch out, this function turns off all type checking and array bounds checking." bits)
         (declare #.$optimize-serialization)
         (declare (type (signed-byte ,bits) val)
                  (type (simple-array (unsigned-byte 8)) buffer)
                  (type fixnum index))
         (loop repeat ,bytes doing
           (let ((byte (,ldb (byte 8 0) val)))
             (declare (type (unsigned-byte 8) byte))
             (setq val (,ash val -8))
             (setf (aref buffer index) byte)
             (iincf index)))
         (values index buffer)))))

(generate-integer-encoders 32)
(generate-integer-encoders 64)

(defun encode-int (val buffer index)
  "Encodes the signed integer 'val' as a varint into the buffer at the given index.
   Modifies the buffer, and returns the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  ;; Seven bits at a time, least significant bits first
  (loop repeat 9                ;up to 63 bits
        do (setf (aref buffer index) (ldb (byte 7 0) val))
           (setq val (ash val -7))
        until (zerop val)
        do (iincf (aref buffer index) #x80)
           (iincf index)
        finally (unless (zerop val)     ;take the 64th bit as needed
                  (setf (aref buffer index) 1)
                  (unless (= val -1)
                    (serialization-error "Integer too large while encoding VarInt."))))
  (values (iincf index) buffer))        ;return the buffer to improve 'trace'

(defun encode-single (val buffer index)
  "Encodes the single float 'val' into the buffer at the given index.
   Modifies the buffer, and returns the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type single-float val)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (let ((bits (single-float-bits val)))
    (loop repeat 4 doing
      (let ((byte (ldb (byte 8 0) bits)))
        (declare (type (unsigned-byte 8) byte))
        (setq bits (ash bits -8))
        (setf (aref buffer index) byte)
        (iincf index))))
  (values index buffer))

(defun encode-double (val buffer index)
  "Encodes the double float 'val' into the buffer at the given index.
   Modifies the buffer, and returns the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type double-float val)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (multiple-value-bind (low high)
      (double-float-bits val)
    (loop repeat 4 doing
      (let ((byte (ldb (byte 8 0) low)))
        (declare (type (unsigned-byte 8) byte))
        (setq low (ash low -8))
        (setf (aref buffer index) byte)
        (iincf index)))
    (loop repeat 4 doing
      (let ((byte (ldb (byte 8 0) high)))
        (declare (type (unsigned-byte 8) byte))
        (setq high (ash high -8))
        (setf (aref buffer index) byte)
        (iincf index))))
  (values index buffer))

(defun encode-string (string buffer index)
  "Encodes the octets into the buffer at the given index.
   Modifies the buffer, and returns the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (let* ((octets (babel:string-to-octets string :encoding :utf-8))
         (len (length octets))
         (idx (encode-uint32 len buffer index)))
    (declare (type (simple-array (unsigned-byte 8)) octets)
             (type fixnum len)
             (type (unsigned-byte 32) idx))
    (replace buffer octets :start1 idx)
    (values (i+ idx len) buffer)))

(defun encode-octets (octets buffer index)
  "Encodes the octets into the buffer at the given index.
   Modifies the buffer, and returns the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (array (unsigned-byte 8)) octets)
           (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (let* ((len (length octets))
         (idx (encode-uint32 len buffer index)))
    (declare (type fixnum len)
             (type (unsigned-byte 32) idx))
    (replace buffer octets :start1 idx)
    (values (i+ idx len) buffer)))


;;; Wire-level decoders
;;; These are called at the lowest level, so arg types are assumed to be correct

;; Decode the value from the buffer at the given index,
;; then return the value and new index into the buffer
(defmacro generate-integer-decoders (bits)
  "Generate 32- or 64-bit versions of integer decoders."
  (assert (and (plusp bits) (zerop (mod bits 8))))
  (let* ((decode-uint (fintern "~A~A" 'decode-uint bits))
         (decode-fixed  (fintern "~A~A" 'decode-fixed bits))
         (decode-sfixed (fintern "~A~A" 'decode-sfixed bits))
         (bytes (/ bits 8))
         ;; Given bits, can we use fixnums safely?
         (fixnump (<= bits (integer-length most-negative-fixnum)))
         (ldb (if fixnump 'ildb 'ldb))
         (ash (if fixnump 'iash 'ash))
         (decf (if fixnump 'idecf 'decf))
         (logior (if fixnump 'ilogior 'logior)))
    `(progn
       (defun ,decode-uint (buffer index)
         ,(format nil
                  "Decodes the next ~A-bit varint integer in the buffer at the given index.~
                   ~&    Returns both the decoded value and the new index into the buffer.~
                   ~&    Watch out, this function turns off all type checking and array bounds checking." bits)
         (declare #.$optimize-serialization)
         (declare (type (simple-array (unsigned-byte 8)) buffer)
                  (type fixnum index))
         ;; Seven bits at a time, least significant bits first
         (let ((val 0))
           (declare (type (unsigned-byte ,bits) val))
           (loop for places fixnum upfrom 0 by 7
                 for byte fixnum = (prog1 (aref buffer index) (iincf index))
                 do (let ((bits (ildb (byte 7 0) byte)))
                      (declare (type (unsigned-byte 8) bits))
                      (setq val (,logior val (,ash bits places))))
                 until (i< byte 128)
                 finally (progn
                           (unless (< val ,(ash 1 bits))
                             (serialization-error "The value ~D is longer than ~A bits" val ,bits))
                           (return (values val index))))))
       (defun ,decode-fixed (buffer index)
         ,(format nil
                  "Decodes the next ~A-bit unsigned fixed integer in the buffer at the given index.~
                   ~&    Returns both the decoded value and the new index into the buffer.~
                   ~&    Watch out, this function turns off all type checking and array bounds checking." bits)
         (declare #.$optimize-serialization)
         (declare (type (simple-array (unsigned-byte 8)) buffer)
                  (type fixnum index))
         ;; Eight bits at a time, least significant bits first
         (let ((val 0))
           ,@(when fixnump `((declare (type fixnum val))))
           (loop repeat ,bytes
                 for places fixnum upfrom 0 by 8
                 for byte fixnum = (prog1 (aref buffer index) (iincf index))
                 do (setq val (,logior val (,ash byte places))))
           (values val index)))
       (defun ,decode-sfixed (buffer index)
         ,(format nil
                  "Decodes the next ~A-bit signed fixed integer in the buffer at the given index.~
                   ~&    Returns both the decoded value and the new index into the buffer.~
                   ~&    Watch out, this function turns off all type checking and array bounds checking." bits)
         (declare #.$optimize-serialization)
         (declare (type (simple-array (unsigned-byte 8)) buffer)
                  (type fixnum index))
         ;; Eight bits at a time, least significant bits first
         (let ((val 0))
           ,@(when fixnump `((declare (type fixnum val))))
           (loop repeat ,bytes
                 for places fixnum upfrom 0 by 8
                 for byte fixnum = (prog1 (aref buffer index) (iincf index))
                 do (setq val (,logior val (,ash byte places))))
           (when (i= (,ldb (byte 1 ,(1- bits)) val) 1)  ;sign bit set, so negative value
             (,decf val ,(ash 1 bits)))
           (values val index))))))

(generate-integer-decoders 32)
(generate-integer-decoders 64)

(defun decode-int (buffer index)
  "Decodes the next varint integer in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (multiple-value-bind (val index)
      (decode-uint64 buffer index)
    (when (i= (ldb (byte 1 63) val) 1)
      (decf val (ash 1 64)))
    (values val index)))

(defun decode-single (buffer index)
  "Decodes the next single float in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  ;; Eight bits at a time, least significant bits first
  (let ((bits 0))
    (loop repeat 4
          for places fixnum upfrom 0 by 8
          for byte fixnum = (prog1 (aref buffer index) (iincf index))
          do (setq bits (logior bits (ash byte places))))
    (when (i= (ldb (byte 1 31) bits) 1)             ;sign bit set, so negative value
      (decf bits #.(ash 1 32)))
    (values (make-single-float bits) index)))

(defun decode-double (buffer index)
  "Decodes the next double float in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
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
    (when (i= (ldb (byte 1 31) high) 1)             ;sign bit set, so negative value
      (decf high #.(ash 1 32)))
    (values (make-double-float low high) index)))

(defun decode-string (buffer index)
  "Decodes the next UTF-8 encoded string in the buffer at the given index.
   Returns both the decoded string and the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (multiple-value-bind (len idx)
      (decode-uint32 buffer index)
    (declare (type (unsigned-byte 32) len)
             (type fixnum idx))
    (values (babel:octets-to-string buffer :start idx :end (i+ idx len) :encoding :utf-8) (i+ idx len))))

(defun decode-octets (buffer index)
  "Decodes the next octets in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index))
  (multiple-value-bind (len idx)
      (decode-uint32 buffer index)
    (declare (type (unsigned-byte 32) len)
             (type fixnum idx))
    (values (subseq buffer idx (i+ idx len)) (i+ idx len))))


;;; Wire-level lengths
;;; These are called at the lowest level, so arg types are assumed to be correct

(defun varint-length (val)
  "Return the length that 'val' will take when encoded as a varint integer."
  (declare #.$optimize-serialization)
  (loop repeat 10                       ;max length of varint
        do (setq val (ash val -7))
        count 1
        until (zerop val)))

;;; Skipping elements
;;; This is called at the lowest level, so arg types are assumed to be correct

(defun skip-element (buffer index tag)
  "Skip an element in the buffer at the index of the given wire type.
   Returns the new index in the buffer.
   Watch out, this function turns off all type checking and all array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type fixnum index)
           (type (unsigned-byte 32) tag))
  (case (ilogand tag #x7)
    ((#.$wire-type-varint)
     (loop for byte fixnum = (prog1 (aref buffer index) (iincf index))
           until (i< byte 128))
     index)
    ((#.$wire-type-string)
     (multiple-value-bind (len idx)
         (decode-uint32 buffer index)
       (declare (type (unsigned-byte 32) len)
                (type fixnum idx))
       (i+ idx len)))
    ((#.$wire-type-32bit)
     (i+ index 4))
    ((#.$wire-type-64bit)
     (i+ index 8))
    ((#.$wire-type-start-group)
     (loop (multiple-value-bind (new-tag idx)
               (decode-uint32 buffer index)
             (cond ((not (i= (ilogand new-tag #x7) $wire-type-end-group))
                    ;; If it's not the end of a group, skip the next element
                    (setq index (skip-element buffer idx new-tag)))
                   ;; If it's the end of the expected group, we're done
                   ((i= (i- tag $wire-type-start-group) (i- new-tag $wire-type-end-group))
                    (return idx))
                   (t
                    (unless (i= (i- tag $wire-type-start-group) (i- new-tag $wire-type-end-group))
                      (serialization-error "Couldn't find a matching end group tag")))))))
    (t index)))
