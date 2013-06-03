;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;; Based on original work by Robert Brown                           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-TEST")


;;; Varint unit tests

(define-test varint-length-test ()
  (assert-equal (varint-length 0) 1)
  (assert-equal (varint-length 1) 1)
  (assert-equal (varint-length 127) 1)
  (assert-equal (varint-length 128) 2)
  (assert-equal (varint-length 16383) 2)
  (assert-equal (varint-length 16384) 3)
  (assert-equal (varint-length (ash 1 31)) 5)
  (assert-equal (varint-length (- (ash 1 21) 1)) 3)
  (assert-equal (varint-length (ash 1 21)) 4)
  (assert-equal (varint-length (ash 1 63)) 10))


(define-test uint32-test ()
  (let* ((val #xE499867)
         (encoding #(#xE7 #xB0 #xA6 #x72))
         (len (length encoding))
         (buf (make-byte-vector len)))
    (let ((idx (encode-uint32 val buf 0)))
      (assert-equal idx len)
      (assert-true (equalp buf encoding))
      (multiple-value-bind (nval nidx)
          (decode-uint32 buf 0)
        (assert-equal nidx len)
        (assert-equal nval val)))))

(define-test uint64-test ()
  (let* ((val #xE4998679470D98D)
         (encoding #(#x8D #xB3 #xC3 #xA3 #xF9 #x8C #xE6 #xA4 #x0E))
         (len (length encoding))
         (buf (make-byte-vector len)))
    (let ((idx (encode-uint64 val buf 0)))
      (assert-equal idx len)
      (assert-true (equalp buf encoding))
      (multiple-value-bind (nval nidx)
          (decode-uint64 buf 0)
        (assert-equal nidx len)
        (assert-equal nval val)))))


(defvar $max-bytes-32  5)
(defvar $max-bytes-64 10)

(define-test powers-varint-test ()
  (let ((buffer (make-byte-vector (* 128 $max-bytes-64)))
        (index 0)
        length)
    ;; Encode powers of 2
    ;; Smaller powers are encoded as both 32-bit and 64-bit varints
    (dotimes (p 64)
      (when (< p 32)
        (setq index (encode-uint32 (ash 1 p) buffer index)))
      (setq index (encode-uint64 (ash 1 p) buffer index)))
    (setq length index)

    ;; Test the decodings
    (setq index 0)
    (dotimes (p 64)
      (when (< p 32)
        (multiple-value-bind (val idx)
            (decode-uint32 buffer index)
          (assert-equal val (ash 1 p))
          (setq index idx)))
      (multiple-value-bind (val idx)
          (decode-uint64 buffer index)
        (assert-equal val (ash 1 p))
        (setq index idx)))
    (assert-equal index length)

    ;; Test skipping, too
    (setq index 0)
    (dotimes (p 64)
      (when (< p 32)
        (setq index (skip-element buffer index $wire-type-varint)))
      (setq index (skip-element buffer index $wire-type-varint)))
    (assert-equal index length)))

(define-test random-varint-test ()
  ;; Encode 1000 random numbers as both 32-bit and 64-bit varints
  (let* ((count 1000)
         (buf32 (make-byte-vector (* count $max-bytes-32)))
         (buf64 (make-byte-vector (* count $max-bytes-64)))
         (vals32 (make-array count))
         (vals64 (make-array count))
         (index32 0)
         (index64 0))
    (dotimes (i count)
      (let* ((val64 (random (ash 1 64)))
             (val32 (ldb (byte 32 0) val64)))
        (setf (aref vals32 i) val32)
        (setf (aref vals64 i) val64)
        (setq index32 (encode-uint32 val32 buf32 index32))
        (setq index64 (encode-uint64 val64 buf64 index64))))

    ;; Test the decodings
    (setq index32 0)
    (setq index64 0)
    (dotimes (i count)
      (multiple-value-bind (val32 idx)
          (decode-uint32 buf32 index32)
        (assert-equal val32 (aref vals32 i))
        (setq index32 idx))
      (multiple-value-bind (val64 idx)
          (decode-uint64 buf64 index64)
        (assert-equal val64 (aref vals64 i))
        (setq index64 idx)))))


(define-test-suite varint-tests ()
  (varint-length-test
   uint32-test
   uint64-test
   powers-varint-test
   random-varint-test))

(register-test 'varint-tests)
