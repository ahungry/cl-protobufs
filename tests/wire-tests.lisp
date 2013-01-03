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


;;; Wire format unit tests

(define-test zig-zag-test ()
  (flet ((verify (encoder pairs)
           (loop for (input output) in pairs
                 do (assert-true (= (funcall encoder input) output)))))
    (verify #'zig-zag-encode32
            `((0 0) (-1 1) (1 2) (-2 3)
              (#x3fffffff #x7ffffffe)
              (,(- #xc0000000 (ash 1 32)) #x7fffffff)
              (#x7fffffff #xfffffffe)
              (,(- #x80000000 (ash 1 32)) #xffffffff)))
    (verify #'zig-zag-decode32
            `((0 0) (1 -1) (2 1) (3 -2)
              (#x7ffffffe #x3fffffff)
              (#x7fffffff ,(- #xc0000000 (ash 1 32)))
              (#xfffffffe #x7fffffff)
              (#xffffffff ,(- #x80000000 (ash 1 32)))))
    (verify #'zig-zag-encode64
            `((0 0) (-1 1) (1 2) (-2 3)
              (#x000000003fffffff #x000000007ffffffe)
              (,(- #xffffffffc0000000 (ash 1 64)) #x000000007fffffff)
              (#x000000007fffffff #x00000000fffffffe)
              (,(- #xffffffff80000000 (ash 1 64)) #x00000000ffffffff)
              (#x7fffffffffffffff #xfffffffffffffffe)
              (,(- #x8000000000000000 (ash 1 64)) #xffffffffffffffff)))
    (verify #'zig-zag-decode64
            `((0 0) (1 -1) (2 1) (3 -2)
              (#x000000007ffffffe #x000000003fffffff)
              (#x000000007fffffff ,(- #xffffffffc0000000 (ash 1 64)))
              (#x00000000fffffffe #x000000007fffffff)
              (#x00000000ffffffff ,(- #xffffffff80000000 (ash 1 64)))
              (#xfffffffffffffffe #x7fffffffffffffff)
              (#xffffffffffffffff ,(- #x8000000000000000 (ash 1 64))))))
  (flet ((round-trip32 (n)
           (assert-true (= n (zig-zag-decode32 (zig-zag-encode32 n)))))
         (round-trip64 (n)
           (assert-true (= n (zig-zag-decode64 (zig-zag-encode64 n))))))
    (dolist (n '(0 1 -1 14927 -3612))
      (round-trip32 n))
    (dolist (n '(0 1 -1 14927 -3612 856912304801416 -75123905439571256))
      (round-trip64 n))))

(define-test encode-length-tests ()
  (flet ((verify (encoder pairs)
           (loop for (input output) in pairs
                 do (assert-true (= (funcall encoder input) output)))))
    (verify #'length32
            '((#x0 1) (#x7f 1)                ; 0-7 bits
              (#x80 2) (#x3fff 2)             ; 8-14 bits
              (#x4000 3) (#x1fffff 3)         ; 15-21 bits
              (#x200000 4) (#xfffffff 4)      ; 22-28 bits
              (#x10000000 5) (#xffffffff 5))) ; 29-35 bits, though we'll actually stop at 32 bits.
    (verify #'length64
            '((#x0 1) (#x7f 1)                                    ; 0-7 bits
              (#x80 2) (#x3fff 2)                                 ; 8-14 bits
              (#x4000 3) (#x1fffff 3)                             ; 15-21 bits
              (#x200000 4) (#xfffffff 4)                          ; 22-28 bits
              (#x10000000 5) (#x7ffffffff 5)                      ; 29-35 bits
              (#x800000000 6) (#x3ffffffffff 6)                   ; 36-42 bits
              (#x40000000000 7) (#x1ffffffffffff 7)               ; 29-49 bits
              (#x2000000000000 8) (#xffffffffffffff 8)            ; 29-54 bits
              (#x100000000000000 9) (#x7fffffffffffffff 9)        ; 29-63 bits
              (#x8000000000000000 10) (#xffffffffffffffff 10))))) ; 64-72 bits, though we'll actually stop at 64 bits.

(define-test encode/decode-ints-tests ()
  (flet ((verify (encoder decoder pairs)
           (loop for (input expected) in pairs
                 for expected-buf = (coerce expected '(vector (unsigned-byte 8)))
                 for filler = (random #xff)
                 when encoder
                   do (let* ((buf (make-array '(16) :element-type '(unsigned-byte 8) :initial-element filler))
                             (index (funcall encoder input buf 0)))
                        ;; Are the bytes as expected?
                        (assert-true (equalp (subseq buf 0 index) expected-buf))
                        ;; Is the rest of the buffer untouched?
                        (assert-true (every #'(lambda (x) (= x filler)) (subseq buf index))))
                 when decoder
                   do (multiple-value-bind (decoded-value index)
                          (funcall decoder expected-buf 0)
                        ;; Did we get the right value?
                        (assert-true (= decoded-value input))
                        ;; Did we get the right index increment?
                        (assert-true (= (length expected) index))))))
    (verify #'encode-uint32
            #'decode-uint32
            '((#x0 (#x00))
              (#x1 (#x01))
              (#x7f (#x7f))
              (#x80 (#x80 #x01))
              (#x3fff (#xff #x7f))
              (#x4000 (#x80 #x80 #x01))
              (#x1fffff (#xff #xff #x7f))
              (#x200000 (#x80 #x80 #x80 #x01))
              (#xfffffff (#xff #xff #xff #x7f))
              (#x10000000 (#x80 #x80 #x80 #x80 #x01))
              (#xffffffff (#xff #xff #xff #xff #x0f))))
    (verify nil
            #'decode-int32
            '((#x0 (#x00))
              (#x1 (#x01))
              (#x7fffffff (#xff #xff #xff #xff #x07))
              (#x-80000000 (#x80 #x80 #x80 #x80 #x08))
              (#x-1 (#xff #xff #xff #xff #x0f))))
    (verify #'encode-fixed32
            #'decode-fixed32
            '((#x0 (#x00 #x00 #x00 #x00))
              (#x1 (#x01 #x00 #x00 #x00))
              (#xff (#xff #x00 #x00 #x00))
              (#x100 (#x00 #x01 #x00 #x00))
              (#xffff (#xff #xff #x00 #x00))
              (#x10000 (#x00 #x00 #x01 #x00))
              (#xffffff (#xff #xff #xff #x00))
              (#x1000000 (#x00 #x00 #x00 #x01))
              (#xffffffff (#xff #xff #xff #xff))))
    (verify #'encode-sfixed32
            #'decode-sfixed32
            '((#x0 (#x00 #x00 #x00 #x00))
              (#x1 (#x01 #x00 #x00 #x00))
              (#x-1 (#xff #xff #xff #xff))
              (#xff (#xff #x00 #x00 #x00))
              (#x-ff (#x01 #xff #xff #xff))
              (#x100 (#x00 #x01 #x00 #x00))
              (#x-100 (#x00 #xff #xff #xff))
              (#xffff (#xff #xff #x00 #x00))
              (#x-ffff (#x01 #x00 #xff #xff))
              (#x10000 (#x00 #x00 #x01 #x00))
              (#x-10000 (#x00 #x00 #xff #xff))
              (#xffffff (#xff #xff #xff #x00))
              (#x-ffffff (#x01 #x00 #x00 #xff))
              (#x1000000 (#x00 #x00 #x00 #x01))
              (#x-1000000 (#x00 #x00 #x00 #xff))
              (#x7fffffff (#xff #xff #xff #x7f))
              (#x-80000000 (#x00 #x00 #x00 #x80))))
    (verify #'encode-uint64
            #'decode-uint64
            '((#x0 (#x00))
              (#x1 (#x01))
              (#x7f (#x7f))
              (#x80 (#x80 #x01))
              (#x3fff (#xff #x7f))
              (#x4000 (#x80 #x80 #x01))
              (#x1fffff (#xff #xff #x7f))
              (#x200000 (#x80 #x80 #x80 #x01))
              (#xfffffff (#xff #xff #xff #x7f))
              (#x10000000 (#x80 #x80 #x80 #x80 #x01))
              (#x7ffffffff (#xff #xff #xff #xff #x7f))
              (#x800000000 (#x80 #x80 #x80 #x80 #x80 #x01))
              (#x3ffffffffff (#xff #xff #xff #xff #xff #x7f))
              (#x40000000000 (#x80 #x80 #x80 #x80 #x80 #x80 #x01))
              (#x1ffffffffffff (#xff #xff #xff #xff #xff #xff #x7f))
              (#x2000000000000 (#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x01))
              (#xffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #x7f))
              (#x100000000000000 (#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x01))
              (#x7fffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #xff #x7f))
              (#x8000000000000000 (#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x01))
              (#xffffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #xff #xff #x01))))
    (verify nil
            #'decode-int64
            '((#x0 (#x00))
              (#x1 (#x01))
              (#x7fffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #xff #xff #x00))
              (#x-8000000000000000 (#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x01))
              (#x-1 (#xff #xff #xff #xff #xff #xff #xff #xff #xff #x01))))
    (verify #'encode-fixed64
            #'decode-fixed64
            '((#x0 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
              (#x1 (#x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
              (#xff (#xff #x00 #x00 #x00 #x00 #x00 #x00 #x00))
              (#x100 (#x00 #x01 #x00 #x00 #x00 #x00 #x00 #x00))
              (#xffff (#xff #xff #x00 #x00 #x00 #x00 #x00 #x00))
              (#x10000 (#x00 #x00 #x01 #x00 #x00 #x00 #x00 #x00))
              (#xffffff (#xff #xff #xff #x00 #x00 #x00 #x00 #x00))
              (#x1000000 (#x00 #x00 #x00 #x01 #x00 #x00 #x00 #x00))
              (#xffffffff (#xff #xff #xff #xff #x00 #x00 #x00 #x00))
              (#x100000000 (#x00 #x00 #x00 #x00 #x01 #x00 #x00 #x00))
              (#xffffffffff (#xff #xff #xff #xff #xff #x00 #x00 #x00))
              (#x10000000000 (#x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00))
              (#xffffffffffff (#xff #xff #xff #xff #xff #xff #x00 #x00))
              (#x1000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x01 #x00))
              (#xffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #x00))
              (#x100000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01))
              (#xffffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #xff))))
    (verify #'encode-sfixed64
            #'decode-sfixed64
            '((#x0 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
              (#x1 (#x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
              (#x-1 (#xff #xff #xff #xff #xff #xff #xff #xff))
              (#xff (#xff #x00 #x00 #x00 #x00 #x00 #x00 #x00))
              (#x-ff (#x01 #xff #xff #xff #xff #xff #xff #xff))
              (#x100 (#x00 #x01 #x00 #x00 #x00 #x00 #x00 #x00))
              (#x-100 (#x00 #xff #xff #xff #xff #xff #xff #xff))
              (#xffff (#xff #xff #x00 #x00 #x00 #x00 #x00 #x00))
              (#x-ffff (#x01 #x00 #xff #xff #xff #xff #xff #xff))
              (#x10000 (#x00 #x00 #x01 #x00 #x00 #x00 #x00 #x00))
              (#x-10000 (#x00 #x00 #xff #xff #xff #xff #xff #xff))
              (#xffffff (#xff #xff #xff #x00 #x00 #x00 #x00 #x00))
              (#x-ffffff (#x01 #x00 #x00 #xff #xff #xff #xff #xff))
              (#x1000000 (#x00 #x00 #x00 #x01 #x00 #x00 #x00 #x00))
              (#x-1000000 (#x00 #x00 #x00 #xff #xff #xff #xff #xff))
              (#x7fffffff (#xff #xff #xff #x7f #x00 #x00 #x00 #x00))
              (#x-ffffffff (#x01 #x00 #x00 #x00 #xff #xff #xff #xff))
              (#x100000000 (#x00 #x00 #x00 #x00 #x01 #x00 #x00 #x00))
              (#x-100000000 (#x00 #x00 #x00 #x00 #xff #xff #xff #xff))
              (#x7fffffffff (#xff #xff #xff #xff #x7f #x00 #x00 #x00))
              (#x-ffffffffff (#x01 #x00 #x00 #x00 #x00 #xff #xff #xff))
              (#x10000000000 (#x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00))
              (#x-10000000000 (#x00 #x00 #x00 #x00 #x00 #xff #xff #xff))
              (#x7fffffffffff (#xff #xff #xff #xff #xff #x7f #x00 #x00))
              (#x-ffffffffffff (#x01 #x00 #x00 #x00 #x00 #x00 #xff #xff))
              (#x1000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x01 #x00))
              (#x-1000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #xff #xff))
              (#x7fffffffffffff (#xff #xff #xff #xff #xff #xff #x7f #x00))
              (#x-ffffffffffffff (#x01 #x00 #x00 #x00 #x00 #x00 #x00 #xff))
              (#x100000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01))
              (#x-100000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff))
              (#x7fffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #x7f))
              (#x-8000000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))))))

;;--- We need more tests:
;;---  (de)serialize-prim, (de)serialize-packed, (de)serialize-enum
;;---  prim-size, packed-size, enum-size
;;---  encode/decode-single/double

(define-test-suite wire-tests ()
  (zig-zag-test)
  (encode-length-tests)
  (encode/decode-ints-tests))

(register-test 'wire-tests)
