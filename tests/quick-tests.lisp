;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;; Based on original work by Robert Brown                           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-TEST")


;;; Bob Brown's functionality unit tests

(defvar *golden-directory*
  #.(make-pathname
     :directory (pathname-directory (or *load-truename* *compile-file-truename*))))

(defvar *golden-pathname* (merge-pathnames "golden.data" *golden-directory*))
(defvar *serial-pathname* (merge-pathnames "serialized.data" *golden-directory*))

(qtest:define-test default-and-clear ()
  ;; Check that required strings are made unbound by 'clear'
  (let ((p (make-instance 'pbtest::test-protocol)))
    (qtest:assert-false (slot-boundp p 'pbtest::zero))
    (setf (pbtest::zero p) "x")
    (qtest:assert-true (string-equal (pbtest::zero p) "x"))
    (proto:clear p)
    (qtest:assert-false (slot-boundp p 'pbtest::zero)))

  ;; Check that optional strings are set to their default value by 'clear'
  (let ((p (make-instance 'pbtest::test-protocol)))
    (qtest:assert-true (string-equal (pbtest::opt-string p) "opt"))
    (setf (pbtest::opt-string p) "x")
    (qtest:assert-true (string-equal (pbtest::opt-string p) "x"))
    (proto:clear p)
    (qtest:assert-true (string-equal (pbtest::opt-string p) "opt"))
    (setf (pbtest::opt-string p) "x")
    (proto:clear p)
    (qtest:assert-true (string-equal (pbtest::opt-string p) "opt"))
    (setf (pbtest::opt-string p) "x")
    (proto:clear-field p 'pbtest::opt-string)
    (qtest:assert-true (string-equal (pbtest::opt-string p) "opt"))))

(qtest:define-test test-pb-write ()
  (let ((p (make-instance 'pbtest::test1proto)))
    ;; Default settings
    (qtest:assert-equal (pbtest::d-int32 p) 12)
    (qtest:assert-true (string-equal (pbtest::d-string p) "foo"))
    (qtest:assert-equal (pbtest::d-bool p) t)

    ;; Test is-initialized
    (qtest:assert-false (pbtest::is-initialized p))
    (setf (pbtest::o-a p) 20)
    (qtest:assert-false (pbtest::is-initialized p))

    ;; Set some unrepeated things
    (setf (pbtest::u-int32 p) 20)
    (setf (pbtest::u-int64 p) -20)
    (setf (pbtest::u-uint64 p) 12345678900)
    (setf (pbtest::u-fixed32 p) 100)
    (setf (pbtest::u-fixed64 p) 12345678900)
    (setf (pbtest::u-bool p) t)
    (setf (pbtest::u-float p) 3.14159f0)
    (setf (pbtest::u-double p) 3.14159265d0)
    (setf (pbtest::u-string p) "foo")
    (setf (pbtest::u-vardata p) "bar")
    (setf (pbtest::u-msg p) (make-instance 'pbtest::test1msg))
    (setf (pbtest::foo (pbtest::u-msg p)) 12)
    (qtest:assert-true (pbtest::is-initialized p))

    ;; Set some repeated things
    (push -30 (pbtest::r-int32 p))
    (push -20 (pbtest::r-int32 p))

    (push 30 (pbtest::r-int64 p))
    (push 20 (pbtest::r-int64 p))

    (push 98765432100 (pbtest::r-uint64 p))
    (push 12345678900 (pbtest::r-uint64 p))

    (push 23456 (pbtest::r-fixed32 p))
    (push 12345 (pbtest::r-fixed32 p))

    (push 98765432100 (pbtest::r-fixed64 p))
    (push 12345678900 (pbtest::r-fixed64 p))

    (push t (pbtest::r-bool p))
    (push nil (pbtest::r-bool p))

    (push -1.75f0 (pbtest::r-float p))
    (push 1.5f0 (pbtest::r-float p))

    (push -1.2d0 (pbtest::r-double p))
    (push 3.3d0 (pbtest::r-double p))

    (push "bar" (pbtest::r-string p))
    (push "foo" (pbtest::r-string p))

    (push "pong" (pbtest::r-vardata p))
    (push "ping" (pbtest::r-vardata p))

    (let ((x (make-instance 'pbtest::test1msg))
          (y (make-instance 'pbtest::test1msg)))
      (setf (pbtest::foo x) 12)
      (setf (pbtest::foo y) 13)
      (push y (pbtest::r-msg p))
      (push x (pbtest::r-msg p)))

    (let ((x (make-instance 'pbtest::test-group1))
          (y (make-instance 'pbtest::test-group2))
          (z (make-instance 'pbtest::test-group2)))
      (setf (pbtest::a x) 80)
      (setf (pbtest::b y) 100)
      (setf (pbtest::b z) 130)
      (push z (pbtest::test-group2 p))
      (push y (pbtest::test-group2 p))
      (push x (pbtest::test-group1 p)))

    ;; int32 tests
    (loop for x in (list (1- (ash 1 31)) (- (ash 1 31)) 1 0 -1)
          do (setf (pbtest::r-int32 p) (append (pbtest::r-int32 p) (list x))))

    ;; int64 tests
    (loop for x in (list (1- (ash 1 63)) (- (ash 1 63)) 1 0 -1)
          do (setf (pbtest::r-int64 p) (append (pbtest::r-int64 p) (list x))))

    ;; fixed32 tests
    (loop for x in (list #xffffffff (1- (ash 1 31)) 0 1)
          do (setf (pbtest::r-fixed32 p) (append (pbtest::r-fixed32 p) (list x))))

    ;; fixed64 tests
    (loop for x in (list #xffffffffffffffff (1- (ash 1 63)) 0 1)
          do (setf (pbtest::r-fixed64 p) (append (pbtest::r-fixed64 p) (list x))))

    ;; uint64 tests
    (loop for x in (list (1- (ash 1 64)) (1- (ash 1 63)) 0 1)
          do (setf (pbtest::r-uint64 p) (append (pbtest::r-uint64 p) (list x))))

    ;; write buffer to a file
    (let ((size (proto:octet-size p)))
      (let* ((buffer (make-byte-vector size))
             (end (proto:serialize p buffer 0 size)))
        (qtest:assert-equal end size)
        (with-open-file (output-stream *serial-pathname*
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
          (write-sequence buffer output-stream)))

      ;; check against the golden data
      (with-open-file (golden-input *golden-pathname*
                       :direction :input
                       :element-type '(unsigned-byte 8))
        (qtest:assert-equal (file-length golden-input) size)
        (with-open-file (test-input *serial-pathname*
                         :direction :input
                         :element-type '(unsigned-byte 8))
          (qtest:assert-equal (file-length test-input) size)
          (let ((golden-buffer (make-byte-vector (file-length test-input)))
                (test-buffer (make-byte-vector size)))
            (read-sequence golden-buffer golden-input)
            (read-sequence test-buffer test-input)
            (qtest:assert-true (equalp golden-buffer test-buffer))
	    (DESCRIBE P)
	    (DESCRIBE (DESERIALIZE-OBJECT (TYPE-OF P) TEST-BUFFER))
	    (DESCRIBE (DESERIALIZE-OBJECT (TYPE-OF P) GOLDEN-BUFFER))))))

    ;; clean up
    (delete-file *serial-pathname*)))

(qtest:define-test test-pb-read ()
  (let ((p (make-instance 'pbtest::Test1-Proto)))
    (with-open-file (golden-input *golden-pathname*
                     :direction :input
                     :element-type '(unsigned-byte 8))
      (let* ((size (file-length golden-input))
             (buffer (make-byte-vector size)))
        (read-sequence buffer golden-input)
        (qtest:assert-equal (proto:merge-from-array p buffer 0 size) size)))

    (flet ((test-repeated (value golden)
            (let ((golden-size (length golden)))
              (qtest:assert-equal (length value) golden-size)
              (loop for v across value
                    for g in golden
                    ;; V and G are either NIL/T, numbers, or strings, actually simple
                    ;; arrays of octets.
                    do (cond ((and (member v '(t nil)) (member g '(t nil)))
                              (qtest:assert-equal v g))
                             ((and (numberp v) (numberp g)) (qtest:assert-equal v g))
                             ((and (arrayp v) (arrayp g)) (qtest:assert-true (equalp v g)))
                             (t (assert (progn "type mismatch" nil))))))))

      ;; unrepeated things
      (qtest:assert-true (proto:has-field p 'pbtest::o-a))
      (qtest:assert-equal (pbtest::o-a p) 20)
      (qtest:assert-false (proto:has-field p 'pbtest::o-b))
      (qtest:assert-equal (pbtest::u-int32 p) 20)
      (qtest:assert-equal (pbtest::u-int64 p) -20)
      (qtest:assert-equal (pbtest::u-uint64 p) 12345678900)
      (qtest:assert-equal (pbtest::u-fixed32 p) 100)
      (qtest:assert-equal (pbtest::u-fixed64 p) 12345678900)
      (qtest:assert-equal (pbtest::u-bool p) t)
      (qtest:assert-equal (pbtest::u-float p) 3.14159f0)
      (qtest:assert-equal (pbtest::u-double p) 3.14159265d0)

      ;; Lisp implementation omits "has" function for embedded messages.
      ;;(qtest:assert (has-u-msg p))
      (qtest:assert-equal (pbtest::foo (pbtest::u-msg p)) 12)

      ;; repeated things
      (test-repeated (pbtest::r-int32 p)
                     (list -20 -30 (1- (ash 1 31)) (- (ash 1 31)) 1 0 -1))
      (test-repeated (pbtest::r-int64 p)
                     (list 20 30 (1- (ash 1 63)) (- (ash 1 63)) 1 0 -1))
      (test-repeated (pbtest::r-uint64 p)
                     (list 12345678900 98765432100
                           (1- (ash 1 64)) (1- (ash 1 63))
                           0 1))
      (test-repeated (pbtest::r-fixed32 p)
                     (list 12345 23456 #xffffffff (1- (ash 1 31)) 0 1))
      (test-repeated (pbtest::r-fixed64 p)
                     (list 12345678900 98765432100 #xffffffffffffffff
                           (1- (ash 1 63)) 0 1))
      (test-repeated (pbtest::r-bool p) '(nil t))
      (test-repeated (pbtest::r-float p) '(1.5f0 -1.75f0))
      (test-repeated (pbtest::r-double p) '(3.3d0 -1.2d0))
      (test-repeated (pbtest::r-string p) (list "foo" "bar"))
      (test-repeated (pbtest::r-vardata p) (list "ping" "pong"))

      (qtest:assert-equal (length (pbtest::r-msg p)) 2)
      (qtest:assert-equal (pbtest::foo (aref (pbtest::r-msg p) 0)) 12)
      (qtest:assert-equal (pbtest::foo (aref (pbtest::r-msg p) 1)) 13)

      ;; groups
      (qtest:assert-equal (length (pbtest::test-group1 p)) 1)
      (qtest:assert-equal (pbtest::a (aref (pbtest::test-group1 p) 0)) 80)

      (qtest:assert-equal (length (pbtest::test-group2 p)) 2)
      (qtest:assert-equal (pbtest::b (aref (pbtest::test-group2 p) 0)) 100)
      (qtest:assert-equal (pbtest::b (aref (pbtest::test-group2 p) 1)) 130)

      ;; default settings
      (qtest:assert-equal (pbtest::d-int32 p) 12)
      (qtest:assert-true (string-equal (pbtest::d-string p) "foo"))
      (qtest:assert-equal (pbtest::d-bool p) t))))

(defun parser-timing (iterations)
  (let ((src (make-instance 'pbtest::Time-Protocol)))
    (dotimes (i 1000)
      (let ((new (make-instance 'pbtest::Time-Protocol-G)))
        (setf (pbtest::v1 new) 100)
        (setf (pbtest::v2 new) 80)
        (PUSH new (pbtest::g src))))

    (let* ((buffer (make-byte-vector 10000))
           ;; XXXXXXXXXX
           (size (proto:serialize src buffer 0 10000)))
      (time (dotimes (i iterations)
              (let ((msg (make-instance 'pbtest::TimeProtocol)))
                (proto:merge-from-array msg buffer 0 size)))))))
