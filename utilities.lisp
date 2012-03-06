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


;;; Utilities

(defun proto-class-name (x)
  (remove-if-not #'alphanumericp
                 (camel-case (format nil "~A" x) :separators '(#\- #\_ #\/))))

(defun proto-field-name (x)
  (remove-if-not #'alphanumericp
                 (camel-case-but-one (format nil "~A" x) :separators '(#\- #\_ #\/ #\.))))

(defun proto-enum-name (x &optional prefix)
  (let* ((x (string-upcase (string x)))
         (x (if (and prefix (starts-with x prefix)) (subseq x (length prefix)) x)))
    (remove-if-not #'(lambda (x) (or (alphanumericp x) (eql x #\_)))
                   (format nil "~{~A~^_~}" (split-string x :separators '(#\- #\_ #\/ #\.))))))


#-quux
(progn

(defmacro i+ (&rest fixnums)
  `(the fixnum (+ ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i- (number &rest fixnums)
  `(the fixnum (- (the fixnum ,number) ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i= (&rest fixnums)
  `(= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i< (&rest fixnums)
  `(< ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i> (&rest fixnums)
  `(> ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro iash (value count)
  `(the fixnum (ash (the fixnum ,value) (the fixnum ,count))))

(defmacro ilogior (&rest fixnums)
  (if (cdr fixnums)
    `(the fixnum (logior (the fixnum ,(car fixnums))
                         ,(if (cddr fixnums)
                            `(ilogior ,@(cdr fixnums))
                            `(the fixnum ,(cadr fixnums)))))
    `(the fixnum ,(car fixnums))))

(defmacro ilogand (&rest fixnums)
  (if (cdr fixnums)
    `(the fixnum (logand (the fixnum ,(car fixnums))
                         ,(if (cddr fixnums)
                            `(ilogand ,@(cdr fixnums))
                            `(the fixnum ,(cadr fixnums)))))
    `(the fixnum ,(car fixnums))))

(define-modify-macro iincf (&optional (delta 1)) i+)
(define-modify-macro idecf (&optional (delta 1)) i-)

)       ;#-quux
