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

(defun class-name->proto (x)
  "Given a Lisp class name, returns a Protobufs message or enum name."
  (remove-if-not #'alphanumericp
                 (camel-case (format nil "~A" x) :separators '(#\- #\_ #\/))))

(defun enum-name->proto (x &optional prefix)
  "Given a Lisp enum value name, returns a Protobufs enum value name."
  (let* ((x (string-upcase (string x)))
         (x (if (and prefix (starts-with x prefix)) (subseq x (length prefix)) x)))
    (remove-if-not #'(lambda (x) (or (alphanumericp x) (eql x #\_)))
                   (format nil "~{~A~^_~}" (split-string x :separators '(#\- #\_ #\/ #\.))))))

(defun slot-name->proto (x)
  "Given a Lisp slot name, returns a Protobufs field name."
  (remove-if-not #'alphanumericp
                 (camel-case-but-one (format nil "~A" x) :separators '(#\- #\_ #\/ #\.))))


(defun proto->class-name (x &optional package)
  "Given a Protobufs message or enum type name, returns a Lisp class or type name."
  (let ((name (string-upcase (uncamel-case x))))
    (if package (intern name package) (make-symbol name))))

(defun proto->enum-name (x &optional package)
  "Given a Protobufs enum value name, returns a Lisp enum value name."
  (let ((name (format nil "~{~A~^-~}" (split-string (string-upcase x) :separators '(#\_)))))
    (if package (intern name package) (make-symbol name))))

(defun proto->slot-name (x &optional package)
  "Given a Protobufs field value name, returns a Lisp slot name."
  (let ((name (string-upcase (uncamel-case x))))
    (if package (intern name package) (make-symbol name))))


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
