;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-TEST")


;;; Ultra light-weight test framework

(defmacro define-test (test-name () &body body)
  `(defun ,test-name ()
     (handler-case
         (progn ,@body)
       (error (e)
         (warn "An error was signalled executing ~S: ~A"
               ',test-name e)))))

(defmacro define-test-suite (suite-name () &body body)
  (if (listp (car body))
    ;; QRes-style body
    `(defun ,suite-name ()
       ,@(loop for test in (car body)
               collect (list test)))
    ;; The more sensible style
    `(defun ,suite-name ()
       ,@(loop for test in body
               collect (list test)))))

(defvar *all-registered-tests* ())
(defmacro register-test (test-name)
  `(pushnew ,test-name *all-registered-tests*))

(defmacro run-test (test-name)
  `(progn
     (format t "~&Running test ~A" ',test-name)
     (funcall ',test-name)))

(defun run-all-tests ()
  (dolist (test *all-registered-tests*)
    (format t "~&Running test ~A" test)
    (funcall test)))

(defmacro assert-equal (actual expected &key (test '#'equal))
  `(unless (funcall ,test ,actual ,expected)
     (warn "The value of ~S (~S) is not equal to the expected value ~S"
           ',actual ,actual ,expected)))

(defmacro assert-true (form)
  `(unless ,form
     (warn "The value of ~S (~S) does not evaluate to 'true'"
           ',form ,form)))

(defmacro assert-false (form)
  `(when ,form
     (warn "The value ~S (~S) does not evaluate to 'false'"
           ',form ,form)))

(defmacro assert-error (condition &body body)
  "Checks if BODY signals a condition of class CONDITION. If it does not, a failure is
   reported. If it is, the condition is caught and the condition object returned so that the test
   can perform further checks on the condition object."
  (let ((c (gensym "C")))
    `(handler-case (progn ,@body)
       (,condition (,c)
         ,c)
       (:no-error ()
         (warn "Expected condition ~a while evaluating~{ ~s~}" ',condition ',body)))))
