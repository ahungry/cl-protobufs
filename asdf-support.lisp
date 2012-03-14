;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Francois-Rene Rideau, Scott McKay               ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-IMPL")


(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass proto-file (asdf:cl-source-file)
  ((asdf::type :initform "proto"))
  (:documentation
   "This ASDF component defines COMPILE-OP and LOAD-OP operations
    that compiles the .proto file into a .lisp file, and the compiles
    the resulting .lisp file into a fasl."))

)       ;eval-when

(defmethod asdf:output-files ((op asdf:compile-op) (c proto-file))
  (append (call-next-method)
          (make-pathname :type "lisp" :defaults (asdf:component-pathname c))))

(defmethod asdf:perform ((op asdf:compile-op) (c proto-file))
  (destructuring-bind (fasl-file lisp-file)
      (asdf:output-files op c)
    (funcall asdf::*compile-op-compile-file-function*
             (parse-protobuf-file (asdf:component-pathname c) lisp-file)
             :output-file fasl-file)))

(defmethod asdf:perform ((op asdf:load-source-op) (c proto-file))
  (destructuring-bind (fasl-file lisp-file)
      (asdf:output-files op c)
    (declare (ignore fasl-file))
    (load (parse-protobuf-file (asdf:component-pathname c) lisp-file))))

(defun parse-protobuf-file (proto-file lisp-file)
  (let ((protobuf (parse-protobuf-from-file proto-file)))
    (with-open-file (stream lisp-file
                     :direction :output
                     :if-exists :replace)
      (write-protobuf protobuf :stream stream :type :lisp)))
  lisp-file)
