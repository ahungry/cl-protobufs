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
                     :if-exists :supersede)
      (write-protobuf protobuf :stream stream :type :lisp)))
  lisp-file)


;; Process 'import' lines
(defun process-imports (&rest imports)
  "Imports all of the files given by 'imports'.
   If the file is a .proto file, it first parses it and writes a .lisp file.
   The .lisp file is the compiled and loaded."
  (dolist (import imports)
    (let* ((base-file  (pathname import))
           (proto-file (make-pathname :type "proto" :defaults base-file))
           (lisp-file  (make-pathname :name (pathname-name base-file) :type "lisp"
                                      :defaults (or *compile-file-pathname* base-file)))
           (fasl-file  (compile-file-pathname lisp-file))
           (proto-date (and (probe-file proto-file)
                            (ignore-errors (file-write-date proto-file))))
           (lisp-date  (and (probe-file lisp-file)
                            (ignore-errors (file-write-date lisp-file))))
           (fasl-date  (and (probe-file fasl-file)
                            (ignore-errors (file-write-date fasl-file)))))
      (when (string= (pathname-type base-file) "proto")
        ;; The user asked to import a .proto file
        ;; If there's no .lisp file or an older .lisp file, parse the .proto file now
        (cond ((not proto-date)
               (warn "Could not find the file to be imported ~A" proto-file))
              ((or (not lisp-date)
                   (< lisp-date proto-date))
               (parse-protobuf-file proto-file lisp-file)
               (setq lisp-date (file-write-date lisp-file)))))
      ;; Compile the .lisp file, if necessary
      (when (or (not fasl-date)
                (< fasl-date lisp-date))
        (setq fasl-file (compile-file lisp-file))
        (setq fasl-date (file-write-date fasl-file)))
      ;; Now we can load the .fasl file
      (load fasl-file))))
