;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;; Based on work by: Robert Brown, Francois-Rene Rideau             ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "ASDF")


;;; ASDF support for CL-Protobufs

(defclass protobuf-file (cl-source-file)
  ((type :initform "proto")             ;default file type
   ;; If non-nil, use this relative pathname
   (proto-pathname :accessor proto-relative-pathname
                   :initform nil
                   :initarg :proto-pathname
                   :documentation "Relative pathname giving the location of the .proto file")
   ;; A search path to try when looking for system-provided .proto files
   (search-path :accessor proto-search-path
                :initform ()
                :initarg :search-path
                :documentation
                "List of directories where the protocol buffer compiler should search
                 for imported protobuf files.  Relative pathnames are treated as relative
                 to the directory containing the DEFSYSTEM form in which they appear.")
   (conc-name :accessor proto-conc-name
              :initform ""
              :initarg :conc-name))
  (:documentation
   "This ASDF component defines PROTO-TO-LISP, COMPILE-OP and LOAD-OP
    operations that compile the .proto file into a .lisp file. The .lisp
    file is then compiled, and possibly loaded, as well."))

(defclass proto-to-lisp (compile-op) ()
  (:documentation
   "The ASDF operation that compiles a .proto file containing Protocol Buffers
    definitions into a .lisp source file."))

(defmethod component-depends-on ((op compile-op) (component protobuf-file))
  "Compiling a protocol buffer file depends on generating Lisp source code for it."
  (if (typep op 'proto-to-lisp)
    (call-next-method)
    `((proto-to-lisp ,(component-name component))
      ,@(call-next-method))))

(defmethod component-depends-on ((op load-op) (component protobuf-file))
  "Loading a protocol buffer file depends on generating Lisp source code for it."
  `((proto-to-lisp ,(component-name component))
    ,@(call-next-method)))

(defmethod component-self-dependencies :around ((op load-op) (component protobuf-file))
  (remove-if #'(lambda (x)
                 (eq (car x) 'proto-to-lisp))
             (call-next-method)))

(defun protobuf-input-file (component)
  "Returns the pathname of the protocol buffer definition file that must be
   translated into Lisp source code for this PROTO-FILE component."
  (check-type component protobuf-file)
  (if (proto-relative-pathname component)
    ;; Path was specified with ':proto-pathname'
    (subpathname (component-pathname (component-parent component))
                 (proto-relative-pathname component)
                 :type "proto")
    ;; No ':proto-pathname', the path of the protobuf file
    ;; defaults to the component-pathname, with its automatic type "proto"
    (component-pathname component)))

(defun resolve-search-path (component)
  (check-type component protobuf-file)
  (let* ((search-path (proto-search-path component))
         (parent-path (component-pathname (component-parent component))))
    (mapcar #'(lambda (path)
                (resolve-relative-pathname path parent-path))
            search-path)))

(defun resolve-relative-pathname (path parent-path)
  "When 'path' doesn't have an absolute directory component,
   treat it as relative to 'parent-path'."
  (pathname-directory-pathname
   (merge-pathnames* path parent-path)))

(defmethod input-files ((op proto-to-lisp) (component protobuf-file))
  "The input file is just the .proto file."
  (declare (ignorable op))
  (list (protobuf-input-file component)))

(defmethod output-files ((op proto-to-lisp) (component protobuf-file))
  "The output file is a .lisp file and a .proto-imports file with dependency data,
   stored where .fasl files are stored"
  (declare (ignorable op))
  (let ((lisp-file (lispize-pathname (component-pathname component))))
    (values (list lisp-file
                  (make-pathname :type "proto-imports"
                                 :defaults lisp-file))
            nil)))

(defmethod input-files ((op compile-op) (component protobuf-file))
  "The input files are the .lisp and .proto-imports files."
  (declare (ignorable op))
  (output-files (make-instance 'proto-to-lisp) component))

(defmethod perform ((op proto-to-lisp) (component protobuf-file))
  (let* ((input  (protobuf-input-file component))
         (output (first (output-files op component)))
         (paths  (cons (directory-namestring input) (resolve-search-path component)))
         (proto-impl:*protobuf-search-path* paths)
         (proto-impl:*protobuf-output-path* output))
    (dolist (path paths (error 'compile-failed
                          :component component :operation op))
      (let ((proto (make-pathname :type "proto" :defaults (merge-pathnames* path (pathname input)))))
        (destructuring-bind (lisp imports)
            (output-files op component)
          (when (probe-file proto)
            (return-from perform
              (proto-impl:parse-protobuf-file proto lisp imports
                                              :conc-name (proto-conc-name component)))))))))

(defmethod operation-description ((op proto-to-lisp) (component protobuf-file))
  (format nil (compatfmt "~@<proto-compiling ~3i~_~A~@:>")
          (first (input-files op component))))

(defmethod input-files ((op load-op) (component protobuf-file))
  "The input files are the .fasl and .proto-imports files."
  (declare (ignorable op))
  (append (output-files (make-instance 'compile-op) component) ; fasl
          (cdr (output-files (make-instance 'proto-to-lisp) component)))) ; proto-imports

(defmethod perform ((op compile-op) (component protobuf-file))
  (let* ((input  (protobuf-input-file component))
         (output (output-file op component))
         (lisp   (first (input-files op component)))
         (fasl   output)
         (paths  (cons (directory-namestring input) (resolve-search-path component)))
         (proto-impl:*protobuf-search-path* paths)
         (proto-impl:*protobuf-output-path* output)
         (*compile-file-warnings-behaviour* (operation-on-warnings op))
         (*compile-file-failure-behaviour* (operation-on-failure op)))
    (proto-impl:process-imports-from-file
     (make-pathname :type "proto-imports"
                    :defaults output))
    (multiple-value-bind (output warnings-p failure-p)
        (apply #'compile-file* lisp
               :output-file fasl
               (compile-op-flags op))
      (when warnings-p
        (case (operation-on-warnings op)
          (:warn  (warn "~@<COMPILE-FILE warned while performing ~A on ~A.~@:>" op component))
          (:error (error 'compile-warned
                         :component component :operation op))
          (:ignore nil)))
      (when failure-p
        (case (operation-on-failure op)
          (:warn  (warn "~@<COMPILE-FILE failed while performing ~A on ~A.~@:>" op component))
          (:error (error 'compile-failed
                         :component component :operation op))
          (:ignore nil)))
      (unless output
        (error 'compile-error
               :component component :operation op)))))

(defmethod perform ((op load-op) (component protobuf-file))
  (let* ((input  (protobuf-input-file component))
         (paths  (cons (directory-namestring input) (resolve-search-path component)))
         (proto-impl:*protobuf-search-path* paths)
         (proto-impl:*protobuf-output-path* (first (input-files op component))))
    (proto-impl:process-imports-from-file
     (make-pathname :type "proto-imports"
                    :defaults (first (input-files op component)))))
  (call-next-method))

(defmethod operation-description ((op compile-op) (component protobuf-file))
  (format nil (compatfmt "~@<compiling ~3i~_~A~@:>")
          (first (input-files op component))))


;;; Processing of imports

(in-package "PROTO-IMPL")

(defun parse-protobuf-file (protobuf-file lisp-file imports-file &key (conc-name ""))
  (let ((schema (parse-schema-from-file protobuf-file :conc-name conc-name)))
    (with-open-file (stream lisp-file
                     :direction :output
                     :if-exists :supersede
                     :external-format :utf-8
                     :element-type 'character)
      (write-schema schema :stream stream :type :lisp))
    (with-open-file (stream imports-file
                     :direction :output
                     :if-exists :supersede
                     :external-format :utf-8
                     :element-type 'character)
      (with-standard-io-syntax
        (format stream "~W~%" (proto-imports schema)))))
  lisp-file)

;; Process 'import' lines
(defun process-imports (schema imports)
  "Imports all of the files given by 'imports'.
   If the file is a .proto file, it first parses it and writes a .lisp file.
   The .lisp file is the compiled and loaded."
  (dolist (import imports)
    (block import-one
      (let* ((import      (pathname import))
             (import-name (pathname-name import))
             (imported    (find-schema (class-name->proto import-name))))
        ;; If this schema has already been imported somewhere else,
        ;; mark it as imported here and carry on
        (when imported
          (setf (proto-imported-schemas schema)
                (nconc (proto-imported-schemas schema) (list imported)))
          (return-from import-one))
        (%process-import import import-name)
        (let* ((imported (find-schema (class-name->proto import-name))))
          (when imported
            (setf (proto-imported-schemas schema)
                  (nconc (proto-imported-schemas schema) (list imported))))
          (return-from import-one))))))

(defun process-imports-from-file (imports-file)
  (when (probe-file imports-file)
    (let ((imports (with-open-file (stream imports-file
                                    :direction :input
                                    :external-format :utf-8
                                    :element-type 'character)
                     (with-standard-io-syntax (read stream)))))
      (dolist (import imports)
        (let* ((import      (pathname import))
               (import-name (pathname-name import)))
          ;; If this schema has already been loaded, we're done.
          (unless (find-schema (class-name->proto import-name))
            (%process-import import import-name)))))))

(defun %process-import (import import-name
                        &key (search-path *protobuf-search-path*)
                             (output-path *protobuf-output-path*))
  (dolist (path search-path (error "Could not import ~S" import))
    (let* ((base-path  (asdf::merge-pathnames* import path))
           (proto-file (make-pathname :name import-name :type "proto"
                                      :defaults base-path))
           (lisp-file  (asdf::lispize-pathname
                        (if output-path
                            (make-pathname :name import-name
                                           :directory (pathname-directory output-path))
                            base-path)))
           (imports-file (make-pathname :type "proto-imports"
                                        :defaults lisp-file))
           (fasl-file  (compile-file-pathname lisp-file))
           (asdf:*asdf-verbose* nil)    ;for safe-file-write-date
           (proto-date (asdf::safe-file-write-date proto-file))
           (lisp-date  (asdf::safe-file-write-date lisp-file))
           (fasl-date  (asdf::safe-file-write-date fasl-file))
           (imports-date  (asdf::safe-file-write-date imports-file)))
      (when (probe-file proto-file)
        (let ((*protobuf-pathname* proto-file))
          (when (string= (pathname-type base-path) "proto")
            ;; The user asked to import a .proto file
            ;; If there's no .lisp file or an older .lisp file, or no
            ;; .proto-imports file or an older .proto-imports file parse
            ;; the .proto file now
            ;; If we did not parse the .proto file, process the generated
            ;; .proto-imports file now.
            (cond ((not proto-date)
                   (warn "Could not find the .proto file to be imported: ~A" proto-file))
                  ((or (not (and lisp-date imports-date))
                       (< lisp-date proto-date)
                       (< imports-date proto-date))
                   (parse-protobuf-file proto-file lisp-file imports-file)
                   (setq lisp-date (file-write-date lisp-file))
                   (setq imports-date (file-write-date imports-file)))
                  (t
                   (process-imports-from-file imports-file))))
          ;; Compile the .lisp file, if necessary
          (cond ((not lisp-date)
                 (unless (string= (pathname-type base-path) "proto")
                   (warn "Could not find the .lisp file to be compiled: ~A" lisp-file)))
                (t
                 (when (or (not fasl-date)
                           (< fasl-date lisp-date))
                   (let ((*compile-file-pathname* lisp-file)
                         (*load-pathname* nil))
                     (setq fasl-file (compile-file lisp-file)))
                   (setq fasl-date (file-write-date fasl-file)))))
          ;; Load the .fasl file
          (cond ((not fasl-date)
                 (unless (string= (pathname-type base-path) "proto")
                   (warn "Could not find the .fasl file to be loaded: ~A" fasl-file)))
                (t
                 (let ((*compile-file-pathname* nil)
                       (*load-pathname* fasl-file))
                   (load fasl-file)))))
        (return (values))))))
