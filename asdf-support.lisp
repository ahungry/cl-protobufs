;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 ITA Software, Inc.  All rights reserved.      ;;;
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
   (relative-pathname :accessor proto-relative-pathname
                      :initform nil
                      :initarg :relative-pathname)
   ;; A search path to try when looking for system-provided .proto files
   (search-path :accessor proto-search-path
                :initform ()
                :initarg :search-path))
  (:documentation
   "This ASDF component defines COMPILE-OP and LOAD-OP operations
    that compiles the .proto file into a .lisp file. You must then
    compile the generated .lisp file in another module."))

(defclass proto-to-lisp (compile-op) ())

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
    ;; Path was specified with ':relative-pathname'
    (merge-pathnames
      (make-pathname :type "proto")
      (merge-pathnames (pathname (proto-relative-pathname component))
                        (component-pathname (component-parent component))))
    ;; No ':relative-pathname', the  path of the protobuf file
    ;; defaults to that of the Lisp file with a ".proto" suffix
    (merge-pathnames
      (make-pathname :type "proto")
      (component-pathname component))))

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
  (let* ((pathname  (pathname path))
         (directory (pathname-directory pathname)))
    (if (and (list directory) (eq (car directory) :absolute))
      pathname
      (let ((resolved-path (merge-pathnames pathname parent-path)))
        (make-pathname :directory (pathname-directory resolved-path)
                       :name nil
                       :type nil
                       :defaults resolved-path)))))

(defmethod input-files ((op proto-to-lisp) (component protobuf-file))
  (list (protobuf-input-file component)))

(defmethod output-files ((op proto-to-lisp) (component protobuf-file))
  (values (list (component-pathname component))
          nil))

(defmethod perform ((op proto-to-lisp) (component protobuf-file))
  (let* ((input  (protobuf-input-file component))
         (output (first (output-files op component)))
         (paths  (cons (directory-namestring input) (resolve-search-path component))))
    (dolist (path paths (error 'compile-failed
                               :component component :operation op))
      (let ((source (merge-pathnames path (pathname input))))
        (when (probe-file source)
          (return-from perform
            (proto-impl:parse-protobuf-file
             (make-pathname :type "proto" :defaults source)
             (make-pathname :type "lisp"  :defaults output))))))))

(defmethod operation-description ((op proto-to-lisp) (component protobuf-file))
  (format nil (compatfmt "~@<proto-compiling ~3i~_~A~@:>")
          (make-pathname :name (pathname-name (component-pathname component))
                         :type "proto"
                         :defaults (first (output-files op component)))))

(defmethod perform ((op compile-op) (component protobuf-file))
  (let ((source (make-pathname :name (pathname-name (component-pathname component))
                               :type "lisp"
                               :defaults (first (output-files op component))))
        (output (first (output-files op component)))
        (*compile-file-warnings-behaviour* (operation-on-warnings op))
        (*compile-file-failure-behaviour* (operation-on-failure op)))
    (multiple-value-bind (output warnings-p failure-p)
        (apply #'compile-file* source
               :output-file output
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

(defmethod operation-description ((op compile-op) (component protobuf-file))
  (format nil (compatfmt "~@<compiling ~3i~_~A~@:>")
          (make-pathname :name (pathname-name (component-pathname component))
                         :type "lisp"
                         :defaults (first (output-files op component)))))


(in-package "PROTO-IMPL")

(defun parse-protobuf-file (protobuf-file lisp-file)
  (let ((schema (parse-schema-from-file protobuf-file)))
    (with-open-file (stream lisp-file
                     :direction :output
                     :if-exists :supersede
                     :external-format :utf-8
                     :element-type 'character)
      (write-schema schema :stream stream :type :lisp)))
  lisp-file)

;; Process 'import' lines
(defun process-imports (schema &rest imports)
  "Imports all of the files given by 'imports'.
   If the file is a .proto file, it first parses it and writes a .lisp file.
   The .lisp file is the compiled and loaded."
  (dolist (import imports)
    (let* ((base-path  (if *compile-file-pathname*
                         (merge-pathnames (pathname import) *compile-file-pathname*)
                         (pathname import)))
           (proto-file (make-pathname :type "proto" :defaults base-path))
           (lisp-file  (make-pathname :type "lisp"  :defaults base-path))
           (fasl-file  (compile-file-pathname lisp-file))
           (proto-date (and (probe-file proto-file)
                            (ignore-errors (file-write-date proto-file))))
           (lisp-date  (and (probe-file lisp-file)
                            (ignore-errors (file-write-date lisp-file))))
           (fasl-date  (and (probe-file fasl-file)
                            (ignore-errors (file-write-date fasl-file)))))
      (when (string= (pathname-type base-path) "proto")
        ;; The user asked to import a .proto file
        ;; If there's no .lisp file or an older .lisp file, parse the .proto file now
        (cond ((not proto-date)
               (warn "Could not find the .proto file to be imported: ~A" proto-file))
              ((or (not lisp-date)
                   (< lisp-date proto-date))
               (parse-protobuf-file proto-file lisp-file)
               (setq lisp-date (file-write-date lisp-file)))))
      ;; Compile the .lisp file, if necessary
      (cond ((not lisp-date)
             (unless (string= (pathname-type base-path) "proto")
               (warn "Could not find the .lisp file to be compiled: ~A" lisp-file)))
            (t
             (when (or (not fasl-date)
                       (< fasl-date lisp-date))
               (setq fasl-file (compile-file lisp-file))
               (setq fasl-date (file-write-date fasl-file)))
             ;; Now we can load the .fasl file
             (load fasl-file)))
      (let* ((imported (find-schema base-path)))
        (when imported
          (setf (proto-imported-schemas schema)
                (nconc (proto-imported-schemas schema) (list imported)))))
      base-path)))
