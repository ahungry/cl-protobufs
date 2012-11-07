;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-IMPL")


;;; Optimized fixnum arithmetic

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter $optimize-default     '(optimize (speed 1) (safety 3) (debug 3))
  "Compiler optimization settings for safe, debuggable code.")
(defparameter $optimize-fast-unsafe '(optimize (speed 3) (safety 0) (debug 0))
  "Compiler optimization settings for fast, unsafe, hard-to-debug code.")

)       ;eval-when


(defmacro i+ (&rest fixnums)
  `(the fixnum (+ ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i- (number &rest fixnums)
  `(the fixnum (- (the fixnum ,number) ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i* (&rest fixnums)
  `(the fixnum (* ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i= (&rest fixnums)
  `(= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i< (&rest fixnums)
  `(< ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i<= (&rest fixnums)
  `(<= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i> (&rest fixnums)
  `(> ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i>= (&rest fixnums)
  `(>= ,@(loop for n in fixnums collect `(the fixnum ,n))))

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

(defmacro ildb (bytespec value)
  `(the fixnum (ldb ,bytespec (the fixnum ,value))))


;;; String utilities

(defun starts-with (string prefix &key (start 0))
  "Returns true if 'string' starts with the prefix 'prefix' (case insensitive)."
  (and (i>= (length string) (i+ start (length prefix)))
       (string-equal string prefix :start1 start :end1 (i+ start (length prefix)))
       prefix))

(defun ends-with (string suffix &key (end (length string)))
  "Returns true if 'string' ends with the prefix 'prefix' (case insensitive)."
  (and (i>= end (length suffix))
       (string-equal string suffix :start1 (i- end (length suffix)) :end1 end)
       suffix))

(defun strcat (&rest strings)
  "Concatenate a bunch of strings."
  (declare (dynamic-extent strings))
  (apply #'concatenate 'string strings))


;; (camel-case "camel-case") => "CamelCase"
(defun camel-case (string &optional (separators '(#\-)))
  "Take a hyphen-separated string and turn it into a camel-case string."
  (let ((words (split-string string :separators separators)))
    (format nil "~{~@(~A~)~}" words)))

;; (camel-case-but-one "camel-case") => "camelCase"
(defun camel-case-but-one (string &optional (separators '(#\-)))
  "Take a hyphen-separated string and turn its tail into a camel-case string."
  (let ((words (split-string string :separators separators)))
    (format nil "~(~A~)~{~@(~A~)~}" (car words) (cdr words))))


;; (uncamel-case "CamelCase") => "CAMEL-CASE"
;; (uncamel-case "TCPConnection") => "TCP-CONNECTION"
;; (uncamel-case "NewTCPConnection") => "NEW-TCP-CONNECTION"
;; (uncamel-case "new_RPC_LispService") => "NEW-RPC-LISP-SERVICE"
;; (uncamel-case "RPC_LispServiceRequest_get_request") => "RPC-LISP-SERVICE-REQUEST-GET-REQUEST"
;; (uncamel-case "TCP2Name3") => "TCP2-NAME3"
(defun uncamel-case (name)
  "Take a camel-case string and turn it into a hyphen-separated string."
  ;; We need a whole state machine to get this right
  (labels ((uncamel (chars state result)
             (let ((ch (first chars)))
               (cond ((null chars)
                      result)
                     ((upper-case-p ch)
                      (uncamel (rest chars) 'upper
                               (case state
                                 ((upper)
                                  ;; "TCPConnection" => "TCP-CONNECTION"
                                  (if (and (second chars) (lower-case-p (second chars)))
                                    (list* ch #\- result)
                                    (cons ch result)))
                                 ((lower digit) (list* ch #\- result))
                                 (otherwise (cons ch result)))))
                     ((lower-case-p ch)
                      (uncamel (rest chars) 'lower
                               (cons (char-upcase ch) result)))
                     ((digit-char-p ch)
                      (uncamel (rest chars) 'digit 
                               (cons ch result)))
                     ((or (eql ch #\-) (eql ch #\_))
                      (uncamel (rest chars) 'dash
                               (cons #\- result)))
                     ((eql ch #\.)
                      (uncamel (rest chars) 'dot
                               (cons #\. result)))
                     (t
                      (error "Invalid name character: ~A" ch))))))
    (strcat (nreverse (uncamel (concatenate 'list name) nil ())))))


(defun split-string (line &key (start 0) (end (length line)) (separators '(#\-)))
  "Given a string 'string', splits it at each of the separators.
   Returns a list of the string pieces, with empty pieces removed."
  (unless (i= start end)
    (loop for this fixnum = start then (i+ next 1)
          for next fixnum = (or (position-if #'(lambda (ch) (member ch separators)) line
                                             :start this :end end)
                                end)
          for piece = (string-right-trim '(#\space) (subseq line this next))
          when (not (i= (length piece) 0))
            collect piece
          until (i>= next end))))


;;; Managing symbols

(defmacro with-gensyms ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (b) `(,b (gensym ,(string b)))) bindings)
     ,@body))

(defun make-lisp-symbol (string)
  "Intern a string of the 'package:string' and return the symbol."
  (let* ((string (string string))
         (colon  (position #\: string))
         (pkg    (if colon (subseq string 0 colon) "KEYWORD"))
         (sym    (if colon (subseq string (+ colon 1)) string)))
    (intern sym pkg)))

(defun fintern (format-string &rest format-args)
  "Interns a new symbol in the current package."
  (declare (dynamic-extent format-args))
  (intern (nstring-upcase (apply #'format nil format-string format-args))))

(defun kintern (format-string &rest format-args)
  "Interns a new symbol in the keyword package."
  (declare (dynamic-extent format-args))
  (intern (nstring-upcase (apply #'format nil format-string format-args)) "KEYWORD"))

(defun keywordify (x)
  "Given a symbol designator 'x', return a keyword whose name is 'x'.
   If 'x' is nil, this returns nil."
  (check-type x (or string symbol null))
  (cond ((null x) nil)
        ((keywordp x) x)
        ((symbolp x) (keywordify (symbol-name x)))
        ((zerop (length x)) nil)
        ((string-not-equal x "nil")
         (intern (string-upcase x) (find-package "KEYWORD")))
        (t nil)))


;;; Collectors, etc

(defmacro with-collectors ((&rest collection-descriptions) &body body)
  "'collection-descriptions' is a list of clauses of the form (coll function).
   The body can call each 'function' to add a value to 'coll'. 'function'
   runs in constant time, regardless of the length of the list."
  (let ((let-bindings  ())
        (flet-bindings ())
        (dynamic-extents ())
        (vobj '#:OBJECT))
    (dolist (description collection-descriptions)
      (destructuring-bind (place name) description
        (let ((vtail (make-symbol (format nil "~A-TAIL" place))))
          (setq dynamic-extents
                (nconc dynamic-extents `(#',name)))
          (setq let-bindings
                (nconc let-bindings
                       `((,place ())
                         (,vtail nil))))
          (setq flet-bindings
                (nconc flet-bindings
                       `((,name (,vobj)
                           (setq ,vtail (if ,vtail
                                          (setf (cdr ,vtail)  (list ,vobj))
                                          (setf ,place (list ,vobj)))))))))))
    `(let (,@let-bindings)
       (flet (,@flet-bindings)
         ,@(and dynamic-extents
                `((declare (dynamic-extent ,@dynamic-extents))))
         ,@body))))

(defmacro with-prefixed-accessors (names (prefix object) &body body)
  `(with-accessors (,@(loop for name in names
                            collect `(,name ,(fintern "~A~A" prefix name))))
       ,object
     ,@body))

(defmacro dovector ((var vector &optional value) &body body)
  "Like 'dolist', but iterates over the vector 'vector'."
  (with-gensyms (vidx vlen vvec)
    `(let* ((,vvec ,vector)
            (,vlen (length ,vvec)))
       (loop for ,vidx fixnum from 0 below ,vlen
             as ,var = (aref ,vvec ,vidx)
             do (progn ,@body)
             finally (return ,value)))))

(defmacro doseq ((var sequence &optional value) &body body)
  "Iterates over a sequence, using 'dolist' or 'dovector' depending on
   the type of the sequence. In optimized code, this turns out to be
   faster than (map () #'f sequence).
   Note that the body gets expanded twice!"
  (with-gensyms (vseq)
    `(let ((,vseq ,sequence))
       (if (vectorp ,vseq)
         (dovector (,var ,vseq ,value)
           ,@body)
         (dolist (,var ,vseq ,value)
           ,@body)))))


;;; Functional programming, please

(defun curry (function &rest args)
  "Returns a function that applies 'function' to 'args', plus any
   additional arguments given at the call site."
  (if (and args (null (cdr args)))                      ;fast test for length = 1
    (let ((arg (car args)))
      #'(lambda (&rest more-args)
          (apply function arg more-args)))
    #'(lambda (&rest more-args)
        (apply function (append args more-args)))))

(define-compiler-macro curry (&whole form function &rest args &environment env)
  (declare (ignore env))
  (if (and (listp function)
           (eq (first function) 'function)
           (symbolp (second function))
           (and args (null (cdr args))))
    `#'(lambda (&rest more-args)
         (apply ,function ,(car args) more-args))
    form))


;;; Types

;; A parameterized list type for repeated fields
;; The elements aren't type-checked
(deftype list-of (type)
  (if (eq type 'nil) ; a list that cannot have any element (element-type nil) is null.
    'null
    'list))

;; The same, but use a (stretchy) vector
(deftype vector-of (type)
  (if (eq type 'nil); an array that cannot have any element (element-type nil) is of size 0.
    '(array * (0))
    '(array * (*))))            ;an 1-dimensional array of any type

;; This corresponds to the :bytes Protobufs type
(deftype byte-vector () '(array (unsigned-byte 8) (*)))

(defun make-byte-vector (size)
  (make-array size :element-type '(unsigned-byte 8)))

;; The Protobufs integer types
(deftype    int32 () '(signed-byte 32))
(deftype    int64 () '(signed-byte 64))
(deftype   uint32 () '(unsigned-byte 32))
(deftype   uint64 () '(unsigned-byte 64))
(deftype   sint32 () '(signed-byte 32))
(deftype   sint64 () '(signed-byte 64))
(deftype  fixed32 () '(signed-byte 32))
(deftype  fixed64 () '(signed-byte 64))
(deftype sfixed32 () '(signed-byte 32))
(deftype sfixed64 () '(signed-byte 64))


;;; Code generation utilities

(defvar *proto-name-separators* '(#\- #\_ #\/ #\space))
(defvar *camel-case-field-names* nil)

(defun find-proto-package (name)
  "A very fuzzy definition of 'find-package'."
  (typecase name
    ((or string symbol)
     ;; Try looking under the given name and the all-uppercase name
     (or (find-package (string name))
         (find-package (string-upcase (string name)))))
    (cons
     ;; If 'name' is a list, it's actually a fully-qualified path
     (or (find-proto-package (first name))
         (find-proto-package (format nil "~{~A~^.~}" name))))))

;; "class-name" -> "ClassName", ("ClassName")
;; "outer-class.inner-class" -> "InnerClass", ("OuterClass" "InnerClass")
(defun class-name->proto (x)
  "Given a Lisp class name, returns a Protobufs message or enum name.
   The second value is the fully qualified name, as a list."
  (let* ((xs (split-string (string x) :separators '(#\.)))
         (ns (loop for x in (butlast xs)
                   collect (remove-if-not #'alphanumericp
                                          (camel-case (format nil "~A" x) *proto-name-separators*))))
         (nx (car (last xs)))
         (name (remove-if-not #'alphanumericp (camel-case nx *proto-name-separators*))))
    (values name (append ns (list name))
            ;; This might be the name of a package, too
            (format nil "~{~A~^.~}" (butlast xs)))))

;; "enum-value" -> "ENUM_VALUE", ("ENUM_VALUE")
;; "class-name.enum-value" -> "ENUM_VALUE", ("ClassName" "ENUM_VALUE")
(defun enum-name->proto (x &optional prefix)
  "Given a Lisp enum value name, returns a Protobufs enum value name.
   The second value is the fully qualified name, as a list."
  (let* ((xs (split-string (string x) :separators '(#\.)))
         (ns (loop for x in (butlast xs)
                   collect (remove-if-not #'alphanumericp
                                          (camel-case (format nil "~A" x) *proto-name-separators*))))
         (nx (string-upcase (car (last xs))))
         (nx (if (and prefix (starts-with nx prefix)) (subseq nx (length prefix)) nx))
         ;; Keep underscores, they are standards separators in Protobufs enum names
         (name (remove-if-not #'(lambda (x) (or (alphanumericp x) (eql x #\_)))
                              (format nil "~{~A~^_~}"
                                      (split-string nx :separators *proto-name-separators*)))))
    (values name (append ns (list name))
            (format nil "~{~A~^.~}" (butlast xs)))))

;; "slot-name" -> "slot_name", ("slot_name") or "slotName", ("slotName")
;; "class-name.slot-name" -> "Class.slot_name", ("ClassName" "slot_name")
(defun slot-name->proto (x)
  "Given a Lisp slot name, returns a Protobufs field name.
   The second value is the fully qualified name, as a list."
  (let* ((xs (split-string (string x) :separators '(#\.)))
         (ns (loop for x in (butlast xs)
                   collect (remove-if-not #'alphanumericp
                                          (camel-case (format nil "~A" x) *proto-name-separators*))))
         (nx (string-downcase (car (last xs))))
         (name (if *camel-case-field-names*
                 (remove-if-not #'alphanumericp
                                (camel-case-but-one (format nil "~A" nx) *proto-name-separators*))
                 ;; Keep underscores, they are standards separators in Protobufs field names
                 (remove-if-not #'(lambda (x) (or (alphanumericp x) (eql x #\_)))
                                (format nil "~{~A~^_~}"
                                        (split-string nx :separators *proto-name-separators*))))))
    (values name (append ns (list name))
            (format nil "~{~A~^.~}" (butlast xs)))))


;; "ClassName" -> 'class-name
;; "cl-user.ClassName" -> 'cl-user::class-name
;; "cl-user.OuterClass.InnerClass" -> 'cl-user::outer-class.inner-class
(defun proto->class-name (x &optional package)
  "Given a Protobufs message or enum type name, returns a Lisp class or type name.
   This resolves Protobufs qualified names as best as it can."
  (let* ((xs (split-string (substitute #\- #\_ (uncamel-case x))
                           :separators '(#\.)))
         (pkg1 (and (cdr xs) (find-proto-package (first xs))))
         (pkgn (and (cdr xs) (find-proto-package (butlast xs))))
         (package (or pkg1 pkgn package))
         (name (format nil "~{~A~^.~}" (if pkg1 (cdr xs) (if pkgn (last xs) xs)))))
    (values (if package (intern name package) (make-symbol name)) package xs
            ;; This might be the name of a package, too
            (format nil "~{~A~^.~}" (butlast xs)))))

;; "ENUM_VALUE" -> :enum-value
;; "cl-user.ENUM_VALUE" -> :enum-value
;; "cl-user.OuterClass.ENUM_VALUE" -> :enum-value
(defun proto->enum-name (x &optional package)
  "Given a Protobufs enum value name, returns a Lisp enum value name.
   This resolves Protobufs qualified names as best as it can."
  (let* ((xs (split-string (substitute #\- #\_ (uncamel-case x))
                           :separators '(#\.)))
         (pkg1 (and (cdr xs) (find-proto-package (first xs))))
         (pkgn (and (cdr xs) (find-proto-package (butlast xs))))
         (package (or pkg1 pkgn package))
         (name (format nil "~{~A~^.~}" (if pkg1 (cdr xs) (if pkgn (last xs) xs)))))
    (values (kintern name) package xs
            (format nil "~{~A~^.~}" (butlast xs)))))

;; "slot_name" or "slotName" -> 'slot-name
;; "cl-user.slot_name" or "cl-user.slotName" -> 'cl-user::slot-name
;; "cl-user.OuterClass.slot_name" -> 'cl-user::outer-class.slot-name
(defun proto->slot-name (x &optional package)
  "Given a Protobufs field value name, returns a Lisp slot name.
   This resolves Protobufs qualified names as best as it can."
  (let* ((xs (split-string (substitute #\- #\_ (uncamel-case x))
                           :separators '(#\.)))
         (pkg1 (and (cdr xs) (find-proto-package (first xs))))
         (pkgn (and (cdr xs) (find-proto-package (butlast xs))))
         (package (or pkg1 pkgn package))
         (name (format nil "~{~A~^.~}" (if pkg1 (cdr xs) (if pkgn (last xs) xs)))))
    (values (if package (intern name package) (make-symbol name)) package xs
            (format nil "~{~A~^.~}" (butlast xs)))))


;;; Warnings

(define-condition protobufs-warning (warning simple-condition) ())

(defun protobufs-warn (format-control &rest format-arguments)
  (warn 'protobufs-warning
        :format-control format-control
        :format-arguments format-arguments))


#-(or allegro lispworks)
(defmacro without-redefinition-warnings (() &body body)
  `(progn ,@body))
    
#+allegro
(defmacro without-redefinition-warnings (() &body body)
  `(excl:without-redefinition-warnings ,@body))

#+lispworks
(defmacro without-redefinition-warnings (() &body body)
  `(let ((dspec:*redefinition-action* :quiet)) ,@body))


;;; Portable floating point utilities

#+(or abcl allegro cmu sbcl lispworks)
(defun single-float-bits (x)
  (declare (type single-float x))
  #+abcl    (system:single-float-bits x)
  #+allegro (multiple-value-bind (high low)
                (excl:single-float-to-shorts x)
              (declare (type (unsigned-byte 16) high low))
              (logior (ash high 16) low))
  #+cmu  (kernel:single-float-bits x)
  #+sbcl (sb-kernel:single-float-bits x)
  #+lispworks (lispworks-float:single-float-bits x))

#-(or abcl allegro cmu sbcl lispworks)
(defun single-float-bits (x)
  (declare (type single-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
    (if (eql x 0.0f0) 0 #x-80000000)
    (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
        (integer-decode-float x)
      (assert (plusp lisp-significand))
      (let* ((significand lisp-significand)
             (exponent (+ lisp-exponent 23 127))
             (unsigned-result
              (if (plusp exponent)                      ;if not obviously denormalized
                (do () (nil)
                  (cond
                    ;; Special termination case for denormalized float number
                    ((zerop exponent)
                     ;; Denormalized numbers have exponent one greater than
                     ;; in the exponent field
                     (return (ash significand -1)))
                    ;; Ordinary termination case
                    ((>= significand (expt 2 23))
                     (assert (< 0 significand (expt 2 24)))
                     ;; Exponent 0 is reserved for denormalized numbers,
                     ;; and 255 is reserved for specials like NaN
                     (assert (< 0 exponent 255))
                     (return (logior (ash exponent 23)
                                     (logand significand (1- (ash 1 23))))))
                    (t
                     ;; Shift as necessary to set bit 24 of significand
                     (setq significand (ash significand 1)
                           exponent (1- exponent)))))
                (do () ((zerop exponent)
                        ;; Denormalized numbers have exponent one greater than
                        ;; the exponent field
                        (ash significand -1))
                  (unless (zerop (logand significand 1))
                    (warn "Denormalized '~S' losing bits in ~D" 'single-float-bits x))
                  (setq significand (ash significand -1)
                        exponent (1+ exponent))))))
        (ecase lisp-sign
          ((1)  unsigned-result)
          ((-1) (logior unsigned-result (- (expt 2 31)))))))))


#+(or abcl allegro cmu sbcl lispworks)
(defun double-float-bits (x)
  (declare (type double-float x))
  #+abcl    (values (system:double-float-low-bits x)
                    (system:double-float-high-bits x))
  #+allegro (multiple-value-bind (us3 us2 us1 us0)
                (excl:double-float-to-shorts x)
              (logior (ash us1 16) us0)
              (logior (ash us3 16) us2))
  #+cmu  (values (kernel:double-float-low-bits x)
                 (kernel:double-float-high-bits x))
  #+sbcl (values (sb-kernel:double-float-low-bits x)
                 (sb-kernel:double-float-high-bits x))
  #+lispworks (let ((bits (lispworks-float:double-float-bits x)))
                (values (logand #xffffffff bits)
                        (ash bits -32))))

#-(or abcl allegro cmu sbcl lispworks)
(defun double-float-bits (x)
  (declare (type double-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
    (if (eql x 0.0d0) 0 #x-8000000000000000)
    (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
        (integer-decode-float x)
      (assert (plusp lisp-significand))
      (let* ((significand lisp-significand)
             (exponent (+ lisp-exponent 52 1023))
             (unsigned-result
              (if (plusp exponent)                      ;if not obviously denormalized
                (do () (nil)
                  (cond
                    ;; Special termination case for denormalized float number
                    ((zerop exponent)
                     ;; Denormalized numbers have exponent one greater than
                     ;; in the exponent field
                     (return (ash significand -1)))
                    ;; Ordinary termination case
                    ((>= significand (expt 2 52))
                     (assert (< 0 significand (expt 2 53)))
                     ;; Exponent 0 is reserved for denormalized numbers,
                     ;; and 2047 is reserved for specials like NaN
                     (assert (< 0 exponent 2047))
                     (return (logior (ash exponent 52)
                                     (logand significand (1- (ash 1 52))))))
                    (t
                     ;; Shift as necessary to set bit 53 of significand
                     (setq significand (ash significand 1)
                           exponent (1- exponent)))))
                (do () ((zerop exponent)
                        ;; Denormalized numbers have exponent one greater than
                        ;; the exponent field
                        (ash significand -1))
                  (unless (zerop (logand significand 1))
                    (warn "Denormalized '~S' losing bits in ~D" 'double-float-bits x))
                  (setq significand (ash significand -1)
                        exponent (1+ exponent))))))
        (let ((result
               (ecase lisp-sign
                 ((1)  unsigned-result)
                 ((-1) (logior unsigned-result (- (expt 2 63)))))))
          ;; Return the low bits and the high bits
          (values (logand #xffffffff result) (ash result -32)))))))


#+(or abcl allegro cmu sbcl lispworks)
(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  #+abcl    (system:make-single-float bits)
  #+allegro (excl:shorts-to-single-float (ldb (byte 16 16) bits)
                                         (ldb (byte 16 0) bits))
  #+cmu  (kernel:make-single-float bits)
  #+sbcl (sb-kernel:make-single-float bits)
  #+lispworks (lispworks-float:make-single-float bits))

#-(or abcl allegro cmu sbcl lispworks)
(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  (cond
    ;; IEEE float special cases
    ((zerop bits) 0.0)
    ((= bits #x-80000000) -0.0)
    (t
     (let* ((sign (ecase (ldb (byte 1 31) bits)
                    (0 1.0)
                    (1 -1.0)))
            (iexpt (ldb (byte 8 23) bits))
            (exponent (if (zerop iexpt)                 ;denormalized
                        -126
                        (- iexpt 127)))
            (mantissa (* (logior (ldb (byte 23 0) bits)
                                 (if (zerop iexpt) 0 (ash 1 23)))
                         (expt 0.5 23))))
       (* sign (expt 2.0 exponent) mantissa)))))


#+(or abcl allegro cmu sbcl lispworks)
(defun make-double-float (low high)
  (declare (type (unsigned-byte 32) low)
           (type (signed-byte   32) high))
  #+abcl (system:make-double-float (logior (ash high 32) low))
  #+allegro (excl:shorts-to-double-float (ldb (byte 16 16) high)
                                         (ldb (byte 16 0) high)
                                         (ldb (byte 16 16) low)
                                         (ldb (byte 16 0) low))
  #+cmu  (kernel:make-double-float high low)
  #+sbcl (sb-kernel:make-double-float high low)
  #+lispworks (lispworks-float:make-double-float high low))

#-(or abcl allegro cmu sbcl lispworks)
(defun make-double-float (low high)
  (declare (type (unsigned-byte 32) low)
           (type (signed-byte   32) high))
  (cond
    ;; IEEE float special cases
    ((and (zerop high) (zerop low)) 0.0d0)
    ((and (= high #x-80000000)
          (zerop low)) -0.0d0)
    (t
     (let* ((bits (logior (ash high 32) low))
            (sign (ecase (ldb (byte 1 63) bits)
                    (0 1.0d0)
                    (1 -1.0d0)))
            (iexpt (ldb (byte 11 52) bits))
            (exponent (if (zerop iexpt)                 ;denormalized
                        -1022
                        (- iexpt 1023)))
            (mantissa (* (logior (ldb (byte 52 0) bits)
                                 (if (zerop iexpt) 0 (ash 1 52)))
                         (expt 0.5d0 52))))
       (* sign (expt 2.0d0 exponent) mantissa)))))
