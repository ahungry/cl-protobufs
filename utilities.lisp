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


;;; Code generation utilities

;; "class-name" -> "ClassName"
(defun class-name->proto (x)
  "Given a Lisp class name, returns a Protobufs message or enum name."
  (remove-if-not #'alphanumericp
                 (camel-case (format nil "~A" x) :separators '(#\- #\_ #\/ #\. #\space))))

;; "enum-value" -> "ENUM_VALUE"
(defun enum-name->proto (x &optional prefix)
  "Given a Lisp enum value name, returns a Protobufs enum value name."
  (let* ((x (string-upcase (string x)))
         (x (if (and prefix (starts-with x prefix)) (subseq x (length prefix)) x)))
    (remove-if-not #'(lambda (x) (or (alphanumericp x) (eql x #\_)))
                   (format nil "~{~A~^_~}"
                           (split-string x :separators '(#\- #\_ #\/ #\. #\space))))))

;; "slot-name" -> "slot_name" or "slotName"
(defvar *camel-case-field-names* nil)
(defun slot-name->proto (x)
  "Given a Lisp slot name, returns a Protobufs field name."
  (if *camel-case-field-names*
    (remove-if-not #'alphanumericp
                   (camel-case-but-one (format nil "~A" x) :separators '(#\- #\_ #\/ #\. #\space)))
    (let ((x (string-downcase (string x))))
      (remove-if-not #'(lambda (x) (or (alphanumericp x) (eql x #\_)))
                     (format nil "~{~A~^_~}"
                             (split-string x :separators '(#\- #\_ #\/ #\. #\space)))))))


;; "ClassName" -> "class-name"
(defun proto->class-name (x &optional package)
  "Given a Protobufs message or enum type name, returns a Lisp class or type name."
  (let ((name (nstring-upcase (uncamel-case x))))
    (if package (intern name package) (make-symbol name))))

;; "ENUM_VALUE" -> "enum-name"
(defun proto->enum-name (x &optional package)
  "Given a Protobufs enum value name, returns a Lisp enum value name."
  (let ((name (format nil "~{~A~^-~}"
                      (split-string (string-upcase x) :separators '(#\_)))))
    (if package (intern name package) (make-symbol name))))

;; "slot_name" or "slotName" -> "slot-name"
(defun proto->slot-name (x &optional package)
  "Given a Protobufs field value name, returns a Lisp slot name."
  (let ((name (format nil "~{~A~^-~}" (split-string (nstring-upcase (uncamel-case x)) :separators '(#\_)))))
    (if package (intern name package) (make-symbol name))))


(defun make-lisp-symbol (string)
  "Intern a string of the 'package:string' and return the symbol."
  (let* ((colon (position #\: string))
         (pkg   (if colon (subseq string 0 colon) "KEYWORD"))
         (sym   (if colon (subseq string (+ colon 1)) string)))
    (intern sym pkg)))

#-quux
(defun fintern (format-string &rest format-args)
  "Interns a new symbol in the current package."
  (declare (dynamic-extent format-args))
  (intern (nstring-upcase (apply #'format nil format-string format-args))))

#-quux
(defun kintern (format-string &rest format-args)
  "Interns a new symbol in the keyword package."
  (declare (dynamic-extent format-args))
  (intern (nstring-upcase (apply #'format nil format-string format-args)) "KEYWORD"))


(define-condition protobufs-warning (warning simple-condition) ())

(defun protobufs-warn (format-control &rest format-arguments)
  (warn 'protobufs-warning
        :format-control format-control
        :format-arguments format-arguments))


;;; Other utilities

;; A parameterized list types for repeated fields (not type-checked!)
(deftype list-of (type)
  (if (eq type 'null)
    'null
    'list))


#-quux (progn

;;; Optimized fixnum arithmetic

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
  `(ldb ,bytespec (the fixnum ,value)))


;;; Collectors, etc

(defmacro with-gensyms ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (b) `(,b (gensym ,(string b)))) bindings)
     ,@body))

(defmacro with-prefixed-accessors (names (prefix object) &body body)
  `(with-accessors (,@(loop for name in names
                            collect `(,name ,(fintern "~A~A" prefix name))))
       ,object
     ,@body))


(defmacro with-collectors ((&rest collection-descriptions) &body body)
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


;;; Function programming, please

(defun curry (function &rest args)
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


;;; String utilities

(defun starts-with (string prefix &key (start 0))
  (and (i>= (length string) (i+ start (length prefix)))
       (string-equal string prefix :start1 start :end1 (i+ start (length prefix)))
       prefix))

(defun ends-with (string suffix &key (end (length string)))
  (and (i>= end (length suffix))
       (string-equal string suffix :start1 (i- end (length suffix)) :end1 end)
       suffix))


;; (camel-case "camel-case") => "CamelCase"
(defun camel-case (string &key (separators '(#\-)))
  (let ((words (split-string string :separators separators)))
    (format nil "~{~@(~A~)~}" words)))

;; (camel-case-but-one "camel-case") => "camelCase"
(defun camel-case-but-one (string &key (separators '(#\-)))
  (let ((words (split-string string :separators separators)))
    (format nil "~(~A~)~{~@(~A~)~}" (car words) (cdr words))))

;; (uncamel-case "CamelCase") => "Camel-Case"
(defun uncamel-case (string &key (separator #\-))
  (format nil (format nil "~~{~~A~~^~C~~}" separator)
          (cl-ppcre:split "(?<=[a-z])(?=[A-Z])" string)))


(defun split-string (line &key (start 0) (end (length line)) (separators '(#\-)))
  (unless (i= start end)
    (loop for this fixnum = start then (i+ next 1)
          for next fixnum = (or (position-if #'(lambda (ch) (member ch separators)) line
                                             :start this :end end)
                                end)
          for piece = (string-right-trim '(#\space) (subseq line this next))
          when (not (i= (length piece) 0))
            collect piece
          until (i>= next end))))

)       ;#-quux


;;; Floating point utilities

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
