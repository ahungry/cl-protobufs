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


;;; Examples, for manual testing

;;--- Turn these into a test suite

#||
(setq pnr-schema (proto:generate-schema-for-classes
                  '(qres-core::legacy-pnr
                    qres-core::legacy-pnr-pax
                    qres-core::legacy-pnr-segment
                    qres-core::legacy-pnr-pax-segment)
                  :slot-filter #'quake::quake-slot-filter
                  :type-filter #'quake::quake-type-filter
                  :enum-filter #'quake::quake-enum-filter
                  :value-filter #'quake::quake-value-filter))

(proto:write-schema pnr-schema)
(proto:write-schema pnr-schema :type :lisp)

(proto:serialize-object-to-stream pnr 'qres-core::legacy-pnr :stream nil)
||#

#||
(setq sched-schema (proto:generate-schema-for-classes
                    '(quux::zoned-time
                      sched::scheduled-flight
                      sched::flight-designator
                      sched::flight-key
                      sched::scheduled-segment
                      sched::segment-key
                      sched::subsegment-key
                      sched::scheduled-leg
                      sched::leg-key
                      sched::revision-entry)
                    :package :qres-sched
                    :slot-filter #'quake::quake-slot-filter
                    :type-filter #'quake::quake-type-filter
                    :enum-filter #'quake::quake-enum-filter
                    :value-filter #'quake::quake-value-filter))

(proto:write-schema sched-schema)
(proto:write-schema sched-schema :type :lisp)
||#

#||
(defclass geodata ()
  ((countries :type (proto:list-of qres-core::country) :initform () :initarg :countries)
   (regions :type (proto:list-of qres-core::region) :initform () :initarg :regions)
   (cities :type (proto:list-of qres-core::city) :initform () :initarg :cities)
   (airports :type (proto:list-of qres-core::airport) :initform () :initarg :airports)))

(setq bizd-schema (proto:generate-schema-for-classes
                   '(qres-core::country
                     qres-core::region
                     qres-core::region-key
                     qres-core::city
                     qres-core::airport
                     qres-core::timezone
                     qres-core::tz-variation
                     qres-core::currency
                     qres-core::country-currencies
                     qres-core::carrier
                     geodata)
                   :install t))

(proto:write-schema bizd-schema)
(proto:write-schema bizd-schema :type :lisp)

(let* ((countries (loop for v being the hash-values of (qres-core::country-business-data) collect (car v)))
       (regions   (loop for v being the hash-values of (qres-core::region-business-data) collect v))
       (cities    (loop for v being the hash-values of (qres-core::city-business-data) collect (car v)))
       (airports  (loop for v being the hash-values of (car (qres-core::airport-business-data)) collect (car v))))
  (setq geodata (make-instance 'geodata
                  :countries countries
                  :regions regions
                  :cities cities
                  :airports airports)))

(dolist (class '(qres-core::country
                 qres-core::region
                 qres-core::region-key
                 qres-core::city
                 qres-core::airport
                 qres-core::timezone
                 qres-core::tz-variation
                 qres-core::currency
                 qres-core::country-currencies
                 qres-core::carrier
                 geodata))
  (let ((message (proto-impl:find-message bizd-schema class)))
    (eval (proto-impl:generate-object-size  message))
    (eval (proto-impl:generate-serializer   message))
    (eval (proto-impl:generate-deserializer message))))

(time (progn (setq gser (proto:serialize-object-to-stream geodata 'geodata :stream nil)) nil))
(time (proto:deserialize-object 'geodata gser))

(equalp gser (proto:serialize-object-to-stream
              (proto:deserialize-object 'geodata gser)
              'geodata :stream nil))
||#

#||
(setq pschema (proto:generate-schema-for-classes
               '(proto:protobuf proto:protobuf-option
                 proto:protobuf-enum proto:protobuf-enum-value
                 proto:protobuf-message proto:protobuf-field proto:protobuf-extension
                 proto:protobuf-service proto:protobuf-method)))

(proto:write-schema pschema)
(proto:write-schema pschema :type :lisp)

(progn (setq pser (proto:serialize-object-to-stream pschema 'proto:protobuf :stream nil)) nil)
(describe (proto:deserialize-object 'proto:protobuf pser))

(proto:print-text-format pschema)
(proto:print-text-format (proto:deserialize-object 'proto:protobuf pser))

(dolist (class '(proto:protobuf
                 proto:protobuf-option
                 proto:protobuf-enum
                 proto:protobuf-enum-value
                 proto:protobuf-message
                 proto:protobuf-field
                 proto:protobuf-extension
                 proto:protobuf-service
                 proto:protobuf-method))
  (let ((message (proto-impl:find-message pschema class)))
    (eval (proto-impl:generate-object-size  message))
    (eval (proto-impl:generate-serializer   message))
    (eval (proto-impl:generate-deserializer message))))
||#

#||
(defclass proto-test1 ()
  ((intval :type (integer -2147483648 +2147483647)
           :initarg :intval)))

(defclass proto-test2 ()
  ((intval :type (or null (integer -2147483648 +2147483647))
           :initform nil
           :initarg :intval)
   (strval :type (or null string)
           :initform nil
           :initarg :strval)))

(defclass proto-test3 ()
  ((intval :type (or null (integer -2147483648 +2147483647))
           :initform nil
           :initarg :intval)
   (strval :type (or null string)
           :initform nil
           :initarg :strval)
   (recval :type (or null proto-test1)
           :initform nil
           :initarg :recval)))

(defclass proto-test4 ()
  ((intval :type (or null (integer -2147483648 +2147483647))
           :initform nil
           :initarg :intval)
   (strval :type (or null string)
           :initform nil
           :initarg :strval)
   (recval :type (or null proto-test2)
           :initform nil
           :initarg :recval)))

(defclass proto-test5 ()
  ((color   :type (member :red :green :blue)
            :initarg :color)
   (intvals :type (proto:list-of integer)
            :initform ()
            :initarg :intvals)
   (strvals :type (proto:list-of string)
            :initform ()
            :initarg :strvals)))

(defclass proto-test6 ()
  ((intvals :type (proto:list-of integer)
            :initform ()
            :initarg :intvals)
   (strvals :type (proto:list-of string)
            :initform ()
            :initarg :strvals)
   (recvals :type (proto:list-of proto-test2)
            :initform ()
            :initarg :recvals)))

(setq test-schema (proto:generate-schema-for-classes
                   '(proto-test1 proto-test2 proto-test3 proto-test4 proto-test5 proto-test6)
                   :install t))

(proto:write-schema test-schema)
(proto:write-schema test-schema :type :lisp)

(dolist (class '(proto-test1 proto-test2 proto-test3 proto-test4 proto-test5 proto-test6))
  (let ((message (proto-impl:find-message test-schema class)))
    (eval (proto-impl:generate-object-size  message))
    (eval (proto-impl:generate-serializer   message))
    (eval (proto-impl:generate-deserializer message))))

(setq test1  (make-instance 'proto-test1 :intval 150))
(setq test1b (make-instance 'proto-test1 :intval -150))
(setq test2  (make-instance 'proto-test2 :strval "testing"))
(setq test2b (make-instance 'proto-test2 :strval "1 2 3"))
(setq test3  (make-instance 'proto-test3 :recval test1))
(setq test4  (make-instance 'proto-test4 :recval test2))
(setq test5  (make-instance 'proto-test5 :color :red
                                         :intvals '(2 3 5 7) :strvals '("two" "three" "five" "seven")))
(setq test6  (make-instance 'proto-test6 :intvals '(2 3 5 7) :strvals '("two" "three" "five" "seven")
                                         :recvals (list test2 test2b)))

(setq tser1 (proto:serialize-object-to-stream test1 'proto-test1 :stream nil))
(equalp tser1 #(#x08 #x96 #x01))
(describe (proto:deserialize-object 'proto-test1 tser1))

(setq tser1b (proto:serialize-object-to-stream test1b 'proto-test1 :stream nil))
(equalp tser1b #(#x08 #xEA #xFE #xFF #xFF #x0F))
(describe (proto:deserialize-object 'proto-test1 tser1b))

(setq tser2 (proto:serialize-object-to-stream test2 'proto-test2 :stream nil))
(equalp tser2 #(#x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67))
(describe (proto:deserialize-object 'proto-test2 tser2))

(setq tser3 (proto:serialize-object-to-stream test3 'proto-test3 :stream nil))
(equalp tser3 #(#x1A #x03 #x08 #x96 #x01))
(describe (proto:deserialize-object 'proto-test3 tser3))
(describe (slot-value (proto:deserialize-object 'proto-test3 tser3) 'recval))

(setq tser4 (proto:serialize-object-to-stream test4 'proto-test4 :stream nil))
(equalp tser4 #(#x1A #x09 #x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67))
(describe (proto:deserialize-object 'proto-test4 tser4))
(describe (slot-value (proto:deserialize-object 'proto-test4 tser4) 'recval))

(setq tser5 (proto:serialize-object-to-stream test5 'proto-test5 :stream nil))
(equalp tser5 #(#x08 #x00
                #x10 #x04 #x02 #x03 #x05 #x07
                #x1A #x03 #x74 #x77 #x6F #x1A #x05 #x74 #x68 #x72 #x65 #x65 #x1A #x04 #x66 #x69 #x76 #x65 #x1A #x05 #x73 #x65 #x76 #x65 #x6E))
(describe (proto:deserialize-object 'proto-test5 tser5))

(setq tser6 (proto:serialize-object-to-stream test6 'proto-test6 :stream nil))
(equalp tser6 #(#x08 #x04 #x02 #x03 #x05 #x07 #x12 #x03 #x74 #x77 #x6F #x12 #x05 #x74 #x68 #x72 #x65 #x65 #x12 #x04 #x66 #x69 #x76 #x65 #x12 #x05 #x73 #x65 #x76 #x65 #x6E #x1A #x09 #x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67 #x1A #x07 #x12 #x05 #x31 #x20 #x32 #x20 #x33))
(describe (proto:deserialize-object 'proto-test6 tser6))
(describe (slot-value (proto:deserialize-object 'proto-test6 tser6) 'recvals))


(equalp (mapcar #'proto-impl:zig-zag-encode32
                '(0 -1 1 -2 2 -2147483648 2147483647))
        '(0 1 2 3 4 4294967295 4294967294))
(equalp (mapcar #'proto-impl:zig-zag-encode64
                '(0 -1 1 -2 2 -2147483648 2147483647 -1152921504606846976 1152921504606846975))
        '(0 1 2 3 4 4294967295 4294967294 2305843009213693951 2305843009213693950))

(proto:print-text-format test1)
(proto:print-text-format (proto:deserialize-object 'proto-test1 tser1))
(let ((text (with-output-to-string (s)
              (proto:print-text-format test1 'proto-test1 :stream s))))
  (with-input-from-string (s text)
    (proto:parse-text-format 'proto-test1 :stream s)))

(proto:print-text-format test2)
(proto:print-text-format (proto:deserialize-object 'proto-test2 tser2))
(let ((text (with-output-to-string (s)
              (proto:print-text-format test2 'proto-test2 :stream s))))
  (with-input-from-string (s text)
    (proto:parse-text-format 'proto-test2 :stream s)))

(proto:print-text-format test3)
(proto:print-text-format (proto:deserialize-object 'proto-test3 tser3))
(let ((text (with-output-to-string (s)
              (proto:print-text-format test3 'proto-test3 :stream s))))
  (with-input-from-string (s text)
    (proto:parse-text-format 'proto-test3 :stream s)))

(proto:print-text-format test4)
(proto:print-text-format (proto:deserialize-object 'proto-test4 tser4))
(let ((text (with-output-to-string (s)
              (proto:print-text-format test4 'proto-test4 :stream s))))
  (with-input-from-string (s text)
    (proto:parse-text-format 'proto-test4 :stream s)))

(proto:print-text-format test5)
(proto:print-text-format (proto:deserialize-object 'proto-test5 tser5))
(let ((text (with-output-to-string (s)
              (proto:print-text-format test5 'proto-test5 :stream s))))
  (with-input-from-string (s text)
    (proto:parse-text-format 'proto-test5 :stream s)))

(proto:print-text-format test6)
(proto:print-text-format (proto:deserialize-object 'proto-test6 tser6))
(let ((text (with-output-to-string (s)
              (proto:print-text-format test6 'proto-test6 :stream s))))
  (with-input-from-string (s text)
    (proto:parse-text-format 'proto-test6 :stream s)))
||#

#||
(let* ((enums (list (make-instance 'proto:protobuf-enum
                      :name "ColorName"
                      :values (list (make-instance 'proto:protobuf-enum-value
                                      :name "RED"
                                      :index 1
                                      :value :red)
                                    (make-instance 'proto:protobuf-enum-value
                                      :name "GREEN"
                                      :index 2
                                      :value :green)
                                    (make-instance 'proto:protobuf-enum-value
                                      :name "BLUE"
                                      :index 3
                                      :value :blue)))))
       (msgs  (list (make-instance 'proto:protobuf-message
                      :name "Color"
                      :enums (list (make-instance 'proto:protobuf-enum
                                      :name "ContrastName"
                                      :values (list (make-instance 'proto:protobuf-enum-value
                                                      :name "LOW"
                                                      :index 1
                                                      :value :high)
                                                    (make-instance 'proto:protobuf-enum-value
                                                      :name "HIGH"
                                                      :index 100
                                                      :value :low))))
                      :fields (list (make-instance 'proto:protobuf-field
                                      :name "color"
                                      :type "ColorName"
                                      :required :required
                                      :index 1)
                                    (make-instance 'proto:protobuf-field
                                      :name "contrast"
                                      :type "ContrastName"
                                      :required :optional
                                      :index 2
                                      :default "LOW")))))
       (methods  (list (make-instance 'proto:protobuf-method
                         :name "GetColor"
                         :input-name "string"
                         :output-name "Color")
                       (make-instance 'proto:protobuf-method
                         :name "SetColor"
                         :input-name "Color"
                         :output-name "Color"
                         :options (list (make-instance 'proto:protobuf-option
                                          :name "deadline" :value 1.0)))))
       (svcs  (list (make-instance 'proto:protobuf-service
                      :name "ColorWheel"
                      :methods methods)))
       (proto (make-instance 'proto:protobuf
                :package "ita.color"
                :imports '("descriptor.proto")
                :enums enums
                :messages msgs
                :services svcs)))
  ;; The output should be example the same as the output of 'write-schema' below
  (proto:write-schema proto))
||#

#||
(proto:define-schema color-wheel
    (:package ita.color
     :import "descriptor.proto"
     :documentation "Color wheel example")
  (proto:define-enum color-name
      (:documentation "A color name")
    red
    green
    blue)
  (proto:define-message color
      (:conc-name color-
       :documentation "Color and contrast")
    (proto:define-enum contrast-name
        (:documentation "A contrast name")
      (low    1)
      (high 100))
    (color    :type color-name)
    (contrast :type (or null contrast-name) :default :low))
  (proto:define-service color-wheel
      (:documentation "Get and set colors")
    (get-color (string color))
    (set-color (color color)
               :options ("deadline" 1.0))))

=> (PROGN
     (DEFTYPE COLOR-NAME () '(MEMBER :RED :GREEN :BLUE))
     (DEFTYPE CONTRAST-NAME () '(MEMBER :LOW :HIGH))
     (DEFCLASS COLOR ()
       ((COLOR :TYPE COLOR-NAME :ACCESSOR COLOR-COLOR :INITARG :COLOR)
        (CONTRAST :TYPE (OR NULL CONTRAST-NAME) :ACCESSOR COLOR-CONTRAST :INITARG :CONTRAST :INITFORM :LOW)))
     (DEFVAR *COLOR-WHEEL*
       (MAKE-INSTANCE 'PROTOBUF-SCHEMA
         :NAME "ColorWheel"
         :CLASS 'COLOR-WHEEL
         :PACKAGE "ita.color"
         :IMPORTS '("descriptor.proto")
         :SYNTAX "proto2"
         :OPTIONS ()
         :ENUMS (LIST (MAKE-INSTANCE 'PROTOBUF-ENUM
                        :NAME "ColorName"
                        :CLASS 'COLOR-NAME
                        :VALUES (LIST (MAKE-INSTANCE 'PROTOBUF-ENUM-VALUE
                                        :NAME "RED" :INDEX 1 :VALUE :RED)
                                      (MAKE-INSTANCE 'PROTOBUF-ENUM-VALUE
                                        :NAME "GREEN" :INDEX 2 :VALUE :GREEN)
                                      (MAKE-INSTANCE 'PROTOBUF-ENUM-VALUE
                                        :NAME "BLUE" :INDEX 3 :VALUE :BLUE))))
         :MESSAGES (LIST (MAKE-INSTANCE 'PROTOBUF-MESSAGE
                           :NAME "Color"
                           :CLASS 'COLOR
                           :CONC-NAME "COLOR-"
                           :ENUMS (LIST (MAKE-INSTANCE 'PROTOBUF-ENUM
                                          :NAME "ContrastName"
                                          :CLASS 'CONTRAST-NAME
                                          :VALUES (LIST (MAKE-INSTANCE 'PROTOBUF-ENUM-VALUE
                                                          :NAME "LOW" :INDEX 1 :VALUE :LOW)
                                                        (MAKE-INSTANCE 'PROTOBUF-ENUM-VALUE
                                                          :NAME "HIGH" :INDEX 100 :VALUE :HIGH))))
                           :MESSAGES (LIST)
                           :FIELDS (LIST (MAKE-INSTANCE 'PROTOBUF-FIELD
                                           :NAME "color"
                                           :TYPE "ColorName"
                                           :CLASS 'COLOR-NAME
                                           :REQUIRED :REQUIRED
                                           :INDEX 1
                                           :VALUE 'COLOR
                                           :DEFAULT NIL
                                           :PACKED NIL)
                                         (MAKE-INSTANCE 'PROTOBUF-FIELD
                                           :NAME "contrast"
                                           :TYPE "ContrastName"
                                           :CLASS 'CONTRAST-NAME
                                           :REQUIRED :OPTIONAL
                                           :INDEX 2
                                           :VALUE 'CONTRAST
                                           :DEFAULT "LOW"
                                           :PACKED NIL))))
         :SERVICES (LIST (MAKE-INSTANCE 'PROTOBUF-SERVICE
                           :NAME "ColorWheel"
                           :CLASS 'COLOR-WHEEL
                           :METHODS (LIST (MAKE-INSTANCE 'PROTOBUF-METHOD
                                            :NAME "GetColor"
                                            :CLASS 'GET-COLOR
                                            :INPUT-NAME "string"
                                            :OUTPUT-NAME "Color"
                                            :OPTIONS (LIST))
                                          (MAKE-INSTANCE 'PROTOBUF-METHOD
                                            :NAME "SetColor"
                                            :CLASS 'SET-COLOR
                                            :INPUT-NAME "Color"
                                            :OUTPUT-NAME "Color"
                                            :OPTIONS (LIST (MAKE-INSTANCE 'PROTOBUF-OPTION
                                                             :NAME "deadline" :VALUE "1.0")))))))))

;; The output should be example the same as the output of 'write-schema' above
(proto:write-schema *color-wheel*)

;; How does the Lisp version look?
(proto:write-schema *color-wheel* :type :lisp)

(setq clr (make-instance 'color :color :red))
(setq cser (proto:serialize-object-to-stream clr 'color :stream nil))
(proto:print-text-format clr)
(proto:print-text-format (proto:deserialize-object 'color cser))
||#

#||
(let ((ps "package ita.color;

import \"descriptor.proto\";

enum ColorName {
  RED = 1;
  GREEN = 2;
  BLUE = 3;
}

message Color {
  enum ContrastName {
    LOW = 1;
    HIGH = 100;
  }
  required ColorName color = 1;
  optional ContrastName contrast = 2 [default = LOW];
}

service ColorWheel {
  rpc GetColor (string) returns (Color);
  rpc SetColor (Color) returns (Color) {
    option deadline = 1.0;
  }
}"))
  (with-input-from-string (s ps)
    (setq ppp (proto:parse-schema-from-stream s))))

(proto:write-schema ppp)
(proto:write-schema ppp :type :lisp)
||#

#||
(proto:define-schema typed-list ()
  (proto:define-message typed-list ()
    (string-car  :type (or null string)  :reader string-car)
    (symbol-car  :type (or null string)  :reader symbol-car)
    (integer-car :type (or null integer) :reader integer-car)
    (float-car   :type (or null single-float) :reader float-car)
    (list-car  :type (or null typed-list) :reader list-car)
    (list-cdr  :type (or null typed-list) :reader list-cdr)))

(defun string-car (x)
  (and (stringp (car x)) (car x)))

(defun symbol-car (x)
  (and (symbolp (car x)) (symbol-name (car x))))

(defun integer-car (x)
  (and (integerp (car x)) (car x)))

(defun float-car (x)
  (and (floatp (car x)) (car x)))

(defun list-car (x)
  (etypecase (car x)
    ((or string symbol integer float) nil)
    (list (car x))))

(defun list-cdr (x) 
  (assert (listp (cdr x)) ())
  (cdr x))

(let ((list '("this" "is" "a" ("nested" "test"))))
  (proto:serialize-object-to-stream list 'typed-list :stream nil)
  (proto:print-text-format list 'typed-list)
  (proto:print-text-format list 'typed-list :suppress-line-breaks t)
  (let ((text (with-output-to-string (s)
                (proto:print-text-format list 'typed-list :stream s))))
    (with-input-from-string (s text)
      (proto:parse-text-format 'typed-list :stream s))))

(let ((list '((1 one) (2 two) (3 three))))
  (proto:serialize-object-to-stream list 'typed-list :stream nil)
  (proto:print-text-format list 'typed-list)
  (proto:print-text-format list 'typed-list :suppress-line-breaks t)
  (let ((text (with-output-to-string (s)
                (proto:print-text-format list 'typed-list :stream s))))
    (with-input-from-string (s text)
      (proto:parse-text-format 'typed-list :stream s))))
||#

#||
(proto:define-schema integrity-test ()
  (proto:define-message inner ()
    (i :type (or null integer)))
  (proto:define-message outer ()
    (inner :type (proto:list-of inner))
    (simple :type (or null inner))
    (i :type (or null integer))))

(defun integrity-test (message)
  (let* ((type (type-of message))
         (buf (proto:serialize-object-to-stream message type :stream nil))
         (new (proto:deserialize-object type buf))
         (newbuf (proto:serialize-object-to-stream new type :stream nil)))
    (assert (equalp (length buf) (length newbuf)))
    (assert (equalp buf newbuf))
    (assert (string= (with-output-to-string (s)
                       (proto:print-text-format message nil :stream s))
                     (with-output-to-string (s)
                       (proto:print-text-format new nil :stream s))))
    new))

(integrity-test (make-instance 'outer :i 4))

(integrity-test (make-instance 'outer 
                  :inner (mapcar #'(lambda (i) (make-instance 'inner :i i)) '(1 2 3))))

(integrity-test (make-instance 'outer 
                  :simple (make-instance 'inner :i 4)))
||#


;;; Stubby examples

#||
(proto:define-schema color-wheel
    (:package color-wheel
     :optimize :speed
     :documentation "Color wheel example")
  (proto:define-message color-wheel
      (:conc-name color-wheel-)
    (name   :type string)
    (colors :type (proto:list-of color) :default ()))
  (proto:define-message color
      (:conc-name color-
       :documentation "A (named) color")
    (name    :type (or string null))
    (r-value :type integer)
    (g-value :type integer)
    (b-value :type integer)
    (proto:define-extension 1000 max))
  (proto:define-extend color ()
    ((opacity 1000) :type (or null integer)))
  (proto:define-message get-color-request ()
    (wheel :type color-wheel)
    (name  :type string))
  (proto:define-message add-color-request ()
    (wheel :type color-wheel)
    (color :type color))
  (proto:define-service color-wheel ()
    (get-color (get-color-request color)
      :options ("deadline" 1.0)
      :documentation "Look up a color by name")
    (add-color (add-color-request color)
      :options ("deadline" 1.0)
      :documentation "Add a new color to the wheel")))

(proto:write-schema *color-wheel*)
(proto:write-schema *color-wheel* :type :lisp)

(progn ;with-rpc-channel (rpc)
  (let* ((wheel  (make-instance 'color-wheel :name "Colors"))
         (color1 (make-instance 'color :r-value 100 :g-value 0 :b-value 100))
         (rqst1  (make-instance 'add-color-request :wheel wheel :color color1))
         (color2 (make-instance 'color :r-value 100 :g-value 0 :b-value 100))
         (rqst2  (make-instance 'add-color-request :wheel wheel :color color2)))
    (setf (color-opacity color2) 50)
    #-ignore (progn
               (format t "~2&Unextended (has-extension ~S)~%" (has-extension color1 'opacity))
               (let ((ser1 (proto:serialize-object-to-stream rqst1 'add-color-request :stream nil)))
                 (print ser1)
                 (proto:print-text-format rqst1)
                 (proto:print-text-format (proto:deserialize-object 'add-color-request ser1))))
    #-ignore (progn 
               (format t "~2&Extended (has-extension ~S)~%" (has-extension color2 'opacity))
               (let ((ser2 (proto:serialize-object-to-stream rqst2 'add-color-request :stream nil)))
                 (print ser2)
                 (proto:print-text-format rqst2)
                 (proto:print-text-format (proto:deserialize-object 'add-color-request ser2))))
    #+stubby (add-color request)
    #+ignore (add-color request)))
||#

#||
(let ((ps "syntax = \"proto2\";

package color_wheel;

option optimize_for = SPEED;

message ColorWheel {
  required string name = 1;
  repeated Color colors = 2;
  optional group Metadata = 3 {
    optional string author = 1;
    optional string revision = 2;
    optional string date = 3;
  }
}

message Color {
  optional string name = 1;
  required int64 r_value = 2;
  required int64 g_value = 3;
  required int64 b_value = 4;
  extensions 1000 to max;
}

extend Color {
  optional int64 opacity = 1000;
}

message GetColorRequest {
  required ColorWheel wheel = 1;
  required string name = 2;
}

message AddColorRequest {
  required ColorWheel wheel = 1;
  required Color color = 2;
}

service ColorWheel {
  rpc GetColor (GetColorRequest) returns (Color) {
    option deadline = 1.0;
  }
  rpc AddColor (AddColorRequest) returns (Color) {
    option deadline = 1.0;
  }
}"))
  (with-input-from-string (s ps)
    (setq cw (proto:parse-schema-from-stream s))))

(proto:define-schema color-wheel1
    (:package color-wheel
     ;; :optimize :speed
     :documentation "Color wheel example, with nested")
  (proto:define-message color-wheel1 ()
    (proto:define-message metadata1 ()
      (author :type (or null string))
      (revision :type (or null string))
      (date :type (or null string)))
    (name :type string)
    (colors :type (list-of color1))
    (metadata1 :type (or null metadata1)))
  (proto:define-message color1 ()
    (name :type (or null string))
    (r-value :type integer)
    (g-value :type integer)
    (b-value :type integer))
  (proto:define-message add-color1 ()
    (wheel :type color-wheel1)
    (color :type color1)))

(proto:define-schema color-wheel2
    (:package color-wheel
     ;; :optimize :speed
     :documentation "Color wheel example, with group")
  (proto:define-message color-wheel2 ()
    (name :type string)
    (colors :type (list-of color2))
    (proto:define-group metadata2
        (:index 3
         :arity :optional)
      (author :type (or null string))
      (revision :type (or null string))
      (date :type (or null string))))
  (proto:define-message color2 ()
    (name :type (or null string))
    (r-value :type integer)
    (g-value :type integer)
    (b-value :type integer))
  (proto:define-message add-color2 ()
    (wheel :type color-wheel2)
    (color :type color2)))

(proto:write-schema *color-wheel1*)
(proto:write-schema *color-wheel2*)

(progn ;with-rpc-channel (rpc)
  (let* ((meta1  (make-instance 'metadata1 :revision "1.0"))
         (wheel1 (make-instance 'color-wheel1 :name "Colors" :metadata1 meta1))
         (color1 (make-instance 'color1 :r-value 100 :g-value 0 :b-value 100))
         (rqst1  (make-instance 'add-color1 :wheel wheel1 :color color1))
         (meta2  (make-instance 'metadata2 :revision "1.0"))
         (wheel2 (make-instance 'color-wheel2 :name "Colors" :metadata2 meta2))
         (color2 (make-instance 'color2 :r-value 100 :g-value 0 :b-value 100))
         (rqst2  (make-instance 'add-color2 :wheel wheel2 :color color2)))
    #-ignore (progn
               (format t "~2&Nested")
               (let ((ser1 (proto:serialize-object-to-stream rqst1 'add-color1 :stream nil)))
                 (print ser1)
                 (proto:print-text-format rqst1)
                 (proto:print-text-format (proto:deserialize-object 'add-color1 ser1))))
    #-ignore (progn
               (format t "~2&Group")
               (let ((ser2 (proto:serialize-object-to-stream rqst2 'add-color2 :stream nil)))
                 (print ser2)
                 (proto:print-text-format rqst2)
                 (proto:print-text-format (proto:deserialize-object 'add-color2 ser2))))))
||#
