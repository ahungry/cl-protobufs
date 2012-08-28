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


;;; Some examples; also for some for manual testing

#||
;; A pretty useful subset of air schedule objects
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
;; A pretty useful subset of geographic business data
(defclass geodata ()
  ;; This one stores the data in lists
  ((countries :type (proto:list-of qres-core::country)
              :initform ()
              :initarg :countries)
   (regions :type (proto:list-of qres-core::region)
            :initform ()
            :initarg :regions)
   (cities :type (proto:list-of qres-core::city)
           :initform ()
           :initarg :cities)
   (airports :type (proto:list-of qres-core::airport)
             :initform ()
             :initarg :airports)))

(defclass geodata-v ()
  ;; This one stores the data in vectors
  ((countries :type (proto:vector-of qres-core::country)
              :initform #()
              :initarg :countries)
   (regions :type (proto:vector-of qres-core::region)
            :initform #()
            :initarg :regions)
   (cities :type (proto:vector-of qres-core::city)
           :initform #()
           :initarg :cities)
   (airports :type (proto:vector-of qres-core::airport)
             :initform #()
             :initarg :airports)))

(setq *geodata* (proto:generate-schema-for-classes
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
                   geodata geodata-v)
                 :install t))

(proto:write-schema *geodata*)
(proto:write-schema *geodata* :type :lisp)

;; Load the data
(let* ((countries (loop for v being the hash-values of (qres-core::country-business-data) collect (car v)))
       (regions   (loop for v being the hash-values of (qres-core::region-business-data) collect v))
       (cities    (loop for v being the hash-values of (qres-core::city-business-data) collect (car v)))
       (airports  (loop for v being the hash-values of (car (qres-core::airport-business-data)) collect (car v))))
  (setq geodata (make-instance 'geodata
                  :countries countries
                  :regions regions
                  :cities cities
                  :airports airports)
        geodata-v (make-instance 'geodata-v
                    :countries (make-array (length countries) :fill-pointer t :initial-contents countries)
                    :regions (make-array (length regions) :fill-pointer t :initial-contents regions)
                    :cities (make-array (length cities) :fill-pointer t :initial-contents cities)
                    :airports (make-array (length airports) :fill-pointer t :initial-contents airports))))

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
                 geodata geodata-v))
  (let ((message (proto:find-message *geodata* class)))
    (eval (proto-impl:generate-object-size  message))
    (eval (proto-impl:generate-serializer   message))
    (eval (proto-impl:generate-deserializer message))))

(time (progn (setq gser (proto:serialize-object-to-bytes geodata 'geodata)) nil))
(time (proto:deserialize-object 'geodata gser))

(equalp gser (proto:serialize-object-to-bytes
              (proto:deserialize-object 'geodata gser) 'geodata))

(time (progn (setq gser-v (proto:serialize-object-to-bytes geodata-v 'geodata-v)) nil))
(time (proto:deserialize-object 'geodata-v gser-v))

(equalp gser-v (proto:serialize-object-to-bytes
                (proto:deserialize-object 'geodata-v gser-v) 'geodata-v))

(equalp gser gser-v)
||#


#||
;; Lisp lists :-)
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
  (proto:serialize-object-to-bytes list 'typed-list)
  (proto:print-text-format list 'typed-list)
  (proto:print-text-format list 'typed-list :suppress-line-breaks t)
  (let ((text (with-output-to-string (s)
                (proto:print-text-format list 'typed-list :stream s))))
    (with-input-from-string (s text)
      (proto:parse-text-format 'typed-list :stream s))))

(let ((list '((1 one) (2 two) (3 three))))
  (proto:serialize-object-to-bytes list 'typed-list)
  (proto:print-text-format list 'typed-list)
  (proto:print-text-format list 'typed-list :suppress-line-breaks t)
  (let ((text (with-output-to-string (s)
                (proto:print-text-format list 'typed-list :stream s))))
    (with-input-from-string (s text)
      (proto:parse-text-format 'typed-list :stream s))))
||#


#||
;; Extension example
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

(let* ((wheel  (make-instance 'color-wheel :name "Colors"))
       (color1 (make-instance 'color :r-value 100 :g-value 0 :b-value 100))
       (rqst1  (make-instance 'add-color-request :wheel wheel :color color1))
       (color2 (make-instance 'color :r-value 100 :g-value 0 :b-value 100))
       (rqst2  (make-instance 'add-color-request :wheel wheel :color color2)))
  (setf (color-opacity color2) 50)
  (progn
    (format t "~2&Unextended (has-extension ~S)~%" (has-extension color1 'opacity))
    (let ((ser1 (proto:serialize-object-to-bytes rqst1 'add-color-request)))
      (print ser1)
      (proto:print-text-format rqst1)
      (proto:print-text-format (proto:deserialize-object 'add-color-request ser1))))
  (progn 
    (format t "~2&Extended (has-extension ~S)~%" (has-extension color2 'opacity))
    (let ((ser2 (proto:serialize-object-to-bytes rqst2 'add-color-request)))
      (print ser2)
      (proto:print-text-format rqst2)
      (proto:print-text-format (proto:deserialize-object 'add-color-request ser2)))))
||#


#||
;; Group example
(proto:define-schema color-wheel1
    (:package color-wheel
     ;; :optimize :speed
     :documentation "Color wheel example, with nested message")
  (proto:define-message color-wheel1 ()
    (proto:define-message metadata1 ()
      (author :type (or null string))
      (revision :type (or null string))
      (date :type (or null string)))
    (name :type string)
    (colors :type (proto:list-of color1))
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
    (colors :type (proto:list-of color2))
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

(let* ((meta1  (make-instance 'metadata1 :revision "1.0"))
       (wheel1 (make-instance 'color-wheel1 :name "Colors" :metadata1 meta1))
       (color1 (make-instance 'color1 :r-value 100 :g-value 0 :b-value 100))
       (rqst1  (make-instance 'add-color1 :wheel wheel1 :color color1))
       (meta2  (make-instance 'metadata2 :revision "1.0"))
       (wheel2 (make-instance 'color-wheel2 :name "Colors" :metadata2 meta2))
       (color2 (make-instance 'color2 :r-value 100 :g-value 0 :b-value 100))
       (rqst2  (make-instance 'add-color2 :wheel wheel2 :color color2)))
  (progn
    (format t "~2&Nested")
    (let ((ser1 (proto:serialize-object-to-bytes rqst1 'add-color1)))
      (print ser1)
      (proto:print-text-format rqst1)
      (proto:print-text-format (proto:deserialize-object 'add-color1 ser1))))
  (progn
    (format t "~2&Group")
    (let ((ser2 (proto:serialize-object-to-bytes rqst2 'add-color2)))
      (print ser2)
      (proto:print-text-format rqst2)
      (proto:print-text-format (proto:deserialize-object 'add-color2 ser2)))))
||#
