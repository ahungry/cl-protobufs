;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-TEST")


;;; Basic serialization unit tests

(defclass basic-test1 ()
  ((intval :type (signed-byte 32)
           :initarg :intval)))

(defclass basic-test2 ()
  ((intval :type (or null (signed-byte 32))
           :initform nil
           :initarg :intval)
   (strval :type (or null string)
           :initform nil
           :initarg :strval)))

(defclass basic-test3 ()
  ((intval :type (or null (signed-byte 32))
           :initform nil
           :initarg :intval)
   (strval :type (or null string)
           :initform nil
           :initarg :strval)
   (recval :type (or null basic-test1)
           :initform nil
           :initarg :recval)))

(defclass basic-test4 ()
  ((intval :type (or null (signed-byte 32))
           :initform nil
           :initarg :intval)
   (strval :type (or null string)
           :initform nil
           :initarg :strval)
   (recval :type (or null basic-test2)
           :initform nil
           :initarg :recval)))

(defclass basic-test5 ()
  ((color   :type (member :red :green :blue)
            :initarg :color)
   (intvals :type (proto:list-of integer)
            :initform ()
            :initarg :intvals)
   (strvals :type (proto:list-of string)
            :initform ()
            :initarg :strvals)))

(defclass basic-test6 ()
  ((intvals :type (proto:list-of integer)
            :initform ()
            :initarg :intvals)
   (strvals :type (proto:list-of string)
            :initform ()
            :initarg :strvals)
   (recvals :type (proto:list-of basic-test2)
            :initform ()
            :initarg :recvals)))

(defvar *basic-test-schema*
  (generate-schema-for-classes
    '(basic-test1 basic-test2 basic-test3 basic-test4 basic-test5 basic-test6)
    :install t))

(qtest:define-test basic-serialization ()
  (let* ((test1  (make-instance 'basic-test1 :intval 150))
         (test1b (make-instance 'basic-test1 :intval -150))
         (test2  (make-instance 'basic-test2 :strval "testing"))
         (test2b (make-instance 'basic-test2 :strval "1 2 3"))
         (test3  (make-instance 'basic-test3 :recval test1))
         (test4  (make-instance 'basic-test4 :recval test2))
         (test5  (make-instance 'basic-test5
                   :color :red :intvals '(2 3 5 7) :strvals '("two" "three" "five" "seven")))
         (test6  (make-instance 'basic-test6
                   :intvals '(2 3 5 7) :strvals '("two" "three" "five" "seven") :recvals (list test2 test2b))))
    (let ((tser1  (serialize-object-to-stream test1 'basic-test1 :stream nil))
          (tser1b (serialize-object-to-stream test1b 'basic-test1 :stream nil))
          (tser2  (serialize-object-to-stream test2 'basic-test2 :stream nil))
          (tser3  (serialize-object-to-stream test3 'basic-test3 :stream nil))
          (tser4  (serialize-object-to-stream test4 'basic-test4 :stream nil))
          (tser5  (serialize-object-to-stream test5 'basic-test5 :stream nil))
          (tser6  (serialize-object-to-stream test6 'basic-test6 :stream nil)))
      (qtest:assert-true (equalp tser1 #(#x08 #x96 #x01)))
      (qtest:assert-true (equalp tser1b #(#x08 #xEA #xFE #xFF #xFF #x0F)))
      (qtest:assert-true (equalp tser2 #(#x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)))
      (qtest:assert-true (equalp tser3 #(#x1A #x03 #x08 #x96 #x01)))
      (qtest:assert-true (equalp tser4 #(#x1A #x09 #x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)))
      (qtest:assert-true (equalp tser5 #(#x08 #x00
                                         #x10 #x04 #x02 #x03 #x05 #x07
                                         #x1A #x03 #x74 #x77 #x6F #x1A #x05 #x74 #x68 #x72 #x65 #x65 #x1A #x04 #x66 #x69 #x76 #x65 #x1A #x05 #x73 #x65 #x76 #x65 #x6E)))
      (qtest:assert-true (equalp tser6 #(#x08 #x04 #x02 #x03 #x05 #x07 #x12 #x03 #x74 #x77 #x6F #x12 #x05 #x74 #x68 #x72 #x65 #x65 #x12 #x04 #x66 #x69 #x76 #x65 #x12 #x05 #x73 #x65 #x76 #x65 #x6E #x1A #x09 #x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67 #x1A #x07 #x12 #x05 #x31 #x20 #x32 #x20 #x33)))
      (macrolet ((slots-equalp (obj1 obj2 &rest slots)
                   (quux:with-gensyms (vobj1 vobj2)
                     (quux:with-collectors ((forms collect-form))
                       (dolist (slot slots)
                         (collect-form `(qtest:assert-true
                                         (equalp (slot-value ,vobj1 ',slot) (slot-value ,vobj2 ',slot)))))
                       `(let ((,vobj1 ,obj1)
                              (,vobj2 ,obj2))
                          ,@forms)))))
        (slots-equalp test1 (deserialize-object 'basic-test1 tser1)
                      intval)
        (slots-equalp test1b (deserialize-object 'basic-test1 tser1b)
                      intval)
        (slots-equalp test2 (deserialize-object 'basic-test2 tser2)
                      intval strval)
        (slots-equalp test3 (deserialize-object 'basic-test3 tser3)
                      intval strval)
        (slots-equalp (slot-value test3 'recval)
                      (slot-value (deserialize-object 'basic-test3 tser3) 'recval)
                      intval)
        (slots-equalp test4 (deserialize-object 'basic-test4 tser4)
                      intval strval)
        (slots-equalp (slot-value test4 'recval)
                      (slot-value (deserialize-object 'basic-test4 tser4) 'recval)
                      intval strval)
        (slots-equalp test5 (deserialize-object 'basic-test5 tser5)
                      color intvals strvals)
        (slots-equalp test6 (deserialize-object 'basic-test6 tser6)
                      intvals strvals)
        (slots-equalp (first (slot-value test6 'recvals))
                      (first (slot-value (deserialize-object 'basic-test6 tser6) 'recvals))
                      strval)
        (slots-equalp (second (slot-value test6 'recvals))
                      (second (slot-value (deserialize-object 'basic-test6 tser6) 'recvals))
                      strval)))))

(qtest:define-test basic-optimized-serialization ()
  (dolist (class '(basic-test1 basic-test2 basic-test3 basic-test4 basic-test5 basic-test6))
    (let ((message (find-message *basic-test-schema* class)))
      (eval (generate-object-size  message))
      (eval (generate-serializer   message))
      (eval (generate-deserializer message))))
  (let* ((test1  (make-instance 'basic-test1 :intval 150))
         (test1b (make-instance 'basic-test1 :intval -150))
         (test2  (make-instance 'basic-test2 :strval "testing"))
         (test2b (make-instance 'basic-test2 :strval "1 2 3"))
         (test3  (make-instance 'basic-test3 :recval test1))
         (test4  (make-instance 'basic-test4 :recval test2))
         (test5  (make-instance 'basic-test5
                   :color :red :intvals '(2 3 5 7) :strvals '("two" "three" "five" "seven")))
         (test6  (make-instance 'basic-test6
                   :intvals '(2 3 5 7) :strvals '("two" "three" "five" "seven") :recvals (list test2 test2b))))
    (let ((tser1  (serialize-object-to-stream test1 'basic-test1 :stream nil))
          (tser1b (serialize-object-to-stream test1b 'basic-test1 :stream nil))
          (tser2  (serialize-object-to-stream test2 'basic-test2 :stream nil))
          (tser3  (serialize-object-to-stream test3 'basic-test3 :stream nil))
          (tser4  (serialize-object-to-stream test4 'basic-test4 :stream nil))
          (tser5  (serialize-object-to-stream test5 'basic-test5 :stream nil))
          (tser6  (serialize-object-to-stream test6 'basic-test6 :stream nil)))
      (qtest:assert-true (equalp tser1 #(#x08 #x96 #x01)))
      (qtest:assert-true (equalp tser1b #(#x08 #xEA #xFE #xFF #xFF #x0F)))
      (qtest:assert-true (equalp tser2 #(#x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)))
      (qtest:assert-true (equalp tser3 #(#x1A #x03 #x08 #x96 #x01)))
      (qtest:assert-true (equalp tser4 #(#x1A #x09 #x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)))
      (qtest:assert-true (equalp tser5 #(#x08 #x00
                                         #x10 #x04 #x02 #x03 #x05 #x07
                                         #x1A #x03 #x74 #x77 #x6F #x1A #x05 #x74 #x68 #x72 #x65 #x65 #x1A #x04 #x66 #x69 #x76 #x65 #x1A #x05 #x73 #x65 #x76 #x65 #x6E)))
      (qtest:assert-true (equalp tser6 #(#x08 #x04 #x02 #x03 #x05 #x07 #x12 #x03 #x74 #x77 #x6F #x12 #x05 #x74 #x68 #x72 #x65 #x65 #x12 #x04 #x66 #x69 #x76 #x65 #x12 #x05 #x73 #x65 #x76 #x65 #x6E #x1A #x09 #x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67 #x1A #x07 #x12 #x05 #x31 #x20 #x32 #x20 #x33)))
      (macrolet ((slots-equalp (obj1 obj2 &rest slots)
                   (quux:with-gensyms (vobj1 vobj2)
                     (quux:with-collectors ((forms collect-form))
                       (dolist (slot slots)
                         (collect-form `(qtest:assert-true
                                         (equalp (slot-value ,vobj1 ',slot) (slot-value ,vobj2 ',slot)))))
                       `(let ((,vobj1 ,obj1)
                              (,vobj2 ,obj2))
                          ,@forms)))))
        (slots-equalp test1 (deserialize-object 'basic-test1 tser1)
                      intval)
        (slots-equalp test1b (deserialize-object 'basic-test1 tser1b)
                      intval)
        (slots-equalp test2 (deserialize-object 'basic-test2 tser2)
                      intval strval)
        (slots-equalp test3 (deserialize-object 'basic-test3 tser3)
                      intval strval)
        (slots-equalp (slot-value test3 'recval)
                      (slot-value (deserialize-object 'basic-test3 tser3) 'recval)
                      intval)
        (slots-equalp test4 (deserialize-object 'basic-test4 tser4)
                      intval strval)
        (slots-equalp (slot-value test4 'recval)
                      (slot-value (deserialize-object 'basic-test4 tser4) 'recval)
                      intval strval)
        (slots-equalp test5 (deserialize-object 'basic-test5 tser5)
                      color intvals strvals)
        (slots-equalp test6 (deserialize-object 'basic-test6 tser6)
                      intvals strvals)
        (slots-equalp (first (slot-value test6 'recvals))
                      (first (slot-value (deserialize-object 'basic-test6 tser6) 'recvals))
                      strval)
        (slots-equalp (second (slot-value test6 'recvals))
                      (second (slot-value (deserialize-object 'basic-test6 tser6) 'recvals))
                      strval)))))

(qtest:define-test text-serialization ()
  (let* ((test1  (make-instance 'basic-test1 :intval 150))
         (test1b (make-instance 'basic-test1 :intval -150))
         (test2  (make-instance 'basic-test2 :strval "testing"))
         (test2b (make-instance 'basic-test2 :strval "1 2 3"))
         (test3  (make-instance 'basic-test3 :recval test1))
         (test4  (make-instance 'basic-test4 :recval test2))
         (test5  (make-instance 'basic-test5
                   :color :red :intvals '(2 3 5 7) :strvals '("two" "three" "five" "seven")))
         (test6  (make-instance 'basic-test6
                   :intvals '(2 3 5 7) :strvals '("two" "three" "five" "seven") :recvals (list test2 test2b))))
    (let ((tser1  (serialize-object-to-stream test1 'basic-test1 :stream nil))
          (tser1b (serialize-object-to-stream test1b 'basic-test1 :stream nil))
          (tser2  (serialize-object-to-stream test2 'basic-test2 :stream nil))
          (tser3  (serialize-object-to-stream test3 'basic-test3 :stream nil))
          (tser4  (serialize-object-to-stream test4 'basic-test4 :stream nil))
          (tser5  (serialize-object-to-stream test5 'basic-test5 :stream nil))
          (tser6  (serialize-object-to-stream test6 'basic-test6 :stream nil)))
      (macrolet ((slots-equalp (obj1 obj2 &rest slots)
                   (quux:with-gensyms (vobj1 vobj2)
                     (quux:with-collectors ((forms collect-form))
                       (dolist (slot slots)
                         (collect-form `(qtest:assert-true
                                         (equalp (slot-value ,vobj1 ',slot) (slot-value ,vobj2 ',slot)))))
                       `(let ((,vobj1 ,obj1)
                              (,vobj2 ,obj2))
                          ,@forms)))))
        (let ((text (with-output-to-string (s)
                      (print-text-format test1 'basic-test1 :stream s))))
          (qtest:assert-true (string= text (with-output-to-string (s)
                                             (print-text-format
                                               (deserialize-object 'basic-test1 tser1) 'basic-test1 :stream s))))
          (slots-equalp test1 (with-input-from-string (s text)
                                (parse-text-format 'basic-test1 :stream s))
                        intval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test1b 'basic-test1 :stream s))))
          (qtest:assert-true (string= text (with-output-to-string (s)
                                             (print-text-format
                                               (deserialize-object 'basic-test1 tser1b) 'basic-test1 :stream s))))
          (slots-equalp test1b (with-input-from-string (s text)
                                 (parse-text-format 'basic-test1 :stream s))
                        intval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test2 'basic-test2 :stream s))))
          (qtest:assert-true (string= text (with-output-to-string (s)
                                             (print-text-format
                                               (deserialize-object 'basic-test2 tser2) 'basic-test2 :stream s))))
          (slots-equalp test2 (with-input-from-string (s text)
                                (parse-text-format 'basic-test2 :stream s))
                        intval strval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test3 'basic-test3 :stream s))))
          (qtest:assert-true (string= text (with-output-to-string (s)
                                             (print-text-format
                                               (deserialize-object 'basic-test3 tser3) 'basic-test3 :stream s))))
          (slots-equalp test3 (with-input-from-string (s text)
                                (parse-text-format 'basic-test3 :stream s))
                        intval strval)
          (slots-equalp (slot-value test3 'recval)
                        (slot-value (with-input-from-string (s text)
                                      (parse-text-format 'basic-test3 :stream s)) 'recval)
                        intval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test4 'basic-test4 :stream s))))
          (qtest:assert-true (string= text (with-output-to-string (s)
                                             (print-text-format
                                               (deserialize-object 'basic-test4 tser4) 'basic-test4 :stream s))))
          (slots-equalp test4 (with-input-from-string (s text)
                                (parse-text-format 'basic-test4 :stream s))
                        intval strval)
          (slots-equalp (slot-value test4 'recval)
                        (slot-value (with-input-from-string (s text)
                                      (parse-text-format 'basic-test4 :stream s)) 'recval)
                        intval strval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test5 'basic-test5 :stream s))))
          (qtest:assert-true (string= text (with-output-to-string (s)
                                             (print-text-format
                                               (deserialize-object 'basic-test5 tser5) 'basic-test5 :stream s))))
          (slots-equalp test5 (with-input-from-string (s text)
                                (parse-text-format 'basic-test5 :stream s))
                        color intvals strvals))
        (let ((text (with-output-to-string (s)
                      (print-text-format test6 'basic-test6 :stream s))))
          (qtest:assert-true (string= text (with-output-to-string (s)
                                             (print-text-format
                                               (deserialize-object 'basic-test6 tser6) 'basic-test6 :stream s))))
          (slots-equalp test6 (with-input-from-string (s text)
                                (parse-text-format 'basic-test6 :stream s))
                        intvals strvals)
          (slots-equalp (first (slot-value test6 'recvals))
                        (first (slot-value
                                 (with-input-from-string (s text)
                                   (parse-text-format 'basic-test6 :stream s)) 'recvals))
                        strval)
          (slots-equalp (second (slot-value test6 'recvals))
                        (second (slot-value
                                  (with-input-from-string (s text)
                                    (parse-text-format 'basic-test6 :stream s)) 'recvals))
                        strval))))))


(proto:define-schema integrity-test
    (:package proto_test)
  (proto:define-message inner ()
    (i :type (or null integer)))
  (proto:define-message outer ()
    (inner :type (proto:list-of inner))
    (simple :type (or null inner))
    (i :type (or null integer))))

(qtest:define-test serialization-integrity ()
  (flet ((do-test (message)
           (let* ((type (type-of message))
                  (buf (proto:serialize-object-to-stream message type :stream nil))
                  (new (proto:deserialize-object type buf))
                  (newbuf (proto:serialize-object-to-stream new type :stream nil)))
             (qtest:assert-true (equalp (length buf) (length newbuf)))
             (qtest:assert-true (equalp buf newbuf))
             (qtest:assert-true (string= (with-output-to-string (s)
                                           (proto:print-text-format message nil :stream s))
                                         (with-output-to-string (s)
                                           (proto:print-text-format new nil :stream s)))))))
    (do-test (make-instance 'outer :i 4))
    (do-test (make-instance 'outer :i -4))
    (do-test (make-instance 'outer 
               :inner (mapcar #'(lambda (i) (make-instance 'inner :i i)) '(1 2 3))))
    (do-test (make-instance 'outer 
               :inner (mapcar #'(lambda (i) (make-instance 'inner :i i)) '(-1 -2 -3))))
    (do-test (make-instance 'outer 
               :simple (make-instance 'inner :i 4)))
    (do-test (make-instance 'outer 
               :simple (make-instance 'inner :i -4)))))


#+qres (progn

(defclass geodata ()
  ((countries :type (proto:list-of qres-core::country) :initform () :initarg :countries)
   (regions :type (proto:list-of qres-core::region) :initform () :initarg :regions)
   (cities :type (proto:list-of qres-core::city) :initform () :initarg :cities)
   (airports :type (proto:list-of qres-core::airport) :initform () :initarg :airports)))

(defvar *geodata-schema*
  (proto:generate-schema-for-classes
   '(qres-core::country qres-core::region qres-core::region-key
     qres-core::city qres-core::airport
     qres-core::timezone qres-core::tz-variation
     qres-core::currency qres-core::country-currencies
     qres-core::carrier
     geodata)
   :install t))

(qtest:define-test geodata-serialization ()
  (let* ((countries (loop for v being the hash-values of (qres-core::country-business-data) collect (car v)))
         (regions   (loop for v being the hash-values of (qres-core::region-business-data) collect v))
         (cities    (loop for v being the hash-values of (qres-core::city-business-data) collect (car v)))
         (airports  (loop for v being the hash-values of (car (qres-core::airport-business-data)) collect (car v)))
         (geodata (make-instance 'geodata
                    :countries countries
                    :regions regions
                    :cities cities
                    :airports airports)))
    (let ((gser (proto:serialize-object-to-stream geodata 'geodata :stream nil)))
      (qtest:assert-true (equalp gser (proto:serialize-object-to-stream
                                       (proto:deserialize-object 'geodata gser)
                                       'geodata :stream nil))))))

(qtest:define-test geodata-optimized-serialization ()
  (dolist (class '(qres-core::country qres-core::region qres-core::region-key
                   qres-core::city qres-core::airport
                   qres-core::timezone qres-core::tz-variation
                   qres-core::currency qres-core::country-currencies
                   qres-core::carrier
                   geodata))
    (let ((message (find-message *geodata-schema* class)))
      (eval (generate-object-size  message))
      (eval (generate-serializer   message))
      (eval (generate-deserializer message))))
  (let* ((countries (loop for v being the hash-values of (qres-core::country-business-data) collect (car v)))
         (regions   (loop for v being the hash-values of (qres-core::region-business-data) collect v))
         (cities    (loop for v being the hash-values of (qres-core::city-business-data) collect (car v)))
         (airports  (loop for v being the hash-values of (car (qres-core::airport-business-data)) collect (car v)))
         (geodata (make-instance 'geodata
                    :countries countries
                    :regions regions
                    :cities cities
                    :airports airports)))
    (let ((gser (proto:serialize-object-to-stream geodata 'geodata :stream nil)))
      (qtest:assert-true (equalp gser (proto:serialize-object-to-stream
                                       (proto:deserialize-object 'geodata gser)
                                       'geodata :stream nil))))))

)       ;#+qres


;; Extension example
(proto:define-schema automobile
    (:package proto_test
     :optimize :speed)
  (proto:define-message automobile ()
    (proto:define-enum auto-status ()
      new
      used)
    (model  :type string)
    (color  :type auto-color)
    (status :type auto-status :default :new))
  (proto:define-message auto-color ()
    (name    :type (or string null))
    (r-value :type integer)
    (g-value :type integer)
    (b-value :type integer)
    (proto:define-extension 1000 max))
  (proto:define-extend auto-color (:conc-name paint-)
    ((metallic 1000) :type boolean))
  (proto:define-message buy-car-request ()
    (auto :type automobile))
  (proto:define-message buy-car-response ()
    (price :type (or null uint32)))
  (proto:define-service buy-car ()
    (buy-car (buy-car-request buy-car-response)
      :options (:deadline 1.0))))

(qtest:define-test extension-serialization ()
  (let* ((color1 (make-instance 'auto-color :r-value 100 :g-value 0 :b-value 100))
         (car1   (make-instance 'automobile :model "Audi" :color color1))
         (rqst1  (make-instance 'buy-car-request :auto car1))
         (color2 (make-instance 'auto-color :r-value 100 :g-value 0 :b-value 100))
         (car2   (make-instance 'automobile :model "Audi" :color color2))
         (rqst2  (make-instance 'buy-car-request :auto car2)))
    (setf (paint-metallic color2) t)
    (let ((ser1 (proto:serialize-object-to-stream rqst1 'buy-car-request :stream nil)))
      (qtest:assert-true (string= (with-output-to-string (s)
                                    (proto:print-text-format rqst1 nil :stream s))
                                  (with-output-to-string (s)
                                    (proto:print-text-format
                                      (proto:deserialize-object 'buy-car-request ser1)  nil :stream s)))))
    (let ((ser2 (proto:serialize-object-to-stream rqst2 'buy-car-request :stream nil)))
      (qtest:assert-true (string= (with-output-to-string (s)
                                    (proto:print-text-format rqst2 nil :stream s))
                                  (with-output-to-string (s)
                                    (proto:print-text-format
                                      (proto:deserialize-object 'buy-car-request ser2) nil :stream s)))))
    (qtest:assert-false (string= (with-output-to-string (s)
                                   (proto:print-text-format rqst1 nil :stream s))
                                 (with-output-to-string (s)
                                   (proto:print-text-format rqst2 nil :stream s))))))


;; Group example
;; Supply :name to keep the names stable for string= below
(proto:define-schema submessage-color-wheel
    (:package proto_test)
  (proto:define-message color-wheel1
      (:conc-name color-wheel- :name "ColorWheel")
    (proto:define-message metadata1     ;'metadata1' so we don't get class redefinition
        (:conc-name metadata- :name "Metadata")
      (author :type (or null string))
      (revision :type (or null string))
      (date :type (or null string)))
    (name :type string)
    (colors :type (list-of color1))
    (metadata :type (or null metadata1)))
  (proto:define-message color1
      (:conc-name color- :name "Color")
    (name :type (or null string))
    (r-value :type integer)
    (g-value :type integer)
    (b-value :type integer))
  (proto:define-message add-color1
      (:name "AddColor")
    (wheel :type color-wheel1)
    (color :type color1)))

;; Supply :name to keep the names stable for string= below
(proto:define-schema group-color-wheel
    (:package proto_test)
  (proto:define-message color-wheel2
      (:conc-name color-wheel- :name "ColorWheel")
    (name :type string)
    (colors :type (list-of color2))
    (proto:define-group metadata
        (:conc-name metadata-
         :index 3
         :arity :optional
         :name "Metadata")
      (author :type (or null string))
      (revision :type (or null string))
      (date :type (or null string))))
  (proto:define-message color2
      (:conc-name color- :name "Color")
    (name :type (or null string))
    (r-value :type integer)
    (g-value :type integer)
    (b-value :type integer))
  (proto:define-message add-color2 
      (:name "AddColor")
    (wheel :type color-wheel2)
    (color :type color2)))

(qtest:define-test group-serialization ()
  (let* ((meta1  (make-instance 'metadata1 :revision "1.0"))
         (wheel1 (make-instance 'color-wheel1 :name "Colors" :metadata meta1))
         (color1 (make-instance 'color1 :r-value 100 :g-value 0 :b-value 100))
         (rqst1  (make-instance 'add-color1 :wheel wheel1 :color color1))
         (meta2  (make-instance 'metadata :revision "1.0"))
         (wheel2 (make-instance 'color-wheel2 :name "Colors" :metadata meta2))
         (color2 (make-instance 'color2 :r-value 100 :g-value 0 :b-value 100))
         (rqst2  (make-instance 'add-color2 :wheel wheel2 :color color2)))
    (let ((ser1 (proto:serialize-object-to-stream rqst1 'add-color1 :stream nil))
          (ser2 (proto:serialize-object-to-stream rqst2 'add-color2 :stream nil)))
      (qtest:assert-true (string= (with-output-to-string (s)
                                    (proto:print-text-format rqst1 nil :stream s))
                                  (with-output-to-string (s)
                                    (proto:print-text-format rqst2 nil :stream s))))
      (qtest:assert-true (string= (with-output-to-string (s)
                                    (proto:print-text-format
                                      (proto:deserialize-object 'add-color1 ser1) nil :stream s))
                                  (with-output-to-string (s)
                                    (proto:print-text-format
                                      (proto:deserialize-object 'add-color2 ser2) nil :stream s)))))))


(qtest:define-test-suite serialization-tests ()
  (basic-serialization
   basic-optimized-serialization
   text-serialization
   serialization-integrity
   geodata-serialization
   geodata-optimized-serialization
   extension-serialization
   group-serialization))

(qtest:register-test 'serialization-tests)
