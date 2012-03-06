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

#||
(setq cschema (proto:write-protobuf-schema-for-classes '(qres-core::legacy-pnr
                                                         qres-core::legacy-pnr-pax
                                                         qres-core::legacy-pnr-segment
                                                         qres-core::legacy-pnr-pax-segment)
                                                       :slot-filter #'quake::quake-slot-filter
                                                       :type-filter #'quake::quake-type-filter
                                                       :enum-filter #'quake::quake-enum-filter
                                                       :value-filter #'quake::quake-value-filter))

(proto:serialize-object-to-stream pnr cschema :stream nil)
||#

#||
(setq pschema (proto:write-protobuf-schema-for-classes '(proto:protobuf
                                                         proto:protobuf-message proto:protobuf-field
                                                         proto:protobuf-enum proto:protobuf-enum-value)))

(setq pser (proto:serialize-object-to-stream pschema pschema :stream nil))
(describe (proto:deserialize-object 'proto:protobuf pschema pser))
||#

#||
(defclass proto-test1 ()
  ((intval :type (integer -2147483648 +2147483647)
           :initarg :intval)))

(defclass proto-test2 ()
  ((intval :type (or null (integer -2147483648 +2147483647))
           :initarg :intval)
   (strval :type string
           :initarg :strval)))

(defclass proto-test3 ()
  ((intval :type (or null (integer -2147483648 +2147483647))
           :initarg :intval)
   (strval :type (or null string)
           :initarg :strval)
   (recval :type proto-test1
           :initarg :recval)))

(defclass proto-test4 ()
  ((intval :type (or null (integer -2147483648 +2147483647))
           :initarg :intval)
   (strval :type (or null string)
           :initarg :strval)
   (recval :type proto-test2
           :initarg :recval)))

(defclass proto-test5 ()
  ((color   :type (member :red :green :blue)
            :initarg :color)
   (intvals :type (list-of integer)
            :initform ()
            :initarg :intvals)
   (strvals :type (list-of string)
            :initform ()
            :initarg :strvals)))

(setq tschema (proto:write-protobuf-schema-for-classes
               '(proto-test1 proto-test2 proto-test3 proto-test4 proto-test5)))

(setq test1 (make-instance 'proto-test1 :intval 150))
(setq test2 (make-instance 'proto-test2 :strval "testing"))
(setq test3 (make-instance 'proto-test3 :recval test1))
(setq test4 (make-instance 'proto-test4 :recval test2))
(setq test5 (make-instance 'proto-test5 :color :red :intvals '(2 3 5 7) :strvals '("two" "three" "five" "seven")))

(setq tser1 (proto:serialize-object-to-stream test1 tschema :stream nil))
(equalp tser1 #(#x08 #x96 #x01))
(describe (proto:deserialize-object 'proto-test1 tschema tser1))

(setq tser2 (proto:serialize-object-to-stream test2 tschema :stream nil))
(equalp tser2 #(#x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67))
(describe (proto:deserialize-object 'proto-test2 tschema tser2))

(setq tser3 (proto:serialize-object-to-stream test3 tschema :stream nil))
(equalp tser3 #(#x1A #x03 #x08 #x96 #x01))
(describe (proto:deserialize-object 'proto-test3 tschema tser3))
(describe (slot-value (proto:deserialize-object 'proto-test3 tschema tser3) 'recval))

(setq tser4 (proto:serialize-object-to-stream test4 tschema :stream nil))
(equalp tser4 #(#x1A #x09 #x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67))
(describe (proto:deserialize-object 'proto-test4 tschema tser4))
(describe (slot-value (proto:deserialize-object 'proto-test4 tschema tser4) 'recval))

(setq tser5 (proto:serialize-object-to-stream test5 tschema :stream nil))
(equalp tser5 #(#x08 #x01
                #x10 #x04 #x02 #x03 #x05 #x07
                #x1A #x03 #x74 #x77 #x6F #x1A #x05 #x74 #x68 #x72 #x65 #x65 #x1A #x04 #x66 #x69 #x76 #x65 #x1A #x05 #x73 #x65 #x76 #x65 #x6E))
(describe (proto:deserialize-object 'proto-test5 tschema tser5))

(equalp (mapcar #'proto-impl:zig-zag-encode32
                '(0 -1 1 -2 2 -2147483648 2147483647))
        '(0 1 2 3 4 4294967295 4294967294))
(equalp (mapcar #'proto-impl:zig-zag-encode64
                '(0 -1 1 -2 2 -2147483648 2147483647 -1152921504606846976 1152921504606846975))
        '(0 1 2 3 4 4294967295 4294967294 2305843009213693951 2305843009213693950))

(proto:print-text-format test1 tschema)
(proto:print-text-format test2 tschema)
(proto:print-text-format test3 tschema)
(proto:print-text-format test4 tschema)
(proto:print-text-format test5 tschema)
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
                                      :type "Color"
                                      :required :required
                                      :index 1)
                                    (make-instance 'proto:protobuf-field
                                      :name "contrast"
                                      :type "Contrast"
                                      :required :optional
                                      :index 2
                                      :default "LOW")))))
       (rpcs  (list (make-instance 'proto:protobuf-rpc
                      :name "GetColor"
                      :input-type nil
                      :output-type "Color")
                    (make-instance 'proto:protobuf-rpc
                      :name "SetColor"
                      :input-type "Color"
                      :output-type "Color")))
       (svcs  (list (make-instance 'proto:protobuf-service
                      :name "ColorWheel"
                      :rpcs rpcs)))
       (proto (make-instance 'proto:protobuf
                :package "ita.color"
                :imports '("descriptor.proto")
                :enums enums
                :messages msgs
                :services svcs)))
  ;; The output should be example the same as the output of 'write-protobuf' below
  (proto:write-protobuf proto *standard-output*))
||#

#||
(makunbound '*color-wheel*)

(proto:define-proto color-wheel (:package ita.color
                                 :import "descriptor.proto")
  (proto:define-enum color-name ()
    red
    green
    blue)
  (proto:define-message color (:conc-name color-)
    (proto:define-enum contrast-name ()
      (low    1)
      (high 100))
    (color    :type color)
    (contrast :type (or null contrast) :default :low))
  (proto:define-service color-wheel ()
    (get-color nil color)
    (set-color color color)))

=> (PROGN
     (DEFTYPE COLOR-NAME () '(MEMBER :RED :GREEN :BLUE))
     (DEFTYPE CONTRAST-NAME () '(MEMBER :LOW :HIGH))
     (DEFCLASS COLOR ()
       ((COLOR :TYPE COLOR :ACCESSOR COLOR-COLOR :INITARG :COLOR)
        (CONTRAST :TYPE (OR NULL CONTRAST) :ACCESSOR COLOR-CONTRAST :INITARG :CONTRAST :INITFORM :LOW)))
     (DEFVAR *COLOR-WHEEL*
       (MAKE-INSTANCE 'PROTOBUF
         :NAME "ColorWheel"
         :PACKAGE "ita.color"
         :IMPORTS '("descriptor.proto")
         :SYNTAX NIL
         :OPTIONS 'NIL
         :ENUMS (LIST (MAKE-INSTANCE 'PROTOBUF-ENUM
                        :NAME "ColorName"
                        :CLASS 'COLOR-NAME
                        :VALUES (LIST (MAKE-INSTANCE 'PROTOBUF-ENUM-VALUE
                                        :NAME "RED"
                                        :INDEX 1
                                        :VALUE :RED)
                                      (MAKE-INSTANCE 'PROTOBUF-ENUM-VALUE
                                        :NAME "GREEN"
                                        :INDEX 2
                                        :VALUE :GREEN)
                                      (MAKE-INSTANCE 'PROTOBUF-ENUM-VALUE
                                        :NAME "BLUE"
                                        :INDEX 3
                                        :VALUE :BLUE))))
         :MESSAGES (LIST (MAKE-INSTANCE 'PROTOBUF-MESSAGE
                           :NAME "Color"
                           :CLASS 'COLOR
                           :ENUMS (LIST (MAKE-INSTANCE 'PROTOBUF-ENUM
                                          :NAME "ContrastName"
                                          :CLASS 'CONTRAST-NAME
                                          :VALUES (LIST (MAKE-INSTANCE 'PROTOBUF-ENUM-VALUE
                                                          :NAME "LOW"
                                                          :INDEX 1
                                                          :VALUE :LOW)
                                                        (MAKE-INSTANCE 'PROTOBUF-ENUM-VALUE
                                                          :NAME "HIGH"
                                                          :INDEX 100
                                                          :VALUE :HIGH))))
                           :MESSAGES (LIST)
                           :FIELDS (LIST (MAKE-INSTANCE 'PROTOBUF-FIELD
                                           :NAME "color"
                                           :TYPE "Color"
                                           :CLASS 'COLOR
                                           :REQUIRED :REQUIRED
                                           :INDEX 1
                                           :VALUE 'COLOR
                                           :DEFAULT NIL
                                           :PACKED NIL)
                                         (MAKE-INSTANCE 'PROTOBUF-FIELD
                                           :NAME "contrast"
                                           :TYPE "Contrast"
                                           :CLASS 'CONTRAST
                                           :REQUIRED :OPTIONAL
                                           :INDEX 2
                                           :VALUE 'CONTRAST
                                           :DEFAULT "LOW"
                                           :PACKED NIL))))
         :SERVICES (LIST (MAKE-INSTANCE 'PROTOBUF-SERVICE
                           :NAME "ColorWheel"
                           :RPCS (LIST (MAKE-INSTANCE 'PROTOBUF-RPC
                                         :NAME "GetColor"
                                         :INPUT-TYPE NIL
                                         :OUTPUT-TYPE "Color")
                                       (MAKE-INSTANCE 'PROTOBUF-RPC
                                         :NAME "SetColor"
                                         :INPUT-TYPE "Color"
                                         :OUTPUT-TYPE "Color")))))))

;; The output should be example the same as the output of 'write-protobuf' above
(proto:write-protobuf *color-wheel* *standard-output*)
||#
