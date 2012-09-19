;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-TEST")


;;; (De)serializing of 1 meg of geodata for metering purposes

(defvar *geowd* #.(make-pathname
                   :directory (pathname-directory
                               (or *compile-file-truename* *load-truename*))))

(defvar *geo-file-name* (merge-pathnames "geodata.data" *geowd*))

(defun deserialize-geo-file ()
  (proto:deserialize-object-from-file 'geodata::geodata *geo-file-name*))

(defun serialize-geo-data (geodata)
  (proto:serialize-object-to-bytes geodata 'geodata::geodata))

(defun deserialize-geo-data (bytes)
  (proto:deserialize-object 'geodata bytes))

#||
;; How long does it take to load (deserialize) the data, unoptimized?
;; On my 2011-ish Linux desktop,
;;  - about 10-11 Mb/sec using SBCL
;;  - about  3- 4 Mb/sec using CCL
(time (setq *geo* (deserialize-geo-file)))

;; How long does it take to serialize, unoptimized?
;; On my 2011-ish Linux desktop,
;;  - about 7-8 Mb/sec using SBCL
;;  - about 5-6 Mb/sec using CCL
(time (progn (serialize-geo-data *geo*) nil))

;; Optimize the (de)serializers
(dolist (class '(geodata:country
                 geodata:region
                 geodata:region-key
                 geodata:city
                 geodata:airport
                 geodata:timezone
                 geodata:tz-variation
                 geodata:currency
                 geodata:country-currencies
                 geodata:carrier
                 geodata:geodata geodata:geodata-v))
  (let ((message (proto:find-message geodata::*geodata* class)))
    (eval (proto-impl:generate-object-size  message))
    (eval (proto-impl:generate-serializer   message))
    (eval (proto-impl:generate-deserializer message))))

;; Now how long does it take to load (deserialize) the data, optimized?
;; On my 2011-ish Linux desktop,
;;  - about 20-25 Mb/sec using SBCL
;;  - about  5- 6 Mb/sec using CCL
(time (setq *geo* (deserialize-geo-file)))

;; How long does it take to serialize, optimized?
;; On my 2011-ish Linux desktop,
;;  - about 20-25 Mb/sec using SBCL
;;  - about  7- 8 Mb/sec using CCL
(time (progn (serialize-geo-data *geo*) nil))
||#
