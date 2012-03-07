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


;;; .proto file parsing

(defun parse-protobuf-from-file (filename)
  "Parses the named file as a .proto file, and returns the protobufs schema."
  (with-open-file (stream filename
                   :direction :input
                   :external-format :utf-8
                   :element-type 'character)
    (parse-protobuf-from-stream stream)))

(defun parse-protobuf-from-stream (stream)
  "Parses the contents of the stream as a .proto file, and returns the protobufs schema."
  STREAM                                        ;---*** DO THIS
  )
