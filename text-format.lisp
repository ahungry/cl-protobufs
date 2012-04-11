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


;;; Print objects using Protobufs text format

(defvar *suppress-line-breaks* nil
  "When true, don't generate line breaks in the text format")

(defgeneric print-text-format (object &optional type &key stream suppress-line-breaks)
  (:documentation
   "Prints the object 'object' of type 'type' onto the stream 'stream' using the
    textual format.
    If 'suppress-line-breaks' is true, all the output is put on a single line."))

(defmethod print-text-format (object &optional type
                              &key (stream *standard-output*)
                                   (suppress-line-breaks *suppress-line-breaks*))
  (let* ((type    (or type (class-of object)))
         (message (find-message-for-class type)))
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
    (macrolet ((read-slot (object slot reader)
                 ;; Don't do a boundp check, we assume the object is fully populated
                 ;; Unpopulated slots should be "nullable" and should contain nil
                 `(if ,reader
                    (funcall ,reader ,object)
                    (slot-value ,object ,slot))))
      (labels ((do-field (object trace indent field)
                 ;; We don't do cycle detection here
                 ;; If the client needs it, he can define his own 'print-text-format'
                 ;; method to clean things up first
                 (let* ((type   (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
                        (slot   (proto-value field))
                        (reader (proto-reader field))
                        msg)
                   (when (or slot reader)
                     (cond ((eq (proto-required field) :repeated)
                            (cond ((keywordp type)
                                   (map () #'(lambda (v)
                                               (print-prim v type field stream
                                                           (or suppress-line-breaks indent)))
                                           (read-slot object slot reader)))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type))))
                                          'protobuf-message)
                                   (let ((values (if slot (read-slot object slot reader) (list object))))
                                     (when values
                                       (let ((indent (+ indent 2)))
                                         (dolist (v values)
                                           (if suppress-line-breaks
                                             (format stream "~A: { " (proto-name field))
                                             (format stream "~&~VT~A: {~%" indent (proto-name field)))
                                           (map () (curry #'do-field v msg indent)
                                                   (proto-fields msg))
                                           (if suppress-line-breaks
                                             (format stream "} ")
                                             (format stream "~&~VT}~%" indent)))))))
                                  ((typep msg 'protobuf-enum)
                                   (map () #'(lambda (v)
                                               (print-enum v msg field stream
                                                           (or suppress-line-breaks indent)))
                                           (read-slot object slot reader)))))
                           (t
                            (cond ((keywordp type)
                                   (let ((v (read-slot object slot reader)))
                                     (when (or v (eq type :bool))
                                       (print-prim v type field stream
                                                   (or suppress-line-breaks indent)))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type))))
                                          'protobuf-message)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (when v
                                       (let ((indent (+ indent 2)))
                                         (if suppress-line-breaks
                                             (format stream "~A: { " (proto-name field))
                                             (format stream "~&~VT~A: {~%" indent (proto-name field)))
                                         (map () (curry #'do-field v msg indent)
                                                 (proto-fields msg))
                                         (if suppress-line-breaks
                                             (format stream "} ")
                                             (format stream "~&~VT}~%" indent))))))
                                  ((typep msg 'protobuf-enum)
                                   (let ((v (read-slot object slot reader)))
                                     (when v
                                       (print-enum v msg field stream
                                                   (or suppress-line-breaks indent))))))))))))
        (declare (dynamic-extent #'do-field))
        (if suppress-line-breaks
          (format stream "~A { " (proto-name message))
          (format stream "~&~A {~%" (proto-name message)))
        (map () (curry #'do-field object message 0) (proto-fields message))
        (if suppress-line-breaks
          (format stream "}")
          (format stream "~&}~%"))
        nil))))

(defun print-prim (val type field stream indent)
  (when val
    (if (eq indent 't)
      (format stream "~A: " (proto-name field))
      (format stream "~&~VT~A: " (+ indent 2) (proto-name field)))
    (ecase type
      ((:int32 :uint32 :int64 :uint64 :sint32 :sint64
        :fixed32 :sfixed32 :fixed64 :sfixed64)
       (format stream "~D" val))
      ((:string)
       (format stream "\"~A\"" val))
      ((:bytes)
       (format stream "~S" val))
      ((:bool)
       (format stream "~A" (if val "true" "false")))
      ((:float :double)
       (format stream "~D" val))
      ;; A few of our homegrown types
      ((:symbol)
       (let ((val (if (keywordp val)
                    (string val)
                    (format nil "~A:~A" (package-name (symbol-package val)) (symbol-name val)))))
         (format stream "\"~A\"" val)))
      ((:date :time :datetime :timestamp)
       (format stream "~D" val)))
    (if (eq indent 't)
      (format stream " ")
      (format stream "~%"))))

(defun print-enum (val enum field stream indent)
  (when val
    (if (eq indent 't)
      (format stream "~A: " (proto-name field))
      (format stream "~&~VT~A: " (+ indent 2) (proto-name field)))
    (let ((name (let ((e (find val (proto-values enum) :key #'proto-value)))
                  (and e (proto-name e)))))
      (format stream "~A" name)
      (if (eq indent 't)
        (format stream " ")
        (format stream "~%")))))


;;; Parse objects that were serialized using the text format

(defgeneric parse-text-format (type &key stream)
  (:documentation
   "Parses an object of type 'type' from the stream 'stream' using the textual format."))

(defmethod parse-text-format ((type symbol) &key (stream *standard-input*))
  (let ((message (find-message-for-class type)))
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
    (parse-text-format message :stream stream)))

(defmethod parse-text-format ((message protobuf-message) &key (stream *standard-input*))
  (let ((name (parse-token stream)))
    (assert (string= name (proto-name message)) ()
            "The message is not of the expected type ~A" (proto-name message)))
  (labels ((deserialize (type trace)
             (let* ((message (find-message trace type))
                    (object  (and message
                                  (make-instance (or (proto-alias-for message) (proto-class message)))))
                    (rslots ()))
               (expect-char stream #\{)
               (loop
                 (skip-whitespace stream)
                 (when (eql (peek-char nil stream nil) #\})
                   (read-char stream)
                   (dolist (slot rslots)
                     (setf (slot-value object slot) (nreverse (slot-value object slot))))
                   (return-from deserialize object))
                 (let* ((name  (prog1 (parse-token stream)
                                 (expect-char stream #\:)))
                        (field (and name (find name (proto-fields message) :key #'proto-name :test #'string=)))
                        (type  (and field (if (eq (proto-class field) 'boolean) :bool (proto-class field))))
                        (slot  (and field (proto-value field)))
                        msg)
                   (if (null field)
                     ;;---*** This needs to skip a token or a balanced {}-pair
                     (parse-token stream)
                     (cond ((and field (eq (proto-required field) :repeated))
                            (cond ((keywordp type)
                                   (let ((val (case type
                                                ((:float :double) (parse-float stream))
                                                ((:string) (parse-string stream))
                                                ((:bool)   (parse-token stream))
                                                (otherwise (parse-int stream)))))
                                     (when slot
                                       (pushnew slot rslots)
                                       (push val (slot-value object slot)))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type))))
                                          'protobuf-message)
                                     (let ((obj (deserialize type msg)))
                                       (when slot
                                         (pushnew slot rslots)
                                         (push obj (slot-value object slot)))))
                                  ((typep msg 'protobuf-enum)
                                   (let* ((name (parse-token stream))
                                          (enum (find name (proto-values msg) :key #'proto-name :test #'string=))
                                          (val  (and enum (proto-value enum))))
                                     (when slot
                                       (pushnew slot rslots)
                                       (push val (slot-value object slot)))))))
                           (t
                            (cond ((keywordp type)
                                   (let ((val (case type
                                                ((:float :double) (parse-float stream))
                                                ((:string) (parse-string stream))
                                                ((:bool)   (parse-token stream))
                                                (otherwise (parse-int stream)))))
                                     (when slot
                                       (setf (slot-value object slot) val))))
                                  ((typep (setq msg (and type (or (find-message trace type)
                                                                  (find-enum trace type))))
                                          'protobuf-message)
                                   (let ((obj (deserialize type msg)))
                                     (when slot
                                       (setf (slot-value object slot) obj))))
                                  ((typep msg 'protobuf-enum)
                                   (let* ((name (parse-token stream))
                                          (enum (find name (proto-values msg) :key #'proto-name :test #'string=))
                                          (val  (and enum (proto-value enum))))
                                     (when slot
                                       (setf (slot-value object slot) val)))))))))))))
    (declare (dynamic-extent #'deserialize))
    (deserialize (proto-class message) message)))
