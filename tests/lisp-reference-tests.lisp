;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Ben Wagner                                      ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PROTO-TEST")

(define-test cross-package-reference-test ()
  (flet ((find-by-name (name proto-objects)
           (find name proto-objects :key #'proto-name :test #'string=)))
    (let* ((schema (find-schema :package_test1))
           (message-with-cross-package-reference
            (find-by-name "MessageWithCrossPackageReference" (proto-messages schema)))
           (baz (find-by-name "baz" (proto-fields message-with-cross-package-reference)))
           (bonk (find-by-name "bonk" (proto-fields message-with-cross-package-reference)))
           (bam (find-by-name "bam" (proto-fields message-with-cross-package-reference)))
           (bing (find-by-name "bing" (proto-fields message-with-cross-package-reference)))
           (message-with-cross-package-extension
            (find-by-name "MessageWithCrossPackageExtension" (proto-messages schema)))
           (boo (find-by-name "boo" (proto-fields message-with-cross-package-extension)))
           (service-with-cross-package-input-output
            (find-by-name "ServiceWithCrossPackageInputOutput" (proto-services schema)))
           (bloop (find-by-name "Bloop" (proto-methods service-with-cross-package-input-output)))
           (beep (find-by-name "Beep" (proto-methods service-with-cross-package-input-output)))
           (message-in-other-package-extend
            (find-by-name "MessageInOtherPackage"
                          (proto-messages message-with-cross-package-extension)))
           (baa (find-by-name "baa" (proto-extended-fields message-in-other-package-extend))))
      (assert-equal 'protobuf-package-unittest2::message-in-other-package
                    (proto-class baz))
      (assert-equal 'protobuf-package-unittest2::enum-in-other-package
                    (proto-class bonk))
      (assert-equal 'protobuf-package-unittest1::message-defined-in-both-packages
                    (proto-class bam))
      (assert-equal 'protobuf-package-unittest2::message-defined-in-both-packages
                    (proto-class bing))
      (assert-equal 'protobuf-package-unittest2::message-in-other-package
                    (proto-class boo))
      (assert-equal 'protobuf-package-unittest2::message-in-other-package
                    (proto-input-type bloop))
      (assert-equal 'protobuf-package-unittest1::message-with-cross-package-reference
                    (proto-output-type bloop))
      (assert-equal 'protobuf-package-unittest1::message-with-cross-package-reference
                    (proto-input-type beep))
      (assert-equal 'protobuf-package-unittest2::message-in-other-package
                    (proto-output-type beep))
      (assert-equal 'protobuf-package-unittest1::baa
                    (proto-value baa))))

  (let* ((orig1 (make-instance 'protobuf-package-unittest1::message-with-cross-package-reference
                  :baz (make-instance 'protobuf-package-unittest2::message-in-other-package
                         :foo 123)
                  :bonk :bar
                  :bam (make-instance 'protobuf-package-unittest1::message-defined-in-both-packages
                         :boom "bomb")
                  :bing (make-instance 'protobuf-package-unittest2::message-defined-in-both-packages
                          :bang "gun")))
         (orig2 (let ((extended-obj (make-instance 'protobuf-package-unittest2::message-in-other-package
                                      :foo 123)))
                  (setf (protobuf-package-unittest1::baa extended-obj) 456)
                  (make-instance 'protobuf-package-unittest1::message-with-cross-package-extension
                    :boo extended-obj)))
         (bytes1 (serialize-object-to-bytes orig1
                                            'protobuf-package-unittest1::message-with-cross-package-reference))
         (bytes2 (serialize-object-to-bytes orig2
                                            'protobuf-package-unittest1::message-with-cross-package-extension))
         (new1 (deserialize-object 'protobuf-package-unittest1::message-with-cross-package-reference
                                   bytes1))
         (new2 (deserialize-object 'protobuf-package-unittest1::message-with-cross-package-extension
                                   bytes2)))
    (assert-true (typep (protobuf-package-unittest1::baz new1)
                        'protobuf-package-unittest2::message-in-other-package))
    (assert-equal 123
                  (protobuf-package-unittest2::foo (protobuf-package-unittest1::baz new1)))
    (assert-equal :bar
                  (protobuf-package-unittest1::bonk new1))
    (assert-equal "bomb"
                  (protobuf-package-unittest1::boom (protobuf-package-unittest1::bam new1)))
    (assert-equal "gun"
                  (protobuf-package-unittest2::bang (protobuf-package-unittest1::bing new1)))
    (assert-true (typep (protobuf-package-unittest1::boo new2)
                        'protobuf-package-unittest2::message-in-other-package))
    (assert-equal 123
                  (protobuf-package-unittest2::foo (protobuf-package-unittest1::boo new2)))
    (assert-equal 456
                  (protobuf-package-unittest1::baa (protobuf-package-unittest1::boo new2)))))

(define-test forward-reference-test ()
  (flet ((find-by-name (name proto-objects)
           (find name proto-objects :key #'proto-name :test #'string=)))
    (let* ((schema (find-schema :forward_reference))
           (message-with-forward-reference
            (find-by-name "MessageWithForwardReference" (proto-messages schema)))
           (foo (find-by-name "foo" (proto-fields message-with-forward-reference)))
           (bar (find-by-name "bar" (proto-fields message-with-forward-reference)))
           (service-with-forward-reference
            (find-by-name "ServiceWithForwardReference" (proto-services schema)))
           (bloop (find-by-name "Bloop" (proto-methods service-with-forward-reference)))
           (beep (find-by-name "Beep" (proto-methods service-with-forward-reference))))
      (assert-equal 'protobuf-forward-reference-unittest::msg-w-overridden-lisp-class
                    (proto-class foo))
      (assert-equal 'protobuf-forward-reference-unittest::ENUM-W-OVERRIDDEN-LISP-CLASS
                    (proto-class bar))
      (assert-equal 'protobuf-forward-reference-unittest::MSG-W-OVERRIDDEN-LISP-CLASS
                    (proto-input-type bloop))
      (assert-equal 'protobuf-forward-reference-unittest::MESSAGE-WITH-FORWARD-REFERENCE
                    (proto-output-type bloop))
      (assert-equal 'protobuf-forward-reference-unittest::MESSAGE-WITH-FORWARD-REFERENCE
                    (proto-input-type beep))
      (assert-equal 'protobuf-forward-reference-unittest::MSG-W-OVERRIDDEN-LISP-CLASS
                    (proto-output-type beep))))
  (let* ((orig (make-instance 'protobuf-forward-reference-unittest::message-with-forward-reference
                 :foo (make-instance 'protobuf-forward-reference-unittest::msg-w-overridden-lisp-class
                        :baz 123)
                 :bar :baa))
         (bytes (serialize-object-to-bytes orig
                                           'protobuf-forward-reference-unittest::message-with-forward-reference))
         (new (deserialize-object 'protobuf-forward-reference-unittest::message-with-forward-reference
                                   bytes)))
    (assert-true (typep (protobuf-forward-reference-unittest::foo new)
                        'protobuf-forward-reference-unittest::msg-w-overridden-lisp-class))
    (assert-equal 123
                  (protobuf-forward-reference-unittest::baz (protobuf-forward-reference-unittest::foo new)))
    (assert-equal :baa
                  (protobuf-forward-reference-unittest::bar new))))

(defparameter *test-proto-preamble*
  "syntax = \"proto2\";

package proto_test;

message DefinedMessage {
  optional string foo = 1;
}

")


(define-test undefined-types-test ()
  (labels ((parse-schema-containing (string)
             (with-input-from-string (s (concatenate 'string *test-proto-preamble* string))
               (parse-schema-from-stream s
                                         ;; Parsing from a string doesn't produce a name, so supply
                                         ;; it
                                         :name "proto_test"
                                         :class 'dummy
                                         :conc-name nil)))
           (parse-message-with-field-type (type)
             (parse-schema-containing (format nil "message MessageWithUndefinedFieldType {~%~
                                                   ~&  optional ~a bar = 1;~%~
                                                   }~%" type)))
           (parse-service-with-rpc (rpc)
             (parse-schema-containing (format nil "service ServiceWithUndefinedMethodType {~%~
                                                   ~&  ~a~%~
                                                   }~%" rpc)))
           (poor-mans-assert-regex-equal (expected-strings actual-string)
             (assert-true
              (loop with index = 0
                    for expected-string in expected-strings
                    as position = (search expected-string actual-string :start2 index)
                    always position
                    do (setf index (+ position (length expected-string))))))
           (do-field-test (field-type)
             (let ((condition (assert-error undefined-field-type
                                (parse-message-with-field-type field-type))))
               (poor-mans-assert-regex-equal
                (list "Undefined type: Field #<"
                      "PROTOBUF-FIELD PROTOBUFS-TEST::BAR :: NIL = 1"
                      "in message #<"
                      "PROTOBUF-MESSAGE PROTOBUFS-TEST::MESSAGE-WITH-UNDEFINED-FIELD-TYPE"
                      (format nil "has unknown type \"~a\"." field-type))
                (princ-to-string condition))
               (assert-equal field-type (error-type-name condition))
               (assert-equal "bar" (proto-name (error-field condition)))))
           (method-test-assertions (condition where method-lisp-name method-proto-name type)
             (poor-mans-assert-regex-equal
              (list (format nil "Undefined type: ~a type for rpc #<" where)
                    (format nil "PROTOBUF-METHOD PROTOBUFS-TEST::~a" method-lisp-name)
                    "in service #<"
                    "PROTOBUF-SERVICE ServiceWithUndefinedMethodType"
                    (format nil "has unknown type \"~a\"." type))
              (princ-to-string condition))
             (assert-equal type (error-type-name condition))
             (assert-equal method-proto-name (proto-name (error-method condition))))
           (do-method-input-test (input-type)
             (let ((condition (assert-error undefined-input-type
                                (parse-service-with-rpc
                                 (format nil "rpc MethodWithUndefinedInput (~a) ~
                                              returns (DefinedMessage);" input-type)))))
               (method-test-assertions condition "Input" "METHOD-WITH-UNDEFINED-INPUT"
                                       "MethodWithUndefinedInput" input-type)))
           (do-method-output-test (output-type)
             (let ((condition (assert-error undefined-output-type
                                (parse-service-with-rpc
                                 (format nil "rpc MethodWithUndefinedOutput (DefinedMessage) ~
                                              returns (~a);" output-type)))))
               (method-test-assertions condition "Output" "METHOD-WITH-UNDEFINED-OUTPUT"
                                       "MethodWithUndefinedOutput" output-type)))
           (do-method-stream-test (stream-type)
             (let ((condition (assert-error undefined-stream-type
                                (parse-service-with-rpc
                                 (format nil "rpc MethodWithUndefinedStream (DefinedMessage) ~
                                              returns (DefinedMessage) {~
                                              ~&    option stream_type = \"~a\";~
                                              ~&  };" stream-type)))))
               (method-test-assertions condition "Stream" "METHOD-WITH-UNDEFINED-STREAM"
                                       "MethodWithUndefinedStream" stream-type))))

    (parse-message-with-field-type "int32")
    (do-field-test "int")
    (parse-message-with-field-type "DefinedMessage")
    (do-field-test "UndefinedMessage")
    (do-field-test "other_package.DefinedMessage")

    (parse-service-with-rpc
     "rpc MethodWithDefinedInputOutput (DefinedMessage) returns (DefinedMessage);")
    (do-method-input-test "UndefinedMessage")
    ;; my understanding is that primitive types are not allowed for method input/output; if this is
    ;; incorrect, change to "int"
    (do-method-input-test "int32")
    (do-method-input-test "other_package.DefinedMessage")

    (do-method-output-test "UndefinedMessage")
    (do-method-output-test "int32")
    (do-method-output-test "other_package.DefinedMessage")

    ;; stream_type is required to be fully qualified
    (parse-service-with-rpc (format nil "rpc MethodWithDefinedInputOutput (DefinedMessage) ~
                                         returns (DefinedMessage) {~
                                         ~&    option stream_type = \"proto_test.DefinedMessage\";~
                                         ~&  };"))
    (do-method-stream-test "proto_test.UndefinedMessage")
    (do-method-stream-test "int32")
    (do-method-stream-test "other_package.DefinedMessage")))


(define-test-suite lisp-reference-tests ()
  (cross-package-reference-test
   forward-reference-test
   undefined-types-test))

(register-test 'lisp-reference-tests)
