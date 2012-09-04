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


;;; .lisp <-> .proto stability unit tests

(proto:define-schema stable-color-wheel
    (:package proto_test)
  (proto:define-message stable-color-wheel ()
    (proto:define-enum version-status ()
      deprecated
      unready)
    (proto:define-message stable-metadata ()
      (author :type (or null string))
      (revision :type (or null string))
      (date :type (or null string))
      (status :type version-status :default :unready))
    (name :type string)
    (colors :type (list-of stable-color))
    (metadata1 :type (or null stable-metadata)))
  (proto:define-message stable-color ()
    (name :type (or null string) :default "black")
    (r-value :type integer :default 0)
    (g-value :type integer :default 0)
    (b-value :type integer :default 0))
  (proto:define-message stable-add-color ()
    (wheel :type stable-color-wheel)
    (color :type stable-color))
  (proto:define-service stable-color-wheel ()
    (get-stable-color (string => stable-color))
    (set-stable-color (stable-color => stable-color)
                      :options (:deadline 1.0))))

(defvar *color-wheel-proto*
  "syntax = \"proto2\";

package proto_test;

message StableColorWheel {
  enum VersionStatus {
    DEPRECATED = 0;
    UNREADY = 1;
  }
  message StableMetadata {
    optional string author = 1;
    optional string revision = 2;
    optional string date = 3;
    required VersionStatus status = 4 [default = UNREADY];
  }
  required string name = 1;
  repeated StableColor colors = 2;
  optional StableMetadata metadata1 = 3;
}

message StableColor {
  optional string name = 1 [default = \"black\"];
  required int64 r_value = 2 [default = 0];
  required int64 g_value = 3 [default = 0];
  required int64 b_value = 4 [default = 0];
}

message StableAddColor {
  required StableColorWheel wheel = 1;
  required StableColor color = 2;
}

service StableColorWheel {
  rpc GetStableColor (String) returns (StableColor);
  rpc SetStableColor (StableColor) returns (StableColor) {
    option deadline = 1.0;
  }
}")

(define-test color-wheel-stability ()
  (let* ((schema1 (find-schema 'stable-color-wheel))
         (schema2 (with-input-from-string (s *color-wheel-proto*)
                    (parse-schema-from-stream s
                      ;; Parsing from a string doesn't produce a name, so supply it
                      :name (proto-name schema1)
                      :class (proto-class schema1)
                      :conc-name nil))))
    (assert-true (schemas-equal schema1 schema2))
    (assert-true (string=
                   (with-output-to-string (s)
                     (write-schema schema1 :type :proto :stream s))
                   (with-output-to-string (s)
                     (write-schema schema2 :type :proto :stream s))))
    (assert-true (string=
                   (with-output-to-string (s)
                     (write-schema schema1 :type :lisp :stream s))
                   (with-output-to-string (s)
                     (write-schema schema2 :type :lisp :stream s))))))

(define-test-suite stability-tests ()
  (color-wheel-stability))

(register-test 'stability-tests)
