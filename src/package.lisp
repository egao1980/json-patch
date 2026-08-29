(defpackage #:json-patch
  (:use #:cl)
  (:nicknames #:stack-json-patch)
  (:documentation
   "RFC 6902 JSON Patch and RFC 6901 JSON Pointer over the JSON representation
    this stack uses everywhere: objects are EQUAL hash-tables, arrays are
    vectors, strings/numbers are themselves, and NIL covers both null and false
    (a yason property, not one this library introduces).

    Deliberately dependency-free. Not a -protocol system: there is nothing here
    to plug a backend into, only the algorithm.")
  (:export #:json-patch-error
           #:json-patch-error-message
           #:json-patch-error-operation
           #:json-pointer-error
           #:json-patch-test-failed
           ;; pointers
           #:parse-pointer
           #:unparse-pointer
           #:pointer-get
           #:pointer-exists-p
           ;; patch
           #:apply-patch
           #:apply-operation
           #:json-equal))

(in-package #:json-patch)
