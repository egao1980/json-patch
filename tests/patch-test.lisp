(in-package #:json-patch/tests)

;;; Cases follow RFC 6902 Appendix A, which is the normative example set.

(defun o (&rest kvs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k h) v))
    h))

(defun a (&rest items)
  (make-array (length items) :adjustable t :fill-pointer (length items)
                             :initial-contents items))

(defun patch (document &rest operations)
  (json-patch:apply-patch document operations))

(defun at (document pointer)
  (json-patch:pointer-get document pointer))

;;; Pointers (RFC 6901)

(deftest pointer-parsing
  (ok (equal '() (json-patch:parse-pointer "")))
  (ok (equal '("foo") (json-patch:parse-pointer "/foo")))
  (ok (equal '("foo" "0") (json-patch:parse-pointer "/foo/0")))
  (ok (equal '("") (json-patch:parse-pointer "/")))
  ;; ~1 is a literal slash, ~0 a literal tilde; unescaping order matters so
  ;; that ~01 is "~1" and not "/".
  (ok (equal '("a/b") (json-patch:parse-pointer "/a~1b")))
  (ok (equal '("m~n") (json-patch:parse-pointer "/m~0n")))
  (ok (equal '("~1") (json-patch:parse-pointer "/~01")))
  (ok (signals (json-patch:parse-pointer "foo") 'json-patch:json-pointer-error)))

(deftest pointer-unparsing-round-trips
  (dolist (p '("" "/foo" "/foo/0" "/a~1b" "/m~0n" "/~01"))
    (ok (equal p (json-patch:unparse-pointer (json-patch:parse-pointer p))))))

(deftest pointer-get-and-exists
  (let ((doc (o "foo" (a "bar" "baz") "" 0 "a/b" 1 "m~n" 2)))
    (ok (json-patch:json-equal (a "bar" "baz") (at doc "/foo")))
    (ok (equal "bar" (at doc "/foo/0")))
    (ok (eql 0 (at doc "/")))
    (ok (eql 1 (at doc "/a~1b")))
    (ok (eql 2 (at doc "/m~0n")))
    (ok (json-patch:pointer-exists-p doc "/foo/1"))
    (ng (json-patch:pointer-exists-p doc "/foo/9"))
    (ng (json-patch:pointer-exists-p doc "/nope"))))

;;; Operations

(deftest add-object-member
  (let ((out (patch (o "foo" "bar") (o "op" "add" "path" "/baz" "value" "qux"))))
    (ok (equal "qux" (at out "/baz")))
    (ok (equal "bar" (at out "/foo")))))

(deftest add-array-element-inserts-rather-than-replaces
  (let ((out (patch (o "foo" (a "bar" "baz"))
                    (o "op" "add" "path" "/foo/1" "value" "qux"))))
    (ok (json-patch:json-equal (a "bar" "qux" "baz") (at out "/foo")))))

(deftest add-appends-with-dash
  (let ((out (patch (o "foo" (a 1 2))
                    (o "op" "add" "path" "/foo/-" "value" 3))))
    (ok (json-patch:json-equal (a 1 2 3) (at out "/foo")))))

(deftest add-to-nonexistent-target-fails
  (ok (signals (patch (o "foo" "bar")
                      (o "op" "add" "path" "/baz/bat" "value" "qux"))
               'json-patch:json-patch-error)))

(deftest add-at-root-replaces-the-document
  (let ((out (patch (o "a" 1) (o "op" "add" "path" "" "value" (o "b" 2)))))
    (ok (eql 2 (at out "/b")))
    (ng (json-patch:pointer-exists-p out "/a"))))

(deftest remove-object-member-and-array-element
  (let ((out (patch (o "baz" "qux" "foo" "bar") (o "op" "remove" "path" "/baz"))))
    (ng (json-patch:pointer-exists-p out "/baz"))
    (ok (equal "bar" (at out "/foo"))))
  (let ((out (patch (o "foo" (a "bar" "qux" "baz"))
                    (o "op" "remove" "path" "/foo/1"))))
    (ok (json-patch:json-equal (a "bar" "baz") (at out "/foo")))))

(deftest remove-missing-key-fails
  (ok (signals (patch (o "a" 1) (o "op" "remove" "path" "/nope"))
               'json-patch:json-patch-error)))

(deftest replace-requires-an-existing-target
  (let ((out (patch (o "baz" "qux" "foo" "bar")
                    (o "op" "replace" "path" "/baz" "value" "boo"))))
    (ok (equal "boo" (at out "/baz"))))
  (ok (signals (patch (o "a" 1) (o "op" "replace" "path" "/b" "value" 2))
               'json-patch:json-patch-error)))

(deftest move-value
  (let ((out (patch (o "foo" (o "bar" "baz" "waldo" "fred") "qux" (o "corge" "grault"))
                    (o "op" "move" "from" "/foo/waldo" "path" "/qux/thud"))))
    (ok (equal "fred" (at out "/qux/thud")))
    (ng (json-patch:pointer-exists-p out "/foo/waldo"))))

(deftest move-array-element
  (let ((out (patch (o "foo" (a "all" "grass" "cows" "eat"))
                    (o "op" "move" "from" "/foo/1" "path" "/foo/3"))))
    (ok (json-patch:json-equal (a "all" "cows" "eat" "grass") (at out "/foo")))))

(deftest move-into-own-child-fails
  (ok (signals (patch (o "a" (o "b" 1))
                      (o "op" "move" "from" "/a" "path" "/a/b/c"))
               'json-patch:json-patch-error)))

(deftest copy-value
  (let ((out (patch (o "foo" (a 1 2)) (o "op" "copy" "from" "/foo/0" "path" "/bar"))))
    (ok (eql 1 (at out "/bar")))
    (ok (json-patch:json-equal (a 1 2) (at out "/foo")))))

(deftest test-succeeds-and-fails
  (let ((doc (o "baz" "qux" "foo" (a "a" 2 "c"))))
    (ok (patch doc
               (o "op" "test" "path" "/baz" "value" "qux")
               (o "op" "test" "path" "/foo/1" "value" 2)))
    (ok (signals (patch doc (o "op" "test" "path" "/baz" "value" "bar"))
                 'json-patch:json-patch-test-failed))
    ;; A string and a number are never equal, even when they look alike.
    (ok (signals (patch (o "n" 10) (o "op" "test" "path" "/n" "value" "10"))
                 'json-patch:json-patch-test-failed))
    ;; Testing a location that does not exist is a failure, not an error class
    ;; callers have to distinguish.
    (ok (signals (patch doc (o "op" "test" "path" "/nope" "value" 1))
                 'json-patch:json-patch-test-failed))))

(deftest test-against-null-value
  ;; yason maps both JSON null and false to NIL, so this covers both spellings.
  (let ((doc (o "a" nil)))
    (ok (patch doc (o "op" "test" "path" "/a" "value" nil)))
    (ok (signals (patch doc (o "op" "test" "path" "/a" "value" 1))
                 'json-patch:json-patch-test-failed))))

(deftest add-with-explicit-null-value-is-not-a-missing-value
  (let ((out (patch (o "a" 1) (o "op" "add" "path" "/b" "value" nil))))
    (ok (json-patch:pointer-exists-p out "/b"))
    (ok (null (at out "/b"))))
  (ok (signals (patch (o "a" 1) (o "op" "add" "path" "/b"))
               'json-patch:json-patch-error)))

(deftest unknown-op-fails
  (ok (signals (patch (o "a" 1) (o "op" "frobnicate" "path" "/a" "value" 2))
               'json-patch:json-patch-error)))

;;; Whole-patch behaviour

(deftest patch-is-non-destructive
  (let* ((nested (o "b" 1))
         (doc (o "a" nested "list" (a 1 2)))
         (out (patch doc
                     (o "op" "replace" "path" "/a/b" "value" 99)
                     (o "op" "add" "path" "/list/-" "value" 3))))
    (ok (eql 99 (at out "/a/b")))
    (ok (json-patch:json-equal (a 1 2 3) (at out "/list")))
    ;; The document handed in is untouched, so a consumer still rendering the
    ;; previous state keeps seeing it.
    (ok (eql 1 (at doc "/a/b")))
    (ok (eql 1 (gethash "b" nested)))
    (ok (json-patch:json-equal (a 1 2) (at doc "/list")))))

(deftest failing-operation-leaves-the-input-alone
  (let ((doc (o "a" 1)))
    (ok (signals (patch doc
                        (o "op" "add" "path" "/b" "value" 2)
                        (o "op" "test" "path" "/a" "value" 999))
                 'json-patch:json-patch-test-failed))
    (ok (eql 1 (at doc "/a")))
    (ng (json-patch:pointer-exists-p doc "/b"))))

(deftest operations-apply-in-order
  (let ((out (patch (o "n" 1)
                    (o "op" "replace" "path" "/n" "value" 2)
                    (o "op" "test" "path" "/n" "value" 2)
                    (o "op" "replace" "path" "/n" "value" 3))))
    (ok (eql 3 (at out "/n")))))

(deftest patch-accepts-a-vector-of-operations
  (let ((out (json-patch:apply-patch
              (o "n" 1)
              (a (o "op" "replace" "path" "/n" "value" 2)))))
    (ok (eql 2 (at out "/n")))))

(deftest json-equal-structural
  (ok (json-patch:json-equal (o "a" (a 1 (o "b" 2))) (o "a" (a 1 (o "b" 2)))))
  (ng (json-patch:json-equal (o "a" 1) (o "a" 1 "b" 2)))
  (ng (json-patch:json-equal (a 1 2) (a 2 1)))
  (ok (json-patch:json-equal 1 1.0))
  (ng (json-patch:json-equal "1" 1)))
