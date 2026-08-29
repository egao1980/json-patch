(defsystem "json-patch"
  :version "0.1.0"
  :description "RFC 6902 JSON Patch and RFC 6901 JSON Pointer"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "pointer")
               (:file "patch"))
  :in-order-to ((test-op (test-op "json-patch/tests"))))

(defsystem "json-patch/tests"
  :depends-on ("json-patch" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "patch-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
