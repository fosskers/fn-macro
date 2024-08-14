(defsystem "fn-macro"
  :version "1.0.1"
  :author "Colin Woodbury"
  :mailto "colin@fosskers.ca"
  :license "MPL-2.0"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "fn-macro"))))
  :description "A convenient lambda shorthand."
  :in-order-to ((test-op (test-op "fn-macro/tests"))))

(defsystem "fn-macro/tests"
  :depends-on ("fn-macro" "parachute")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for fn-macro"
  :perform (test-op (op c) (symbol-call :parachute :test :fn-macro/tests)))
