(defsystem "fn-macro"
  :version "0.1.0"
  :author "Colin Woodbury"
  :mailto "colin@fosskers.ca"
  :license "AGPL-3.0-only"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "fn-macro"))))
  :description ""
  :in-order-to ((test-op (test-op "fn-macro/tests"))))

(defsystem "fn-macro/tests"
  :author "Colin Woodbury"
  :license "AGPL-3.0-only"
  :depends-on ("fn-macro" "parachute")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for fn-macro"
  :perform (test-op (op c) (symbol-call :parachute :test :fn-macro/tests)))
