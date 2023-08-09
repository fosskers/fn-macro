(defpackage fn-macro/tests
  (:use :cl :fn-macro :parachute))

(in-package :fn-macro/tests)

(define-test "Basic"
  (is = 1 1))
