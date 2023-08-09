(defpackage fn-macro/tests
  (:use :cl :fn-macro :parachute))

(in-package :fn-macro/tests)

(define-test "Basic"
  (is equal '(2 3 4) (mapcar (fn (+ % 1)) '(1 2 3)))
  (is = 3 (funcall (fn (+ % % %)) 1))
  (is = 9 (funcall (fn (+ %1 %2 %3 %4 %5 %6 %7 %8 %9))
                   1 1 1 1 1 1 1 1 1)))
