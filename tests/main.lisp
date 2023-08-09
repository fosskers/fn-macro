(defpackage fn-macro/tests
  (:use :cl :fn-macro :parachute))

(in-package :fn-macro/tests)

(define-test "Basic"
  (is equal '(2 3 4) (mapcar (fn (+ % 1)) '(1 2 3))))

#+nil
(mapcar (fn (+ 1 %)) '(1 2 3))

#+nil
#'(lambda () (+ %1 (+ 2 %2)))