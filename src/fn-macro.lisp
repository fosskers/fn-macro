(defpackage fn-macro
  (:use :cl)
  (:export #:fn))

(in-package :fn-macro)

(defmacro fn (&rest args)
  "Return an lambda with implicit, positional arguments.

The function's arguments are determined recursively from ARGS.  Each symbol from
`%1' through `%9' that appears in ARGS is treated as a positional argument.
Missing arguments are named `_%N', which keeps the byte-compiler quiet.  `%' is
a shorthand for `%1'; only one of these can appear in ARGS.  `%*' represents
extra `&rest' arguments.

Instead of:

  (lambda (a _ c &rest d)
    (if a c (cadr d)))

you can use this macro and write:

  (fn (if %1 %3 (cadr %*)))

which expands to:

  (lambda (%1 _%2 %3 &rest %*)
    (if %1 %3 (cadr %*)))

This macro was borrowed from Doom Emacs, which itself was adapted from
llama.el (see https://git.sr.ht/~tarsius/llama)."
  `(lambda ,(let ((argv (make-array '(10) :initial-element nil)))
              (fn-crawl args argv)
              `(,@(let ((n -1)
                        sym arglist)
                    (loop for i from (1- (length argv)) above 0
                          do (progn (setq sym (aref argv i))
                                    (unless (and (= n -1) (null sym))
                                      (incf n)
                                      (push (or sym (intern (format nil "_%~a" i)))
                                            arglist))))
                    arglist)
                ,@(and (aref argv 0) '(&rest %*))))
     ,@args))

#+nil
(funcall (fn (+ %1 %2)) 1 2)
#+nil
(fn (+ %1 (+ 2 %2)))
#+nil
(fn (+ % (+ 2 %)))

#+nil
(symbol-plist 'fn-crawl)

(setf (symbol-plist 'fn-crawl) '(%2 2 %3 3 %4 4 %5 5 %6 6 %7 7 %8 8 %9 9))
(defun fn-crawl (data args)
  (cond ((symbolp data)
         (let ((pos (cond ((eq data '%*) 0)
                          ((member data '(% %1) :test #'eq) 1)
                          (t (get 'fn-crawl data)))))
           (when pos
             (when (and (= pos 1)
                        (aref args 1)
                        (not (eq data (aref args 1))))
               (error "% and %1 are mutually exclusive"))
             (setf (aref args pos) data))))
        ((and (not (eq (car-safe data) 'fn))
              (or (listp data)
                  (vectorp data)))
         (loop for i from 0 below (length data)
               do (fn-crawl (elt data i) args)))))

(defun car-safe (object)
  "Return the car of OBJECT if it is a cons cell, or else nil.
Borrowed from Emacs Lisp."
  (when (listp object)
    (car object)))
