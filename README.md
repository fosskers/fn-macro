# `fn-macro`

This small library provides a handy macro, `fn`, for writing short-hand `lambda`s.

Instead of:

```common-lisp
(lambda (a _ c &rest d)
  (if a c (cadr d)))
```

you can use `fn` and write:

```common-lisp
(fn (if %1 %3 (cadr %*)))
```

which expands to:

```common-lisp
(lambda (%1 _%2 %3 &rest %*)
  (if %1 %3 (cadr %*)))
```

If there is only one argument, you can just use `%` without a number:

```common-lisp
(mapcar (fn (* (car %) (cdr %))) '((1 . 2) (3 . 4) (5 . 6)))
;; => (2 12 30)
```

As hinted from the above, the `%N` arguments respect nesting, can be placed in
any order, and can appear multiple times.

`fn` was borrowed and CL-ized from `fn!` in Doom Emacs, which itself was adapted
from [llama.el](https://git.sr.ht/~tarsius/llama).
