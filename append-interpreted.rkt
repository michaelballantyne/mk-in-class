#lang racket

(require "mk.rkt"
         "full-interp.rkt")

(run 1 (q)
  (evalo `(letrec ([append (lambda (l s)
                             (if (null? l)
                                 s
                                 (cons (car l) (append (cdr l) s))))])
            (append '(1 2 3) '(4 5 6)))
         q))