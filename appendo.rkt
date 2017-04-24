#lang racket

(require "mk.rkt")

(provide appendo)


(define appendo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))])))

(run 1 (q)
  (appendo '(1 2 3) '(4 5 6) q))