#lang racket

(require "mk.rkt")

(provide evalo)

(define evalo
  (lambda (expr val)
    (fresh ()
      (absento 'closure expr)
      (eval-expro expr '() val))))

(define eval-expro
  (lambda (expr env val)
    (conde
      [(numbero expr) (== expr val)]
      [(fresh (rator rand x body env^ a)
         (== `(,rator ,rand) expr)
         (eval-expro rator env `(closure ,x ,body ,env^))
         (eval-expro rand env a)
         (eval-expro body `((,x . ,a) . ,env^) val))]
      [(fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (== `(closure ,x ,body ,env) val)
         (not-in-envo 'lambda env))]
      [(symbolo expr) (lookupo expr env val)]
      [(fresh (e res d)
         (== `(car ,e) expr)
         (== `(,val . ,d) res)
         (not-in-envo 'car env)
         (eval-expro e env res))]
      [(fresh (e res a)
         (not-in-envo 'car env)
         (== `(cdr ,e) expr)
         (== `(,a . ,val) res)
         (eval-expro e env res))])))

(define not-in-envo
  (lambda (x env)
    (conde
      [(== '() env)]
      [(fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest))])))

(define lookupo
  (lambda (x env t)
    (conde
      [(fresh (y v rest)
         (== `((,y . ,v) . ,rest) env) (== y x)
         (== v t))]
      [(fresh (y v rest)
         (== `((,y . ,v) . ,rest) env) (=/= y x)
         (lookupo x rest t))])))

(run 1 (q)
  (evalo '(((lambda (x) (lambda (y) x)) 5) 6) q))