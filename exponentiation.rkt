#lang racket
;fast exponentiation

(define (exp b n) (exphl 1 b n))

(define (exph a rem val cur pow)
  (if (= rem 0) 
      val
      (if (> pow rem)
          (exph a rem val a 1)
          (exph a (- rem pow) (* val cur) (* cur cur) (* 2 pow)))))
     
(define (exphl a b n) 
  (if (= n 0) a
      (if (= (modulo n 2) 1) 
          (exphl (* a b) b (- n 1))
          (exphl a (* b b) (/ n 2)))))
(exp 2 10)

