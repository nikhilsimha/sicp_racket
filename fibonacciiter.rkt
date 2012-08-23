#lang racket

(define (fib n)
  (fib-iter 0 1 1 0 n))
(define (fib-iter p q a b n)
  (if (= n 0) 
      (+ (*  b p) (* a q))
      (if (= (modulo n 2) 1)
          (fib-iter p q (+ a b) a (- n 1))
          (fib-iter (+ (* p p) (* q q)) (+ (* 2 p q) (* q q)) a b (/ n 2)))))
(fib 4)