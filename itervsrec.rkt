#lang racket
(define (f n)  
  (fh 2 2 1 0 n))
  
(define (fh curr v1 v2 v3 n)
  (cond ((< n 3) n)
        ((= curr n) v1)
        (else (fh (+ curr 1) (+ v1 (* 2 v2) (* 3 v3)) v1 v2 n))))

(f 30)

(define (g n)
  (cond ((< n 3) n)
        (else (+ (g (- n 1)) (* (g (- n 2)) 2) (* 3 (g (- n 3)))))))
(g 30)
