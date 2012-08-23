#lang racket

(define (sq x) (* x x))

(define (msq_sum x y z)
  (cond ((and (>= x y) (>= z y)) (+ (sq x) (sq z)))
         ((and (>= y x) (>= z x)) (+ (sq z) (sq y)))
         ((and (>= x z) (>= y z)) (+ (sq x) (sq y))) 
   )
)

(msq_sum 7 6  6)

