#lang racket

(define (close-enough? a b tolerance) 
  (define (abs x) (if (> x 0) x (- x)))
  (> tolerance (abs (- b a))))
      

(define (search f neg-point pos-point)
  (let ((midpoint (/ (+ neg-point pos-point) 2)))
    (let ((val (f midpoint)))
      (if (close-enough? val 0 0.001)
          midpoint
          (if (< 0 (f midpoint))
              (search f neg-point midpoint)
              (search f midpoint pos-point))))))

(define (half-point f a b)
  (let ((fa (f a))
        (fb (f b)))
    (if (< 0 (* fa fb))
        (error "the given values are both of the same sign.. " fa fb)
        (if (> fa 0)
            (search f b a)
            (search f a b)))))

;testing
(half-point (lambda (x) (- (* x x) 9)) 5 2)

