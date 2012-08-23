#lang racket

(define (abs x) (if (> x 0) x (- x)))

(define (square x) (* x x))
(define (pow x y) (if (> y 0) (* (pow x (- y 1)) x) 1))


(define (good-enough? guess x index)
   (< (abs (- (pow guess index) x)) 0.00001)
)

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) (average guess (/ x guess)))
(define (improve3 guess x) (/ (+ (/ x 
                                    (* guess guess))
                                 (* 2 guess))
                              3))
                              
(define (sqrt x)
  (sqrt-iter 1 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x 2) 
      guess
      (sqrt-iter (improve guess x) x)
  )
)

(define (cube-root x)
  (cube-root-iter 1 x)
 )

(define (cube-root-iter guess x)
  (if (good-enough? guess x 3) 
      guess 
      (cube-root-iter (improve3 guess x) x)
      )
  )
      
(pow (cube-root 34) 3)
(sqrt 101)

(pow 3 3)