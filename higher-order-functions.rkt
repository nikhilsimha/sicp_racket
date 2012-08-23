#lang racket

(define (sum lower upper term next)
  (if (> lower upper)
      0
      (+ (term lower) (sum (next lower) upper term next))))

(define (cube x) (* x x x))
(define (inc x) (+ x 1))
(define (sum-cube lower upper)
  (sum lower upper cube inc))

(sum-cube 5 7)
(define (sumi a b term next)
  (define (sum-iter a result)
    (if (> a b)
        result
        (sum-iter (next a) (+ result (term a)))))
  (sum-iter a 0))

(define (integral f a b dx)
  (define (next x) (+ x dx))
  (* (sumi (+ a (/ dx 2)) b f next) dx))
(integral cube 0 1 .0001)





;integral using simpson's rule

(define (simpson f a b n)
  (define step (/ (- b a) n))
  (define (next x) (if (= x 4) 2 4))
  (define (sum x y mult)
    (if (< x y)      
        (+ (sum (+ x step) (- y step) (next mult)) (* mult (+ (f x) (f y)))) 0 ))
  (* (sum a b 1) (/ step 3))
  )

;(- .25 (simpson cube 0 1 100))

;iterative filter-accumulate
(define (accumulate operator null filter low up term next)
  (define (accumulate_ current result)
    (if (> current up)
        result
        (accumulate_ (next current) (if (filter current) (operator result (term current)) result))))
  (accumulate_ low null))

(define (isPrime x)
  (define (h_ low)
    (cond ((> (* low low) x) true)
          ((= (modulo x low) 0) false)
          (else (h_ (+ low 1)))))
  (h_ 2))

(isPrime (* 73 73))

(define (sumprimes x)
  (accumulate + 0 isPrime 2 x (lambda (x) (* x x)) (lambda (x) (+ x 1))))

(sumprimes 10)
                                                                     