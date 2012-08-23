#lang racket
;fixed point method
;should be convergent

(define (close-enough? v1 v2 tolerance)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)  
  (define (try-guess guess)
    (display guess)
    (newline)
      (let ((next (f guess)))
      (let ((avg (/ (+ next guess) 2))) ;;removes damping and converges faster
        (if (close-enough? next guess 0.001)
            guess
            (fixed-point f avg)))))
    (try-guess first-guess))

(- (cos (fixed-point cos 1.0)) (fixed-point cos 1.0))

(define (f x) (+ 1 (/ 1 x)))

(define (avg-damp f)
  (define (form x)
    (/ (+ x (f x)) 2))
  form)

(define dx .0001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))

((deriv cube) 5)
;(define (f x) (* x x))

(define (newton f)
  (fixed-point 
   (lambda (x) (- x (/ (f x) ((deriv f) x)))) 1))
;(newton f)
;(fixed-point f 1)

(define (g x) (/ (log 1000) (log x)))

(fixed-point g 5)

(define (make-form n d)
  (define (form term) 
    (/ n (+ d term)))
  form)

(define (continued-fraction form begin)
  (define (h curr)
    (if (close-enough? curr (form curr) 0.001)
        curr
        (h (form curr))))
  (h begin))

(define (cont-frac n d k)
  (if (= k 0)
      (/ n d)
      (/ n (+ d (cont-frac n d (- k 1))))))

;(define (icont-frac n get-d k)
 ; (define (h temp state)    
  ;  (if (> state k) temp
   ;     (h (/ n (+ d temp)) (+ state 1))))
  ;(h (/ n d) 0))

                     
(/ 1 (continued-fraction (make-form 1 1) 1))


(define (compose f g) 
  (lambda (x) (f (g x))))

(define (refunc f n)
  (define (identity x) x)
  (define (aux resfunc temp)
    (if (> temp  n)
        resfunc
        (aux (lambda (x) (f (resfunc x))) (+ 1 temp))))
  (aux identity 1))

(define square (lambda (x) (* x x)))

((refunc square 2) 2)
  
        