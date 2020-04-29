#lang simply-scheme

; SICP E1.3
(define (square x)
  (* x x))

(define (sqsum a b c)
  (if (> a b)
      (if (< c b)
          (+ (square a) (square b))
          (+ (square a) (square c)))
      (if (< c a)
          (+ (square b) (square a))
          (+ (square b) (square c)))))

; CS61A Q1/SICP E1.6

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) 
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

#|(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))|#

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

#|
Applicative order evaluation means that the arguments
are evaluated before the procedure is executed,
leading to an infinite loop.
|#

; CS61A Q2
(define (squares nums)
  (cond ((empty? nums) '())
        (else (sentence (square (first nums)) (squares (bf nums))))))

; CS61A Q3
(define (switch wds)
  (define wd (if (not (empty? wds)) (first wds) '()))
  (cond ((empty? wds) '())
        ((or (equal? wd 'I) (equal? wd 'i) (equal? wd 'me))
         (sentence 'you (switch (bf wds))))
        ((equal? wd 'you) (sentence '(me) (switch (bf wds))))
        ((equal? wd 'You) (sentence '(I) (switch (bf wds))))
        (else (sentence wd (switch (bf wds))))))

; CS61A Q4
(define (ordered? nums)
  (if (or (empty? nums) (empty? (bf nums)))
      #t
      (and (< (first nums) (first (bf nums))) (ordered? (bf (bf nums))))))

; CS61A Q5
(define (ends-e wds)
  (if (empty? wds)
      '()
      (sentence (if (equal? (last (first wds)) 'e) (first wds) '()) (ends-e (bf wds)))))