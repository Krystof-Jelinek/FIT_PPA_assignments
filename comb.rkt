#lang racket

(define (comb lst n)
  (if (= n 0)
   1
  (if (null? lst)
      0
      (+ (comb (cdr lst) (- n (car lst)))
         (comb (cdr lst) n)))))
