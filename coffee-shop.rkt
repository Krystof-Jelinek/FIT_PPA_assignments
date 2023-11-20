#lang racket

(define (count-one-time time all_orders)
  (define counter 0)
  (count-one-helper time all_orders counter)
  )

(define (my-max first second)
  (if (> first second)
      first
      second
      )
  )

(define (count-one-helper time all_orders counter)
  (if (null? all_orders)
      counter
      (if (equal? time (car all_orders))
          (count-one-helper time (cdr all_orders) (+ counter 1))
          (count-one-helper time (cdr all_orders) counter))))

(define (coffe-shop-helper orders)
  (if (null? orders)
      0
      (my-max (count-one-time (car orders) (cdr orders)) (coffe-shop-helper (cdr orders)))))

(define (coffee-shop orders)
  (if (null? orders)
      0
      (+(coffe-shop-helper orders) 1)
      )
  )
