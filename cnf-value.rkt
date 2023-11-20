#lang racket

(define (sub-one-number number eval-rules)
  (let ((cur-rule (car eval-rules))) 
  (if (= (car cur-rule) number)
      (cadr cur-rule)
      (sub-one-number number (cdr eval-rules)))))
      
(define (substitude formula eval-rules)
  (if (null? formula)
      '()
      (cons (sub-one-number (car formula) eval-rules)(substitude (cdr formula) eval-rules))
  ))

(define (eval-disjnc formula)
  (if (null? formula)
      #f
  (if (equal? (car formula) #t)
      #t
      (eval-disjnc (cdr formula)))))

(define (cnf-value form values)
  (if (null? form)
      #t
      (and (eval-disjnc (substitude (car form) values))(cnf-value (cdr form) values))))
