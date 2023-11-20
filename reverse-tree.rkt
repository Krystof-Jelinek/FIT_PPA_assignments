#lang racket

(define (my-list one two tree)
  (cons one (cons two (cons tree '()))))

(define (reverse-tree bst)
  (if (null? bst)
      '()
      (my-list (car bst)
            (reverse-tree (caddr bst))
            (reverse-tree (cadr bst)))))