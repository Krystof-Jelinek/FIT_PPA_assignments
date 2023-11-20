#lang racket

(define (last-item list)
  (if (null? list)
      '()
  (if (null? (cdr list))
      (car list)
      (last-item (cdr list)))))

(define (all-but-last-item list)
  (if (null? (cdr list))
      null
      (cons(car list)(all-but-last-item (cdr list)))))

(define (rotate-row row)
  (cons (last-item row) (all-but-last-item row))
  )

(define (row-lenght-helper row cur_len)
  (if (null? row)
      cur_len
      (row-lenght-helper (cdr row) (+ cur_len 1))))

(define (row-lenght row)
  (row-lenght-helper row 0))

(define(rotate-row-setup row n)
  (modulo n (row-lenght row)))
  

(define (rotate-row-times row n)
  (if (= n 0)
      row
      (rotate-row-times (rotate-row row) (- n 1))))

(define (rotate-row-n-times row n)
  (rotate-row-times row (rotate-row-setup row n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pick-nth-row rows n)
  (if (= n 0)
      (car rows)
      (pick-nth-row (cdr rows) (- n 1))))
  

(define (modify-nth-row matrix new-values n)
  (if (null? matrix)
      '()
      (if (= n 0)
          (cons new-values (cdr matrix))
          (cons (car matrix) (modify-nth-row (cdr matrix) new-values (- n 1))))))

(define (transpose matrix)
  (define (get-cols matrix)
    (if (null? (car matrix))
        '()
        (cons (map car matrix) (get-cols (map cdr matrix)))))

  (get-cols matrix))

(define (modify-nth-colum matrix new-values n)
  (transpose(modify-nth-row (transpose matrix) new-values n)))

(define (apply-command matrix command)
  (if (equal? command '())
      matrix
  (if (= 0 (car command))
      (modify-nth-row matrix (rotate-row-n-times (pick-nth-row matrix (cadr command)) (caddr command)) (cadr command))
      ;this means it was 1
      (transpose (modify-nth-row (transpose matrix)(rotate-row-n-times (pick-nth-row (transpose matrix) (cadr command)) (caddr command)) (cadr command)))
  )))

(define (rotate-mat matrix commands)
  (if (null? commands)
      matrix
      (rotate-mat (apply-command matrix (car commands)) (cdr commands))))
             
