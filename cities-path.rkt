#lang racket

(define (is-present visited n)
  (if (null? visited)
      #f
  (if (= (car visited) n)
      #t
      (is-present (cdr visited) n))))

(define (combine-lists-uniq list1 list2)
  (if (null? list1)
      list2
      (if (is-present list2 (car list1))
          (combine-lists-uniq (cdr list1) list2)
          (cons (car list1) (combine-lists-uniq (cdr list1) list2)))))

(define (find-neighbours from paths)
  (if (null? paths)
      null
      (if (equal? from (car (car paths)))
          (cons (cadr (car paths)) (find-neighbours from (cdr paths)))
          (find-neighbours from (cdr paths)))))

(define (find-neighbours-list list paths)
  (if (null? list)
      '()
      (combine-lists-uniq (find-neighbours (car list) paths) (find-neighbours-list (cdr list) paths))))

(define (find-reachable from paths already-visited)
  (let ((new-nodes (combine-lists-uniq (find-neighbours-list from paths) already-visited)))
    (if (equal? new-nodes already-visited)
        new-nodes
        (find-reachable new-nodes paths new-nodes)))
  )

(define (cities-path? paths from to)
  (if (= from to)
      #t
      (if (equal? #t (is-present (find-reachable (cons from null) paths '()) to))
          #t
          #f)))

