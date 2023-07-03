
#lang racket

(provide (all-defined-out))
;;-------------------------------------
;; This fuction validates if an element
;; given belongs to the list. Return #t
;; if is found and #f in other way
;;-------------------------------------
(define (member? item lst)
  (if (null? lst)  #f
      (if (equal? item (car lst))
          #t
          (member? item (cdr lst)))))
;;-------------------------------------
;; This function choose randomly one
;; element of a given list
;;-------------------------------------
(define (random-item lst)
  (if (null? lst)
      null
      (list-ref lst
                (random (length lst)))))

;;-------------------------------------
;; This function set the given value
;; in the index position of a list
;;-------------------------------------

(define (list-set-in! _list index item)
  (if (zero? index)
      (cons item (cdr _list))
      (cons (car _list) (list-set-in! (cdr _list)
                                      (sub1 index)
                                      item))))
      

;;-------------------------------------
;; This function validates if the number
;; given is equal to one
;;-------------------------------------
(define (one? num)
  (equal? num 1))
;;-------------------------------------
;; This function return a random number
;; between a min and max range given
;; rand=(min max)
;;-------------------------------------
(define (rnd-range min max)
  (cond ((equal? min max) max)
        (else
         (+ (* (random) (- max min)) min)
         )))
;;-------------------------------------
;;-------------------------------------
(define (map0 proc n)
  (define (proc-aux x)
    (proc))
  (map proc-aux (range n)))



