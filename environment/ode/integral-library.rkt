#lang racket

(require "../../ctrl/tree-ctrl/trees/trees.rkt")
;(require "../../ctrl/tree-ctrl/trees/general-fun.rkt")
(provide integral-symb
         dynamical-ctrl?
         make-integral-sys
         integral-fnc
         u-fnc
         )

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
         
(define integral-symb 'iv)

;;-------------------------------------
(define (dynamical-ctrl? ctrl)
  (let *([expr (flatten ctrl)])
    (member? 'integral expr)))

;;-------------------------------------
(define (extract-integral ctrl)
  ;(second
   (third (second ctrl))
  ; )
  )
;;-------------------------------------

(define (make-integral-sys ctrl)
  (let*([ibranch (extract-integral ctrl)]
        [ctrl-cutted (cut-tree ibranch ctrl)]
        [graft-ctrl (graft-tree integral-symb ctrl-cutted)]
        )
    (list (second ibranch) graft-ctrl)))

(define (integral-fnc auxsys)
  (first auxsys))

(define (u-fnc auxsys)
  (second auxsys))



 (define test-i '(*   (* 12.602761504560334 (integral (* (- e) (- 19.60774011872941))))
   (- (* -18.627909513796954 -9.310109460843439))))

