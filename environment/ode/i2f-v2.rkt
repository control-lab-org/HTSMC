#lang racket


(require "./infix2postfix.rkt")
(provide infx<-fix
         infix-2-postfix

         )

(define (infx<-fix sexp)
  (fixed-2-infix sexp))


;; Debugg code 
;; (define t3 '(* (+ (- e) (* e -16.720351734625446))  (+ (- -16.647842075384954) e)))
;; (infx<-fix t3)

