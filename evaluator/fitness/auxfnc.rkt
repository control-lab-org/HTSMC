#lang racket


;; =====  display related functions ===== 
(define (nl) (display "\n"))  ; display new line
(define (dp msg) (display msg) (display "\n"))  ; msg followed by \n



;; ===== random number generation
;; generate a random float in [a, b)
(define (rnd a b)
  (let ([sz (abs (- b a))])
    (+ a (* sz (random)))))


;; ===== repeat procs ===== 
;; repeat a side-effect proc with no arguments
(define (repeat-proc proc n)   ; 
  (define (proc-aux x) 
    (proc))            
  (for-each proc-aux (range n)))

;; create a list by repeating a proc with no arguments
(define (map0 proc n)   ; 
  (define (proc-aux x) 
    (proc))            
  (map proc-aux (range n)))

;; create a list of n zeros
(define (zeros n)
  (map (λ (x) 0.0) (range n)))

;; some list aux functions
(define (lst-scale lst k)
  (map (λ (elem) (* k elem)) lst))

;; ===== making symbols ===== 
(define (make-symb prefix number)
  (string->symbol
   (string-append prefix (number->string number))))

(define (make-list-of-symb prefix n)
  (map (λ (x) (make-symb prefix x)) (range n)))


;; ===== zero fnc ===== 
(define (zero-fnc)
  (λ (x) 0.0))
 


;; ===== execute a list of procedures =====  
(define (call-each procedures)
  (for-each (λ (proc) (proc)) procedures))

(define (call-until-no-more-changes procedures)
  (if (ormap (λ (proc) (proc)) procedures)
      (call-until-no-more-changes procedures)
      'ok))

(define (last-item lis)
  (let  ([last (sub1 (length lis))])
    (list-ref lis last)))

;; export all defined function
(provide (all-defined-out))

