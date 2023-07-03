#lang racket
(provide
 fixed-2-infix
 reduce-sexp        
 )

(provide infix-2-postfix
         )

(define (make-unary op arg)
  (list op (list arg)))
  ;(cons op  arg))

(define (make-binary op arg0 arg1)
  (list  arg0 op arg1))

(define (print-args op args-list)
  (let ([arg-0 (car args-list)]
        [arg-1 (cdr args-list)])
    (if (null? arg-1)
        (make-unary op (infix-2-postfix arg-0))
        (make-binary op
                     (infix-2-postfix arg-0)
                     (infix-2-postfix (car arg-1)))
        )))

(define (infix-2-postfix sexp)
  (if (cons? sexp)
      (if (null? (cdr sexp) )
          (car sexp)
          (print-args (car sexp) (cdr sexp)))
      (if(number? sexp)
         (list sexp)
         sexp))
  )

(define (round-float number)
  (/  (round (* 1000 number)) 1000))

(define (check-arg arg)
  (if (number?  arg)
      (if (< arg 0)
          (list (round-float  arg))
          (round-float  arg))
      arg))

(define (one? something)
  (equal? something 1))

(define (two? something)
  (equal? something 2))

(define (three? something)
  (equal? something 3))

(define (sign num)
  (cond
    [(< num 0) -1.0]
    [(equal? num 0) 0]
    [else      1.0]))

  

(define (replace-syms quo)
  (cond
    [(equal? quo '-) -]
    [(equal? quo '+) +]
    [(equal? quo '*) *]
    [(equal? quo '/) /]
    [(equal? quo 'tanh) tanh]
    [(equal? quo 'sign) sign]

    ))


(define (parse-unary sexp)
  (let* ([op (first sexp)]
         [arg0 (fixed-2-infix(second sexp))]
         )
    (if (number? arg0)
        (check-arg ((replace-syms op) arg0))
        (list op (list  arg0))))) 


(define (parse-binary sexp)
  (let* ([op (first sexp)]
         [arg0 (fixed-2-infix (second sexp))]
         [arg1 (fixed-2-infix (third sexp))])
    (if (and (number? arg0) (number? arg1))
        (check-arg  ((replace-syms op) arg0 arg1))
        ;(list (p-i arg0) op  (p-i arg1))
        (list (check-arg arg0) op  (check-arg arg1))
        )))


(define (parse sexp)
  (let ([len (length sexp)])
    (cond
      [(zero? len) 'zero]
      [(one? len) (check-arg sexp)]
      [(two? len)  (parse-unary sexp)]
      [(three? len) (parse-binary sexp) ]
      [else 'error])
      ))

(define (fixed-2-infix sexp)
  (cond
    [(list? sexp)  (parse sexp)]
    [(number? sexp) (round-float sexp)]
    [else sexp]))



(define (parse-unary-fix sexp)
  (let* ([op (first sexp)]
         [arg0 (reduce-sexp (second sexp))]
         )
    (if (number? arg0)
        ((replace-syms op) arg0)
        (list op (list  arg0))))) 
    
(define (parse-binary-fix sexp)
  (let* ([op (first sexp)]
         [arg0 (reduce-sexp (second sexp))]
         [arg1 (reduce-sexp (third sexp))])
    (if (and (number? arg0) (number? arg1))
        ((replace-syms op) arg0 arg1)
        (list op arg0  arg1)
        )))

(define (parse-fix sexp)
  (let ([len (length sexp)])
    (cond
      [(zero? len) 'zero]
      [(one? len) (check-arg sexp)]
      [(two? len)  (parse-unary-fix sexp)]
      [(three? len) (parse-binary-fix sexp) ]
      [else 'error])
      ))

(define (reduce-sexp sexp)
  (cond
    [(list? sexp)  (parse-fix sexp)]
    [(number? sexp) (round-float sexp)]
    [else sexp]))



(define (make-fxu op arg)
  (list op (list arg)))
(define (make-fxb op arg0 arg1)
  (list op arg0 arg1))

(define (cut-integral sexp)
  (if ( eq? 'integral (first sexp))
      (cadr sexp)
      null))

(define (cut-integrals sexp)
   (let ([len (length sexp)])
    (cond
      [(zero? len) 'zero]
      [(one? len)  null]
      [(two? len)   sexp]
      [(three? len) (parse-binary-fix sexp) ]
      [else 'error])
      ))


