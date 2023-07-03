#lang racket

(provide make-rdynsys)

;; Object to define a dynamic system based
;; on the dynsys simulator XXXX definition.
;; It is a reduce definition with few parameters
(define (make-rdynsys)
  (let( [state-expr  null]
        [out-expr    null]
        
        [state-symb  null]
        [in-symb     null]
        [prmtr-symb  null]
        
        [prmtr-val  null]          
        [init-cond  null]         
        
        [state-val  null]
        [name null])
    
    (define (set-state-expr! new-state-expr)
      (set! state-expr new-state-expr))

    (define (set-out-expr! new-out-expr)
      (set! out-expr new-out-expr))
    
    (define (set-state-symb! new-state-symb)
      (set! state-symb new-state-symb))

    (define (set-in-symb! new-in-symb)
      (set! in-symb new-in-symb))
    
    (define (set-prmtr-symb! new-prmtr-symb)
      (set! prmtr-symb new-prmtr-symb))
    
    (define (set-prmtr-val! new-prmtr-val) 
      (set! prmtr-val new-prmtr-val))
    
    (define (set-init-cond! new-ic)  
      (set! init-cond new-ic))

    (define (set-name! new-name)  
      (if (> (string-length new-name) 9)
          (set! name (substring new-name 0 9))
          (set! name new-name))
      )
   

    (lambda (msg . args)
      (case msg      
        ;; info getters
        [(state-symb)  state-symb]
        [(in-symb)     in-symb]
        [(prmtr-symb)  prmtr-symb]
        [(state-expr)  state-expr]
        [(out-expr)    out-expr]
        
        ;; state getters
        [(state-val)  state-val]
        [(prmtr-val)  prmtr-val]
        [(init-cond)  init-cond]
        [(name)  name]
        
        ;; state setters
        [(set-state-expr!)  (apply set-state-expr! args)]
        [(set-out-expr!)    (apply set-out-expr! args)]       
        [(set-state-symb!)  (apply set-state-symb! args)]
        [(set-prmtr-symb!)  (apply set-prmtr-symb! args)]
        [(set-in-symb!)     (apply set-in-symb! args)]        
        [(set-init-cond!)   (apply set-init-cond! args) ]
        [(set-prmtr-val!)   (apply set-prmtr-val!  args)]
        [(set-name!)   (apply set-name!  args)]
        
        ;; debuggin msg
        [else (error "Unknown operation RDYNSYS" msg)]))
    ))


;;; | ----- Debug Code -------|

;;; Define a dynamic system 
;; (define plant-dbg  (make-rdynsys))
;; ;;; Set the values of the plant 
;; (plant-dbg  'set-state-expr!
;;             '((x2)
;;               (+ (- (- (* (/ g l) (sin x1)))
;;                     (* (* b m) x2))
;;                  (/ u (* m (* l l))))
;;               ))
;; (plant-dbg 'set-out-expr! '( (x1) (x2) ))
;; (plant-dbg 'set-state-symb! '(x1 x2))
;; (plant-dbg 'set-prmtr-symb! '(m g l b))
;; (plant-dbg 'set-in-symb! '(u))
;; (plant-dbg 'set-prmtr-val! '(0.5 9.81 1.0 1.0))
;; (plant-dbg 'set-init-cond! '(0.1 0.0))

;
;;; Display the system values
;(plant-dbg 'state-expr)
;(plant-dbg 'out-expr) 
;(plant-dbg 'state-symb)
;(plant-dbg 'prmtr-symb)
;(plant-dbg 'in-symb )
;(plant-dbg 'prmtr-val)
;
;
