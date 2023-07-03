#lang racket
(require "./trees/tree-mutations-lib.rkt"
         "./trees/trees.rkt")


(provide
 random-item
 random-factor
 mutate-constant
 crossover-constants
 mutate-tree
 crossover-trees
 )
(define scale-k 300)

(define (round-float number)
  (/  (round (* 1000 number)) 1000))

(define (random-item lst)
  (if (null? lst)
      null
      (list-ref lst
                (random (length lst)))))
;;---------------------------------------------
;; Function compute how much a constant will be
;; re-parameterized in a factor:
;; +-(0-100%) --> f:(0 1)
;;---------------------------------------------
(define (random-factor [percent 100])
  (let ([factor  (/ percent 100.0)])
    (+ (- 1.0 factor)
       (* (* 2.0 factor) (random)))))


(define (mutate-constant c)
  (let*([k1 (round-float (* 100 (numeric-leaf-value)))]
        [k2 (round-float (* (random-factor scale-k) c))]
        [k-list (list k1 k2)])
   (random-item k-list)))


(define (crossover-constants c1 c2)
  (let*([r (round-float(random-factor  scale-k))]
        [f (round-float(random-factor  100))]
        [k1 (round-float (* r c1))]
        [k2 (round-float (* r c2)) ]
        [k3 (round-float(* r (+ c1 c2)))]
        [k4 (round-float(+ (* f c1) (* (- 1 f) c2)))]
        [k-list (list k1 k2 k3 k4)]
        )
    (random-item k-list)))
    

(define (mutate-tree tree)
  (random-mutate-tree tree))

(define (crossover-trees tree1 tree2)
  (random-crossover-tree tree1 tree2))

(define (replicate ctrl)   ctrl)




;; (define (mutate controller)
;;   (let* ([op (operator-controller controller)]
;;          [k (k-controller controller)]
;;          [s (sigma-controller controller)]
;;          [f (f-controller controller)]
;;          [member (random-item members-list)])
;;     (define edited
;;       (cond
;;         [(equal? member 'k) (mutate-constant k)]
;;         [(equal? member 's) (mutate-tree s)]
;;         [(equal? member 'f) (mutate-tree f)]))
;;     edited
;;     (cond
;;       [(equal? member 'k) (build-controller op edited s f)]
;;       [(equal? member 's) (build-controller op k edited f)]
;;       [(equal? member 'f) (build-controller op k s edited)])
;;     ))

;; (define (crossover controller-1 controller-2)
;;   (let* ([op1 (operator-controller controller-1)]
;;          [k1 (k-controller controller-1)]
;;          [s1 (sigma-controller controller-1)]
;;          [f1 (f-controller controller-1)]
;;          [op2 (operator-controller controller-2)]
;;          [k2 (k-controller controller-2)]
;;          [s2 (sigma-controller controller-2)]
;;          [f2 (f-controller controller-2)]
;;          [member (random-item members-list)])
;;     (define edited
;;       (cond
;;         [(equal? member 'k) (crossover-constant k1 k2)]
;;         [(equal? member 's) (crossover-tree s1 s2)]
;;         [(equal? member 'f) (crossover-tree f1 f2)]
;;         [else (error "vacio edited")])
;;       )
;;     edited
;;     (cond
;;       [(equal? member 'k) (build-controller op1 edited s1 f1)]
;;       [(equal? member 's) (build-controller op1 k1 edited f1)]
;;       [(equal? member 'f) (build-controller op1 k1 s1 edited)]
;;       [else (error "vacio" member)]
;;       )))
    




