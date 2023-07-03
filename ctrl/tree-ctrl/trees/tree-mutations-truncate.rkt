#lang racket
(require "./trees.rkt")
(require "../tree-prmtrs.rkt")
(require "./general-fun.rkt")

(provide tree-truncation)
;;------------------------------
;; Truncation  definitions
;;------------------------------
 (define (tree-truncation tree)
   (let ([depth (depth-tree tree)])
     (if (> depth 1)
         (random-branch-truncate tree)
         (random-leaf-truncate tree)
         )))
;;------------------------------
 (define (random-branch-truncate tree)
   (let* ([branch (random-branch tree)]
          [marked-tree (prune-tree branch tree)])
     (truncate-branch marked-tree branch)
     ))
;;------------------------------
(define (truncate-branch marked-tree branch)
  (let* ([leaves (extract-leaves branch)]
         [leaf  (random-item leaves)])
    (graft-subtree leaf marked-tree)))
;;------------------------------
(define (random-leaf-truncate tree)
  (let* ([leaves (extract-leaves  tree)]
         [leaf  (random-item leaves)]
         )
    (make-btree '* 1.0 leaf)))
;;------------------------------
;; ;; -----------------------------------
;; ;; --------- Debugg Code  ------------
;; ;; -----------------------------------
;; (define _tree   (make-full-tree 3))
;; (display "Tree:  ")
;; _tree

