#lang racket
(require "./trees.rkt")
(require "../tree-prmtrs.rkt")
(require "./general-fun.rkt")
;;------------------------------
(provide tree-reparam
         tree-scale)
;;------------------------------
;; Replacement definitions
;;------------------------------
(define (tree-reparam tree)
   (let* ([leaves (extract-leaves  tree)]
          [marked-tree (cut-leaves tree)]
          [newleaves (random-scale-leaves leaves)]
          )
     (graft-leaves newleaves marked-tree)))


(define (tree-scale tree)
  (let*
      ([branch (random-branch  tree)]
       [marked-tree (prune-tree branch tree)]
       [scaled-tree (make-btree '* (reparam-random-factor) branch)])
    (graft-subtree scaled-tree marked-tree)))
    
  
;;------------------------------
 ;; (define _tree   (make-full-tree 4))
 ;; _tree
 ;; (tree-scale _tree)
