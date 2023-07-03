#lang racket
(require "./trees.rkt")
(require "../tree-prmtrs.rkt")
(require "./general-fun.rkt")

;;-------------------------------------
;; In this library are defined the functions
;; that provide how to replace a branch, leaf
;; or even the  tree
;; -------------------------------------
(provide tree-replacement)
;;------------------------------------------------
;; Replacement definitions
;;------------------------------------------------
;; This function reaplaces a random branch of the
;; tree by a new one. If its only a simple tree,
;; the leaves are replaced
;;------------------------------------------------
(define (tree-replacement tree)
   (let ([depth (depth-tree tree)])
     (if (> depth 1)
         (random-branch-replace tree)
         (random-leaf-replace tree)
         )))

;;------------------------------------------------
;; This function select the branch of the tree
;; that are going to be cutted, cut the selected
;; brnach and replace it by a new one
;;------------------------------------------------
 (define (random-branch-replace tree)
   (let* ([branch (random-branch  tree)]
          [marked-tree (prune-tree branch tree)])
     (replace-branch marked-tree branch)
     ))
;;------------------------------------------------
;; This function take a marked tree and a branch
;; and create a new tree in the same depth of the
;; cutted branch and graft the new one in the marked
;; tree
;;------------------------------------------------
 (define (replace-branch marked-tree branch)
   (let* ([branchdepth (depth-tree branch)]
          [newbranch (make-free-tree branchdepth)])
      (graft-subtree newbranch marked-tree)))

;;------------------------------------------------
;; This function extract all the leaves in a tree
;; and replace one leaf randomly
;;------------------------------------------------
(define (random-leaf-replace tree)
  (let* ([leaves      (extract-leaves  tree)]
         [marked-tree (cut-leaves tree)]
         [newleaf     (random-leaf)]
         [index       (random (count-leaves leaves))]
         [newleaves   (change-leaf newleaf leaves index)])
    (graft-leaves newleaves marked-tree)))

;; -----------------------------------
;; --------- Debugg Code  ------------
;; -----------------------------------
;; (define _tree   (make-full-tree 3))
;; (display "Tree:  ")
;; _tree
;; (display "Leaf-Replace:  ")
;; (random-leaf-replace _tree)
;; (display "Branch-Reaplace:  ")
;; (random-branch-replace _tree)

