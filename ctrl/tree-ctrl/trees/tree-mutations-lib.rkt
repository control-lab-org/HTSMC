#lang racket
(require "./trees.rkt")
(require "../tree-prmtrs.rkt")
(require "./general-fun.rkt")


(require "./tree-mutations-truncate.rkt")
(require "./tree-mutations-inher.rkt")


;; [x] replacement 
;; [x] truncation  
;; [x] inheritance
;; [x] reparametrize
;; [x] scale
;;------------

(provide tree-reparam
         tree-scale
         tree-replacement
         tree-truncation
         make-btree
         make-utree
         tree-scale-factor 
         mutations-list
         random-mutate-tree
         random-crossover-tree
        ; numeric-random-value
         )


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

;;------------------------------
;; Reparam definitions
;;------------------------------
(define (tree-reparam tree)
   (let* ([leaves (extract-leaves  tree)]
          [marked-tree (cut-leaves tree)]
          [newleaves (random-scale-leaves leaves)]
          )
     (graft-leaves newleaves marked-tree)))

;;------------------------------
;; Scale definitions
;;------------------------------
(define (tree-scale-factor  tree factor)
  (make-btree '* factor tree))

(define (tree-scale tree)
  (let*
      ([branch (random-branch  tree)]
       [marked-tree (prune-tree branch tree)]
       [scaled-tree (tree-scale-factor
                     (reparam-random-factor) branch)])
    (graft-subtree scaled-tree marked-tree)))



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


(define mutations-list (list
                        tree-reparam
                        ;tree-scale
                        tree-truncation
                        tree-replacement
                              ))

(define (random-mutate-tree tree)
  (let ([mutation-op (random-item mutations-list)]) 
   (mutation-op tree))) 


(define (tree-crossover-branch tree-1 tree-2)
  (let*([r (round-float (random))]
        [trees (list tree-1 tree-2)])
    (tree-scale-factor r (random-item trees))))

(define (tree-crossover-part tree-1 tree-2)
  (let*([r1 (round-float  (random))]
        [r2 (-  1 r1 )])
    (make-btree '+
                (tree-scale-factor r1 tree-1)
                (tree-scale-factor r2 tree-2))))

(define (tree-crossover-mix tree-1 tree-2)
  (let*([r (round-float (random))]
        [operator (binary-node)])
    (tree-scale-factor
     r
     (make-btree operator tree-1 tree-2))))

  (define (crossover-tree tree1 tree2)
    (let* ([branch-1 (random-branch tree1)]
           [branch-2 (random-branch tree2)]
           [cutted-tree1    (cut-tree branch-1 tree1)]
           [cutted-tree2    (cut-tree branch-2 tree2)]
           [candidates      (list
                             (graft-subtree  branch-2 cutted-tree1)
                             (graft-subtree  branch-1 cutted-tree2))])
      (random-item candidates)))

      
(define crossover-list (list
                        ;tree-crossover-branch
                        ;tree-crossover-part
                        tree-crossover-mix
                        crossover-tree
                        ))
                            
(define (random-crossover-tree tree-1 tree-2)
  (let ([crossover-op (random-item crossover-list)])
    (crossover-op tree-1 tree-2)))



;; ;; ;; --------- Debugg Code  ------------
;; ;; ;; -----------------------------------
;; (define _tree   (make-full-tree 3))
;; (define _tree1   (make-full-tree 3))
;;  (display "Tree:  ")
;; _tree
;; (newline)
;;  (display "Tree1:  ")
;; _tree1
;; (newline)
;; (define crossed (crossover-tree _tree _tree1))
;; crossed
;; ;; (newline)
;;  (tree-truncation crossed)
;; (tree-inherence _tree)
;; (tree-reparam _tree)
;; (tree-scale _tree)
;; (tree-replacement _tree)


;; (tree-truncation (tree-reparam _tree))
;; (tree-inherence (tree-reparam _tree))
;; (tree-reparam (tree-truncation _tree))
;; (tree-scaletree-replacement _tree))
;; (tree-replacement (tree-reparam _tree))
;;(crossover-tree _tree _tree1)
