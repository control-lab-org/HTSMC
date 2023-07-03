#lang racket
(require "./trees.rkt")
(require "../tree-prmtrs.rkt")
(require "./general-fun.rkt")
;;------------------------------
(provide tree-inherence)
;;------------------------------
;; Inherence  definitions
;;------------------------------
(define (tree-inherence tree)
  (let* ([leaves (extract-leaves  tree)]
         [leaf  (random-item leaves)]
         )
    (make-btree '* 1.0 leaf)))
;;------------------------------
