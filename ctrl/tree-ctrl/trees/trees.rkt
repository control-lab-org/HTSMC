#lang racket
(require "../tree-prmtrs.rkt")
(require "./general-fun.rkt")
(provide (all-defined-out))
;;--------------------------------------------
;;The subsecuent functions describe the manage
;; of tree functios
;; NOTES: Debug Test: ALL PROBED ok
;; updated: april/04/2023 1
;;--------------------------------------------
(define prmtrs (make-tree-prmtrs))

(define (round-float number)
  (/  (round (* 1000 number)) 1000))
;;--------------------------------------------
;; This function return one of two posible
;; options and probability given.
;; The probabilty given must be the one of
;; select the first option
;;--------------------------------------------
(define (select-randomly option-one option-two prob-one)
  (if (< (random) prob-one) option-one option-two))
;;--------------------------------------------
;; This function verify if the tree is created
;; by the data type declared
;;--------------------------------------------
(define (made-by? tree)
  (list? tree))

(define (void-tree? tree)
  (null? tree))

(define (void-forest? forest)
  (null? forest))

(define (tree-forest forest)
  (car forest))
(define (forest-forest forest)
  (cdr forest))

(define void-tree   null)
(define void-forest null)
;;--------------------------------------------

(define cut-marker  'cutted)
(define (cut-marker? item)
  (eq? item cut-marker))
;;--------------------------------------------------
;;---- Estructura para el manejo de árboles --------
;;--------------------------------------------------
(define (tree? item)
  (and (made-by? item)
       (filled? item)
       (valid-node? (node item))
       (filled? (leaves item))))

;;--------------------------------------------
;; This function verify if the item given
;; is not null
;;--------------------------------------------
(define (filled? item)
  (not (null? item)))
;;--------------------------------------------
;; This function validates if the node is
;; defined as valid operator.
;; A node is defined as quoted math symbol:
;; {'*, '+, '-, 'tanh, ....}
;;--------------------------------------------
(define (valid-node? symbol)
  (member? symbol (append (prmtrs 'nodes)
                          (list 'integral 'tanh))))

(define (valid-symb? symbol)
  (member? symbol (prmtrs 'symbolics)))
;;--------------------------------------------
;; This function validates if the node is
;; defined as valid  unary operator
;;--------------------------------------------
(define (unary-node? node)
  (member? node (prmtrs 'unary-nodes)))
;;--------------------------------------------
;; This function validates if the node is
;; defined as valid binary operator
;;--------------------------------------------
(define (binary-node? node)
  (member? node (prmtrs 'binary-nodes)))
;;--------------------------------------------
;; Definition of a tree and selector
;;--------------------------------------------
; A tree is made by one node and a list of leaves
;; |node|leaves|
;;--------------------------------------------
(define (make-tree node leaves)
  (list* node leaves) 
  )
;;--------------------------------------------
;; Selectors of tree elementes
;;--------------------------------------------
(define (node tree)
  (car tree))
(define (leaves tree)
  (cdr tree))
;;--------------------------------------------

;;--------------------------------------------
;; These functions returns an unary or binary
;; node randomly from the sets declared in the
;; parameters
;;--------------------------------------------
(define (unary-node [exception null])
  (random-item (remove
                exception
                (prmtrs 'unary-nodes))))
;;--------------------------------------------
(define (binary-node [exception null])
  (random-item (remove
                exception
                (prmtrs 'binary-nodes))))
;;--------------------------------------------
(define (make-random-node [exception null])
  (let ([probability (prmtrs' unary/binary-probability)])
    (select-randomly
     (unary-node exception)
     (binary-node exception)
     probability )))
;;--------------------------------------------

;;--------------------------------------------
;;This function make the declaration of a leaf
;;--------------------------------------------
(define (make-leaf item)
  (list item) )
;;--------------------------------------------
;;This function join two leaves in one list 
;;--------------------------------------------
(define (join-leaves leaf0 leaf1)
   (append leaf0 leaf1))
;;--------------------------------------------
;; This function make a leaf or leaves from
;; items given.
;; If only one item is given returns a leaf
;; --------------------------------------------
(define (make-leaves item-1 [item-2 null])
  (if (null? item-2)
       (make-leaf item-1)
      (join-leaves (make-leaf item-1) (make-leaf item-2))
      ))

;define (numeric-leaf? leaf)
;;-------------------------------------------
;; This function returns a numeric value
;; into numeric leaf range defined in parameters
;;-------------------------------------------
(define (numeric-leaf-value)
  (let* ([min-range (prmtrs 'min-range)]
         [max-range (prmtrs 'max-range)]
         [value (rnd-range min-range max-range)])5
    (round-float value)))
    ;value
    ;(/  (round (* 1000 value)) 1000)))
    
;;-------------------------------------------
;; This function returns a symbolic variable
;;-------------------------------------------
(define (symbolic-leaf-value)
  (let ([symbols_ (prmtrs 'symbolics)])
    (if (null? symbols_)
        (numeric-leaf-value) 
        (random-item symbols_))))
;;-------------------------------------------
;; This function returns a numeric or symbolic
;; variable randomly
;;-------------------------------------------
(define (random-leaf)
  (let ([probability (prmtrs' symbolic/number-probability)])
    (select-randomly
     (numeric-leaf-value)
     (symbolic-leaf-value)
     probability
     )))

(define (make-nrandom-leaves n)
  (map (lambda (i) (random-leaf)) (range n)))
;;------------------------------------------
;; This function makes an unary tree from
;; a node and one item
;;------------------------------------------
(define (make-utree node item)
  (make-tree node (make-leaf item)))
;;------------------------------------------
;; This function makes a binary tree from
;; a node and two items
;;------------------------------------------
(define (make-btree node item-1 item-2)
  (make-tree node (make-leaves item-1 item-2)))
;;------------------------------------------

;;------------------------------------------
;; This function makes a one level tree
;; unary or binary, filled randomly
;;-------------------------------------------
(define (make-random-onelevel-tree [exception null])
  (let ([node (make-random-node exception)])
    (if (unary-node? node)
        (make-utree node (random-leaf))
        (make-btree node (random-leaf)
                    (random-leaf)))))
;;-------------------------------------------
;; This function builds a random full-tree 
;; Requires two parameters: depth and node exception
;;------------------------------------------
(define (build-full-tree depth excp)
  (let* ([node (make-random-node excp)]
         [newdepth (sub1 depth)])
    (if (unary-node? node)
        (make-utree node
                    (make-full-tree  newdepth excp))
        (make-btree node
                    (make-full-tree  newdepth excp)
                    (make-full-tree newdepth excp)))))
;;-------------------------------------------
;; This function makes random full-tree 
;; Requires two parameters: depth and node exception
;; by default these are:
;; deptyh = 1
;; exception = null (null => no excepetion)
;;------------------------------------------
(define (make-full-tree [depth 1] [excp null])
  (cond ((zero? depth) (random-leaf))
        ((one? depth) (make-random-onelevel-tree excp))
        (else (build-full-tree depth excp))))
  
;;------------------------------------------
;; This function decide if the leaf of a tree
;; is going to be a leaf or the tree grown and
;; add a new tree.
;;------------------------------------------
(define (leaf-or-tree depth excp)
  (select-randomly
   (random-leaf)
   (make-free-tree depth excp)
    (prmtrs 'leaf/node-probability)
    ))
;;-------------------------------------------
;; This function builds a random free-tree 
;; Requires two parameters: depth and node exception
;;------------------------------------------
(define (build-free-tree depth excp)  
  (let* ([node (make-random-node excp)]
         [newdepth (sub1 depth)])
    (if (unary-node? node)
        (make-utree
         node
         (leaf-or-tree newdepth excp))
        (make-btree
         node
         (leaf-or-tree newdepth excp)
         (leaf-or-tree newdepth excp)))))


;;-------------------------------------------
;; This function makes a random free-tree depth
;; Requires two parameters: depth and node exception
;; by default these are:
;; depth = 1
;; exception = null (null => no excepetion)
;;------------------------------------------
(define (make-free-tree [depth 1] [excp null])
  (cond ((zero? depth) (random-leaf))
        ((one? depth) (make-full-tree 1 excp))
        (else (build-free-tree depth excp))))
        

;;------------------------------------------
;; This function cheks if the item given is
;; a number or not
;;------------------------------------------
(define (numeric-item? item)
  (number? item))

;;------------------------------------------
;; This function checks if the item given
;; is declared in the parametrs
;;------------------------------------------
(define (valid-item? item)
  (or (valid-symb? item)
      (numeric-item? item)
      (cut-marker? item)
      ))

;;------------------------------------------
;; This function check if the leaf given
;; is valid
;;------------------------------------------
(define (valid-leaf? leaf)
  (let ([item (car leaf)])
    (valid-item? item)))
;;------------------------------------------
;; This function check if the element given
;; is consider as leaf
;;------------------------------------------
(define (leaf? leaf)
  (if (list? leaf)
      (valid-leaf? leaf)
      (valid-item? leaf)))
;;-------------------------------------------
;;-------- Creación de árboles --------------
;;-------------------------------------------
;; Función que recibe un árbol y devuelve la
;; máxima profundidad
;;-------------------------------------------
(define (depth-tree tree)
  (if (void-tree? tree)
      0
      (if (leaf? tree)
          0
          (add1 (depth-subtree (leaves tree))))))
;;-------------------------------------------
;; Función que una lista de subárboles y
;; devuelve la máxima profundidad encontrada
;;-------------------------------------------
(define (depth-subtree forest)
  (if (void-forest? forest)
      0
      (max (depth-tree    (node forest))
           (depth-subtree (leaves forest)))))
;;-------------------------------------------

;;-------------------------------------------
;; ----------    Tree operation     ---------
;;-------------------------------------------

;;-------------------------------------------
;; This function returns a random branch of a
;; tree
;;-------------------------------------------
(define (random-branch tree)
  (extract-rnd-subtree tree)) 
;;-------------------------------------------

;;-------------------------------------------
;; This function extract a random subtree
;; from a given tree. By definition
;; one-depth tree has not subtrees and return
;; they leaves 
;;-------------------------------------------
(define (extract-rnd-subtree tree)
  (let* ([depthmax (depth-tree tree)]
         [depthmin 1])
     (if (one? depthmax)
         (random-subtree-at-depth
          tree depthmax)
         (random-subtree-at-depth
          tree (random depthmin depthmax)))))
;;-------------------------------------------

;;-------------------------------------------
;; This function retuns randomly a subtree found
;; in the depth given. 
;;-------------------------------------------
(define (random-subtree-at-depth tree depth)
  (let ([subtree-list (subtrees-at-ndepth tree depth)])
    (random-item subtree-list)))
;;-------------------------------------------

;;-------------------------------------------
;; This function retuns a list of subtrees
;; found at the given depth. 
;;-------------------------------------------
(define (subtrees-at-ndepth tree depth)
  (cond ((leaf? tree)  void-tree)
        ((valid-node? tree)  void-tree)
        ((zero? depth) (list tree))
        ((one? (depth-tree tree))  (leaves tree))
        (else (forest-at-ndepth tree (sub1 depth)))))
;;-------------------------------------------
;; This function a list with the subtrees found
;; in a forest
;;-------------------------------------------
(define (forest-at-ndepth forest depth)
   (if (void-forest? forest)  void-forest
       (append
        (subtrees-at-ndepth (tree-forest   forest) depth)
        (forest-at-ndepth   (forest-forest forest) depth))))
;;-------------------------------------------

;; (define _test_tree_ndepth (make-full-tree 1))
;; _test_tree_ndepth
;; (subtrees-at-ndepth _test_tree_ndepth 0)
;; (subtrees-at-ndepth _test_tree_ndepth 1)
;; (subtrees-at-ndepth _test_tree_ndepth 2)
;; (subtrees-at-ndepth _test_tree_ndepth 3)
;; (subtrees-at-ndepth _test_tree_ndepth 4)
;; (subtrees-at-ndepth _test_tree_ndepth 5)
;; (subtrees-at-ndepth _test_tree_ndepth 6)
;; (newline)
;; (extract-rnd-subtree  _test_tree_ndepth) 
;;-------------------------------------------
(define (list-leaves? elements)
  (and (not (tree? elements))
       (list? elements)
       (not (null? elements))
       (or (leaf? (first elements))
           (leaf? (leaves (first elements)))
       )))
  
;;-------------------------------------------


;;-------------------------------------------
;; This function search a subtree inside the tree,
;; if it is found, remove the subtree and is replaced
;; with the mark 'cutted
;;-------------------------------------------
(define (cut-tree-i subtree tree)
  (cond ((eq? subtree (car tree)) cut-marker)
        ((leaf? tree) tree)
        (else (make-tree
               (node tree)
               (cut-forest-i subtree (leaves tree))))))
;;-------------------------------------------
;; This function search a tree into a forest
;; if it is found, remove the tree and is replaced
;; with the mark 'cutted
;;-------------------------------------------
(define (cut-forest-i subtree forest)
  (if (void-forest? forest) forest
      (cons (cut-tree-i
             subtree (tree-forest forest))
            (cut-forest-i
             subtree (forest-forest forest)))))
;;-------------------------------------------

;;-------------------------------------------
;; This function search a subtree inside the tree,
;; if it is found, remove the subtree and is replaced
;; with the mark 'cutted
;;-------------------------------------------
(define (cut-tree subtree tree)
  (cond ((eq? subtree tree) cut-marker)
        ((leaf? tree) tree)
        (else (make-tree
               (node tree)
               (cut-forest subtree (leaves tree))))))
;;-------------------------------------------
;; This function search a tree into a forest
;; if it is found, remove the tree and is replaced
;; with the mark 'cutted
;;-------------------------------------------
(define (cut-forest subtree forest)
  (if (void-forest? forest) forest
      (cons (cut-tree
             subtree (tree-forest forest))
            (cut-forest
             subtree (forest-forest forest)))))
;;-------------------------------------------

;; Repited items are cutted too

;; ;;-------------------------------------------
;; ;; This function search a subtree inside the tree,
;; ;; if it is found, remove the subtree and is replaced
;; ;; with the mark 'cutted
;; ;;-------------------------------------------
;; (define (cut-tree2 cut-item tree)
;;   (cond ((equal? cut-item tree)  cut-marker)
;;         ((leaf? tree) tree)
;;         (else
;;          (make-tree
;;           (node tree)
;;           (cut-forest2 cut-item (leaves tree))))))
;; ;;-------------------------------------------
;; ;; This function search a tree into a forest
;; ;; if it is found, remove the tree and is replaced
;; ;; with the mark 'cutted
;; ;;-------------------------------------------
;; (define (cut-forest2 cut-item forest)
;;   (if (void-forest? forest) forest
;;       (let* ([first-tree (tree-forest forest)]
;;              [newforest (forest-forest forest)]
;;              [tree  (cut-tree2 cut-item first-tree)]
;;             )
;;         (if (cut-marker? tree)
;;             (make-tree tree newforest)                
;;             (make-tree tree (cut-forest2 cut-item newforest))))))
                                    


;;-------------------------------------------
;; This function paste a subtree into a cutted
;; tree marked as 'cutted
;;-------------------------------------------
(define (graft-subtree new-tree marked-tree)
  (cond ((eq? marked-tree cut-marker) new-tree)
        ((leaf? marked-tree) marked-tree)
        (else (make-tree
               (node marked-tree)
               (graft-forest new-tree
                           (leaves marked-tree))))))
;;-------------------------------------------
;; This function paste a tree into a marked
;; forest ('cutted) 
;;-------------------------------------------
(define (graft-forest new-tree marked-forest)
  (if (void-forest? marked-forest)   void-forest
      (make-tree
       (graft-subtree new-tree (tree-forest   marked-forest))
       (graft-forest  new-tree (forest-forest marked-forest)))))

;;-------------------------------------------
;; TEST FUNCTION: REQUIRES TESTING CUT-TREE DEBUGG FUNCTIONS
;;(define _set_tree (make-full-tree 3))
;;-------------------------------------------

;;-------------------------------------------
;; This function returns the number of leaves
;;-------------------------------------------
(define (count-leaves leaves)
  (length leaves))
;;-------------------------------------------

(define (change-leaf leaf leaves index)
  (list-set-in! leaves index leaf))
;;-------------------------------------------
;; This function returns all the leaves of a tree
;;-------------------------------------------
(define (extract-leaves tree)
  (if (leaf? tree)  (make-leaf tree)
      (extract-leaves-forest (leaves tree))))
;;-------------------------------------------
;; This function prunes the leaves of a forest
;; and returns them as a list
 ;;-------------------------------------------
(define (extract-leaves-forest forest)
    (if (void-forest? forest)  void-forest
        (append
         (extract-leaves        (tree-forest   forest))
         (extract-leaves-forest (forest-forest forest)))))
;;-------------------------------------------
;; This function cuts all the leaves of a tree
;; and are replaced with 'cutted mark 
;;-------------------------------------------
 (define (cut-leaves tree)
   (if (leaf? tree)  cut-marker
       (make-tree
        (node tree)
        (cut-leaves-forest (leaves tree)))))
;;-------------------------------------------
;; This function cuts all the leaves of a forest
;; and are replaced with 'cutted mark 
;;-------------------------------------------
(define (cut-leaves-forest forest)
  (if (void-forest? forest) void-forest
      (make-tree
       (cut-leaves (tree-forest forest))
       (cut-leaves-forest (forest-forest forest)))))

;;-------------------------------------------
;; This function returns the number of
;; leaves marked as cutted
;;-------------------------------------------
(define (count-pruned-items tree)
  (if (leaf? tree)
      (if (eq? tree cut-marker)  1  0)
      (count-pruned-items-forest (leaves tree))))
;;-------------------------------------------
;; This function counts the number of
;; leaves marked as cuttedn in a forest
;;-------------------------------------------
(define (count-pruned-items-forest forest)
  (if (void-forest? forest)   0
      (+ (count-pruned-items (tree-forest forest))
         (count-pruned-items-forest (forest-forest forest)))))

;;-------------------------------------------
;; This function removes the first required-leaves
;; from a list o new leaves and returned it.
;; If the list leaves is smaller than requiered
;; then returns null
;;-------------------------------------------
(define (remove-required-leaves list-leaves required-leaves)
  (let ([available-leaves (length list-leaves)])
    (if (< available-leaves required-leaves)
        void-tree
        (list-tail list-leaves required-leaves))))

;;-------------------------------------------        
;; This function replaces with new leaves
;; the marked as cutted
;;-------------------------------------------        
(define (graft-leaves leaves-list tree)
  (cond ((null? leaves-list) tree)
        ((and (leaf? tree) (cut-marker? tree)) (first leaves-list))
        ((leaf? tree) tree)
        (else (make-tree
               (node tree)
               (graft-leaves-forest leaves-list (leaves tree))))))
;;-------------------------------------------        
;; This function replaces with new leaves
;; the marked as cutted is a forest
;;-------------------------------------------        
(define (graft-leaves-forest list-leaves forest)
  (cond
    ((void-forest? forest) void-forest)
    ((null? list-leaves) forest)
    (else
     (let* ([tree (tree-forest forest)]
            [used-leaves (count-pruned-items tree)]
            [remaining-leaves (remove-required-leaves
                               list-leaves
                               used-leaves)])
       (make-tree
        (graft-leaves list-leaves tree)
        (graft-leaves-forest remaining-leaves
                           (forest-forest forest)))))))
;; --------------------------------
;; CHECK THIS FUNCTIONS
;;-------------------------------------------
;; This function prune an element of a tree
;; and return the pruned tree with the cutted mark
;; elemenmt could be a subtree or a list of leaves
;;-------------------------------------------
 (define (prune-tree element tree)
   (if (list-leaves? element)
       (cut-leaves tree)
       (cut-tree element tree)))
;; ;;-------------------------------------------
;; ;; This function set an element of a marked tree
;; ;; elemenmt could be a subtree or a list of leaves
;; ;;-------------------------------------------
 (define (graft-tree element tree)
   (if (list-leaves? element)
       (graft-leaves element tree)
       (graft-subtree element tree)))
;;-------------------------------------------

;;---------------------------------------------
;; Function compute how much a leaf will be
;; re-parameterized in a factor:
;; +-(0-100%) --> f:(0 1)
;;---------------------------------------------
(define (reparam-random-factor [percent 100])
  (let ([factor  (/ percent 100.0)])
    (round-float (+ (- 1.0 factor)
       (* (* 2.0 factor) (random))
       ))
       ))

;;---------------------------------------------
;; This function makes the numeric operation
;; of the leaf with a constant
;;---------------------------------------------
(define (scale-numeric-leaf k leaf)
  (round-float  (* k leaf)
  ))
;;---------------------------------------------
;; This function makes a tree with the scale
;; operation
;;---------------------------------------------
(define (scale-simbolic-leaf k leaf)
  (let ([leaves (make-leaves k leaf)])
    (make-tree '* leaves)))

;;---------------------------------------------
;; This function multiplies the value of
;; a leaf by a factor. If it is a numeric leaf,
;; the operation is made and another case a
;; tree is build.
;;---------------------------------------------
(define (scale-leaf leaf factor)
  (if (valid-symb? leaf)
      (scale-simbolic-leaf factor leaf)
      (scale-numeric-leaf  factor leaf)))
;;---------------------------------------------
;; This function scales a leaf in random factor
;;---------------------------------------------
(define (random-scale-leaf leaf)
  (let ([factor (reparam-random-factor)])
    (scale-leaf leaf factor))) 
;;---------------------------------------------
;; This function multiplies a set of leaves
;; by a set of scale factor
;;---------------------------------------------
(define (scale-leaves leaves factors)
  (map scale-leaf leaves factors))
;;---------------------------------------------
;; This function multiplies a set of leaves
;; by a random factor.
;;---------------------------------------------
(define (random-scale-leaves leaves)
  (map random-scale-leaf leaves))

;;---------------------------------------------
;; This function provide the way to multiplie
;; two trees
;;---------------------------------------------
(define (scale-tree tree scale)
  (make-btree '* scale tree))

;;---------------------------------------------
;; This function the way of combine two trees
;;---------------------------------------------
(define (add-tree tree1 tree2)
  (make-btree '+ tree1 tree2))
;; --------------------------------
;; Debug Functions
;; --------------------------------
;; (define debugg-mode #t)
;; (define tree-a (make-full-tree 1))
;; (define tree-b (make-full-tree 2))
;; (define tree-c (make-full-tree 4))
;; (define tree-d (make-full-tree 4))     

;(depth-tree tree-a)
;; (depth-tree tree-b)
;; (depth-tree tree-c)
;; (depth-tree tree-d)
;; (define tree-af (make-free-tree 10))
;; (define tree-bf (make-free-tree 10))
;; (define tree-cf (make-free-tree 10))
;; (define tree-df (make-free-tree 10))


;; (newline)(display "--------- TEST CODE --------")(newline)
;;  (define tree-a (make-full-tree 2))
;;  (define tree-c (make-full-tree 2))
;; (display "Tree a")(newline)
;; tree-a
;; (display "Tree c")(newline)
;; tree-c
;; (define branch-a (random-branch tree-a))
;; (define branch-c (random-branch tree-c))

;; (display "BY defnition a one-depth tree has not branches")(newline)
;; branch-a
;; (display "Random Branch of tree-c")(newline)
;; branch-c
;; (display "Cutting tree-c- returns a marked-tree-c")(newline)
;; (define cutted-tree-c (cut-tree branch-c tree-c))
;; cutted-tree-c
;; (display "Grafting tree-a in a marked-tree-c")(newline)
;; (define new-tree-c (graft-subtree tree-a cutted-tree-c))
;; new-tree-c

;; (define leaves-c (extract-leaves tree-c)) 
;; (display "Leaves tree-c")(newline) leaves-c
;; (define new-leaves (map (lambda (l) (make-tree '* (list 10 l))) leaves-c))
;; (display "Marked tree-c without leaves")(newline)
;; (define tree-c-wol (cut-leaves tree-c))
;; tree-c-wol
;; (display "Tree-c with new leaves")(newline)
;; (define tree-c-nl (graft-leaves new-leaves tree-c-wol))
;;  tree-c-nl


