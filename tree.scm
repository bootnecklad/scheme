;; Develop a model for a binary tree.                                                 
;; - each node has a value                                                            
;; - each node may have a left child (tree)                                           
;; - each node may have a right child (tree)                                          
;; Write a function that will count the nodes in a tree
;;       10   <---root
;;    L /  \ R
;;     /    \
;;    5      19
;;   / \    /
;;  /   \  4
;; 3     9 



(use (srfi 1))

;;; Example tree
(define example-tree (make-node 10
                                (make-node 5
                                           (make-leaf 3)
                                           (make-leaf 9))
                                (make-node 19 
                                           (make-leaf 4)
                                           empty-tree)))

;;; Checks if thing is a node
(define node?
  (lambda (node)
    (and (list? node)
         (= (length node) 3)
         (tree? (node-left node))
         (tree? (node-right node)))))

;;; Makes a node
(define make-node
  (lambda (value child-left child-right)
    (list value child-left child-right)))

;;; Gets the value of a node
(define node-value
  (lambda (node)
    (car node)))

;;; Gets the child left of a node
(define node-left
  (lambda (node)
    (cadr node)))

;;; Gets the child right of a node
(define node-right
  (lambda (node)
    (caddr node)))

;;; Checks if a tree is empty
(define empty-tree?
  (lambda (tree)
    (eq? tree empty-tree)))

;;; Checks if the thing given is a tree
(define tree?
  (lambda (tree)
    (or (empty-tree? tree)
         (node? tree))))

;;; Makes a leaf
(define make-leaf
  (lambda (value)
    (make-node value empty-tree empty-tree)))

;;; Checks if the thing given is a leaf
(define leaf?
  (lambda (leaf)
    (and (node? leaf)
         (empty-tree? (node-left leaf))
         (empty-tree? (node-right leaf)))))

;;; Outputs the size of a given tree
(define tree-size
  (lambda (tree)
    (cond 
     ((empty-tree? tree) 0)
     ((node? tree) (+ (tree-size (node-right tree))
                      (tree-size (node-left tree))
                      1)))))

;#;12> (tree-size example-tree)
;6


;; bnl tests
;;       Q   <---root
;;      / \  
;;     /    \
;;    U      D
;;   /
;;  /
;; A

(define quad-tree (make-node 'Q
                             (make-node 'U
                                        (make-leaf 'A)
                                        empty-tree)
                             (make-leaf 'D)))

; #;9> (tree-size quad-tree)
; 4
