;;; insert-to - takes a list and inserts something
;;; between every item in the list
;;; 
;;; spaceify - take a string and
;;; m a k e   i t   h a v e   s p a c e s
;;;
;;; #;23> (spacey "Scheme is the best!")
;;; "S c h e m e   i s   t h e   b e s t ! "
;;;

(define nil '())

(define insert-to
  (lambda (lst itm)
    (if (null? lst)
        nil
        (cons (first lst)              
              (cons itm (insert-to (cdr lst) itm))))))
                
                
(define spaceify
  (lambda (str)
    (list->string (insert-to (string->list str) #\space))))
