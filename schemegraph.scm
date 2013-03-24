(define nil '())

(define (in-list? x lst)
  (if (null? lst)
      #f
      (or (eq? x (car lst))
          (in-list? x (cdr lst)))))

(define (make-the-graph-mang)
  (let ((a (cons nil nil))  ; ALLOCATE
        (b (cons nil nil))  ;   THE
        (c (cons nil nil))  ;  NODES
        (d (cons nil nil))  ;  RIGHT
        (e (cons nil nil))  ;  HERE
        (f (cons nil nil))) ; and have f

    ;; LINK THE THANGS UP
    (set-car! a b) ; set a -> b
    (set-car! b c) ; set b -> c
    (set-car! c e) ; set c -> e
    (set-car! e d) ; set e -> d
    (set-cdr! e f) ;       -> f
    (set-car! d b) ; set d -> b
    (set-car! f d) ; set f -> d

    ;; RETURN a
    a))

(define example-graph (make-the-graph-mang))

(define (count-nodes graph)
  (let ((visited nil) (count 0))
    (define (count-nodes-inner graph)
      (if (null? graph) ; is our graph empty?
          0
          (if (in-list? graph visited) ; have we visited the node?
              0
              (let ((newly-visited (cons graph visited)))
                (set! visited newly-visited)
                (set! count (+ count 1))
                   (count-nodes-inner (car graph))
                   (count-nodes-inner (cdr graph))))))
    
    ;; Call the inner function
    (count-nodes-inner graph)
  
  count))

(define (weird-graph)
  ;; MAKES THIS GRAPH
  ;;   ______   _
  ;;  /      v / \
  ;; A       B<--/
  ;; ^------/
  (let ((a (cons nil nil))
        (b (cons nil nil)))
    (set-car! a b)
    (set-car! b a)
    (set-car! b b)
    
    ;; Return the graph
    a))


                             