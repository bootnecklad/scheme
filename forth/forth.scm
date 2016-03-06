;;; A sort of Forth in Chicken Scheme
;;; 2016 (c) Copyright Marc Cleave
;;;

;;; always.
(define nil '())

;;; Yep.
(use (srfi 1))

;;; structure of a forth machine
(define-record forth
  stack
  words)

;;; actual things for the structure
(define (new-forth)
  (make-forth nil
              nil))

;;; primitive to push to data stack
(define set-stack!
  (lambda (processor item)
    (forth-stack-set! processor (cons item (forth-stack processor)))))

;;; returns increment of given argument
(define addone
  (lambda (x)
    (+ x 1)))

;;; returns decrement of given argument
(define subtractone
  (lambda (x)
    (- x 1)))

;;; pushes item to data-stack
(define push-data-stack!
  (lambda (processor word)
    (set-stack! processor word)))

;;; pops item off data-stack
(define pop-data-stack
  (lambda (processor)
    (let ((item (car (forth-stack processor))))
      (forth-stack-set! processor (cdr (forth-stack processor)))
      item)))

;;; does some things
(define controller
  (lambda ()
    (display "> ")
    (let ((user-input (read)))
      (if (list? user-input)
          (let* ((clean-input (clean-dirty-input user-input))
                 (result (evaluate clean-input)))
            result)
          (display "INVALID TITAN-FORTH\n")))
    (controller)))

;;; you have some strange idea of clean and dirty
(define clean-dirty-input
  (lambda (user-input)
    (cond
     ((null? user-input) nil)
     ((list? user-input) user-input)
     (else nil))))

;;; checks if first word is : (define new word)
(define evaluate
  (lambda (expr)
    (if (null? expr)
        nil
        (cond
         ((eq? (car expr) ':) (create-word expr))
         (else (execute-expression expr))))))

;;; deals with expressions, using pointers
(define execute-expression
  (lambda (expr)
    (if (null? expr)
        nil
        (let ((object (car expr)))
          (cond
           ((primitive? object) (execute-primitive object))
           ((quotation? object) (push-data-stack! forth-machine object))
           ((number? object) (push-data-stack! forth-machine object))
           ((boolean? object) (push-data-stack! forth-machine object))
           ((symbol? object) (evaluate (get-word-definition object))))
          (execute-expression (cdr expr))))))


(define set-stack!
  (lambda (processor item)
    (forth-stack-set! processor (cons item (forth-stack processor)))))

;;; adds word definition to word-list
(define create-word
  (lambda (input)
    (forth-words-set! forth-machine (list (cadr input) (cddr input)))))

;;; returns the definition of a word
(define get-word-definition
  (lambda (word)
    (let ((word-definition (member word (forth-words forth-machine))))
      (if word-definition
          (cadr word-definition)
          (begin (display "INVALID WORD: ")
                 (display word)
                 (display #\newline))))))

;;; returns whether a word is valid or not
(define valid-word?
  (lambda (object)
    (let ((word-definition (member object (forth-words forth-machine))))
      (if word-definition
          #t
          #f))))

;;; returns whether the given word is a primitive or not
(define primitive?
  (lambda (object)
    (member object forth-primitives)))

;;; returns whether the given word is a quotation or not
(define quotation?
  (lambda (object)
    (list? object)))

;;; defines the primitives for this forth implementation
(define forth-primitives '(: + - * / =? <? >? IF CALL DUP DROP DISP PRINT ?BRANCH BRANCH CR))

;;; yep.
(define execute-primitive
  (lambda (word)
    (cond
     ((eq? word '+) (push-data-stack! forth-machine (+ (pop-data-stack forth-machine)
                                                       (pop-data-stack forth-machine))))
     ((eq? word '-) (push-data-stack! forth-machine (let ((A (pop-data-stack forth-machine))
                                                          (B (pop-data-stack forth-machine)))
                                                      (- B A))))
     ((eq? word '*) (push-data-stack! forth-machine (* (pop-data-stack forth-machine)
                                                       (pop-data-stack forth-machine))))
     ((eq? word '/) (push-data-stack! forth-machine (let ((A (pop-data-stack forth-machine))
                                                          (B (pop-data-stack forth-machine)))
                                                      (/ B A))))
     ((eq? word '=?) (if (= (pop-data-stack forth-machine)
                            (pop-data-stack forth-machine))
                         (push-data-stack! forth-machine #t)
                         (push-data-stack! forth-machine #f)))
     ((eq? word '<?) (if (> (pop-data-stack forth-machine)
                            (pop-data-stack forth-machine))
                         (push-data-stack! forth-machine #t)
                         (push-data-stack! forth-machine #f)))
     ((eq? word '>?) (if (< (pop-data-stack forth-machine)
                            (pop-data-stack forth-machine))
                         (push-data-stack! forth-machine #t)
                         (push-data-stack! forth-machine #f)))
     ((eq? word 'DROP) (pop-data-stack forth-machine))
     ((eq? word 'DUP) (push-data-stack! forth-machine (car (forth-stack forth-machine))))
     ((eq? word 'DISP) (display (pop-data-stack forth-machine)))
     ((eq? word 'PRINT) (let ((char? (pop-data-stack forth-machine)))
                          (if char?
                              (display (integer->char char?)))))
     ((eq? word 'CR) (display #\newline))
     ((eq? word 'CALL) (evaluate (pop-data-stack forth-machine)))
     ((eq? word 'IF) (let* ((FALSE-CASE (pop-data-stack forth-machine))
                            (TRUE-CASE (pop-data-stack forth-machine)))
                       (if (pop-data-stack forth-machine)
                           (evaluate TRUE-CASE)
                           (evaluate FALSE-CASE)))))))

(define forth-machine (new-forth))

(controller)


;;; awesome function definitions


(: zero? #|n -- ?|# 0 =?)                                                                    
(: half 2 /)                                                             
(: double 2 *)                                                  
(: when #|? thing --|# () if)                                            
(: unless #|? thing --|# () swap if)                                     
(: not #|? -- ?|# (#f) (#t) if)                                          
(: loop #|thing --|# dup call loop)
(: negative? 0 <)
(: abs dup negative? (-) () if)                 
(: even? dup zero? (drop #t) (abs 1 - odd?) if) 
(: odd? dup zero? (drop #f) (abs 1 - even?) if) 
