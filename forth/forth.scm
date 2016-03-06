;;; A sort of Forth in Chicken Scheme
;;; 2016 (c) Copyright Marc Cleave
;;;

;;; always.
(define nil '())

;;; Yep.
(use (srfi 1))

;;; throwback to 2011
(define SUPER-IMPORTANT-NUMBER-<QUAD>-WHATS-A-BUS? 16)

(define address-bus-size SUPER-IMPORTANT-NUMBER-<QUAD>-WHATS-A-BUS?)
(define stack-pointer-size SUPER-IMPORTANT-NUMBER-<QUAD>-WHATS-A-BUS?)
(define data-bus-size SUPER-IMPORTANT-NUMBER-<QUAD>-WHATS-A-BUS?)
(define number-of-registers SUPER-IMPORTANT-NUMBER-<QUAD>-WHATS-A-BUS?)

;;; defines the forth processor
(define-record forth-processor
  memory
  data-stack
  return-stack
  registers
  word-pointer
  compile/interpret) ; compile -> #f / interpret -> #t
                     ; default is compile mode

;;; processor definitions bus widths and bits and bobs
(define (new-forth-processor)
  (make-forth-processor (make-vector (expt 2 address-bus-size) 0)
                        (make-vector (expt 2 stack-pointer-size) 0)
                        (make-vector (expt 2 stack-pointer-size) 0)
                        (make-vector (* SUPER-IMPORTANT-NUMBER-<QUAD>-WHATS-A-BUS? 2) 0)
                        (make-vector (expt 2 (/ SUPER-IMPORTANT-NUMBER-<QUAD>-WHATS-A-BUS? 2)) nil)
                        (make-vector 1 #t)))

;;; reads given address in memory
;;; address #x0000 always returns 0, don't ask. it is what it is.
(define (read-memory forth-processor address)
  (case address
    ((#x0000) 0)
    (else (vector-ref (forth-processor-memory forth-processor) address))))

;;; writes word to given address in memory
(define (write-memory! forth-processor address word)
  (case address
    ((+ address-bus-size 1) (error "How the !*@# did you get here?!"))
    (else (vector-set! (forth-processor-memory forth-processor) address word))))

;;; just read
(define register-numerics
  (iota number-of-registers))

;;; this is a USER-CHANGEABNL list... for names and stuff maybe, not really though?
(define register-names
  '(data-stack-pointer
    return-stack-pointer
    word-pointer
    general-purpose-01
    general-purpose-02
    general-purpose-03))

;;; ''zips up`` two lists (if one is shorter, itll cut off maybe) together
(define zip-up
  (lambda (names numerics)
    (if (or (null? names)
            (null? numerics))
        nil
        (cons (list (car names) (car numerics))
              (zip-up (cdr names) (cdr numerics))))))

;;; just read the line below...
(define register-associations (zip-up register-names register-numerics))

;;; fetches the numeric value associated with its name
(define get-numeric-of
  (lambda (register-name)
    (let ((register-numeric (member register-name (flatten register-associations))))
      (if register-numeric
          (cadr register-numeric)
          (error "INVALID REGISTER NAME")))))

;;; returns increment of given argument
(define addone
  (lambda (x)
    (let ((y 1))
      (+ x y))))

;;; returns decrement of given argument
(define subtractone
  (lambda (x)
    (let ((y 1))
      (- x y))))

;;; reads given register from vector of all registers in processor
(define read-register
  (lambda (forth-processor register-name)
    (vector-ref (forth-processor-registers forth-processor) (get-numeric-of register-name))))

;;; pushes item to data-stack
(define push-data-stack!
  (lambda (forth-processor word)
    (vector-set! (forth-processor-data-stack forth-processor) (read-register forth-processor 'data-stack-pointer) word)
    (vector-set! (forth-processor-registers forth-processor) (get-numeric-of 'data-stack-pointer) (addone (read-register forth-processor 'data-stack-pointer)))))

;;; pops item off data-stack
(define pop-data-stack
  (lambda (forth-processor)
    (vector-set! (forth-processor-registers forth-processor) (get-numeric-of 'data-stack-pointer) (subtractone (read-register forth-processor 'data-stack-pointer)))
    (vector-ref (forth-processor-data-stack forth-processor) (read-register forth-processor 'data-stack-pointer))))

;;; pushes item to return-stack
(define push-return-stack!
  (lambda (forth-processor word)
    (vector-set! (forth-processor-return-stack forth-processor) (read-register forth-processor 'return-stack-pointer) word)
    (vector-set! (forth-processor-registers forth-processor) (get-numeric-of 'return-stack-pointer) (addone (read-register forth-processor 'return-stack-pointer)))))

;;; pops item off return-stack
(define pop-return-stack
  (lambda (forth-processor)
    (vector-set! (forth-processor-registers forth-processor) (get-numeric-of 'return-stack-pointer) (subtractone (read-register forth-processor 'return-stack-pointer)))
    (vector-ref (forth-processor-return-stack forth-processor) (read-register forth-processor 'return-stack-pointer))))

;;; returns mode of operation, compile or interpret
(define current-mode?
  (lambda (forth-processor)
    (vector-ref (forth-processor-compile/interpret forth-processor) 0)))

;;; changes mode of processor, compile #f, interpret #t
(define change-mode!
  (lambda (forth-processor mode)
    (cond
     ((symbol? mode) (case mode
                       ((COMPILE) (vector-set! (forth-processor-compile/interpret forth-processor) 0 #f))
                       ((INTERPRET) (vector-set! (forth-processor-compile/interpret forth-processor) 0 #t))))
     ((boolean? mode) (vector-set! (forth-processor-compile/interpret forth-processor) 0 mode)))))

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
           ((quotation? object) (push-data-stack! forth-thing object))
           ((number? object) (push-data-stack! forth-thing object))
           ((boolean? object) (push-data-stack! forth-thing object))
           ((symbol? object) (evaluate (get-word-definition object))))
          (execute-expression (cdr expr))))))

;;; adds word definition to word-list
(define create-word
  (lambda (input)
    (vector-set! (forth-processor-word-pointer forth-thing)
                 (read-register forth-thing 'word-pointer)
                 (list (first (cdr input)) (read-register forth-thing 'word-pointer) (cdr input)))
    (vector-set! (forth-processor-registers forth-thing)
                 (get-numeric-of 'word-pointer)
                 (addone (read-register forth-thing 'word-pointer)))))

;;; returns pointer where word is in word-pointer
(define word-pointer?
  (lambda (word)
    (let ((word-definition (member word (flatten (vector->list (forth-processor-word-pointer forth-thing))))))
      (if word-definition
          (cadr word-definition)
          #f))))

;;; returns whether a word is valid or not
(define valid-word?
  (lambda (object)
    (if (word-pointer? object)
        #t
        #f)))

;;; returns the definition of a word
(define get-word-definition
  (lambda (word)
    (cdaddr (vector-ref (forth-processor-word-pointer forth-thing) (word-pointer? word)))))

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
     ((eq? word '+) (push-data-stack! forth-thing (+ (pop-data-stack forth-thing)
                                                     (pop-data-stack forth-thing))))
     ((eq? word '-) (push-data-stack! forth-thing (let ((A (pop-data-stack forth-thing))
                                                        (B (pop-data-stack forth-thing)))
                                                    (- B A))))
     ((eq? word '*) (push-data-stack! forth-thing (* (pop-data-stack forth-thing)
                                                     (pop-data-stack forth-thing))))
     ((eq? word '/) (push-data-stack! forth-thing (let ((A (pop-data-stack forth-thing))
                                                        (B (pop-data-stack forth-thing)))
                                                    (/ B A))))
     ((eq? word '=?) (if (= (pop-data-stack forth-thing)
                            (pop-data-stack forth-thing))
                         (push-data-stack! forth-thing #t)
                         (push-data-stack! forth-thing #f)))
     ((eq? word '<?) (if (> (pop-data-stack forth-thing)
                            (pop-data-stack forth-thing))
                         (push-data-stack! forth-thing #t)
                         (push-data-stack! forth-thing #f)))
     ((eq? word '>?) (if (< (pop-data-stack forth-thing)
                            (pop-data-stack forth-thing))
                         (push-data-stack! forth-thing #t)
                         (push-data-stack! forth-thing #f)))
     ((eq? word 'DROP) (pop-data-stack forth-thing))
     ((eq? word 'DUP) (push-data-stack! forth-thing (vector-ref (forth-processor-data-stack forth-thing)
                                                                (subtractone (read-register forth-thing 'data-stack-pointer)))))
     ((eq? word 'DISP) (display (pop-data-stack forth-thing)))
     ((eq? word 'PRINT) (let ((char? (pop-data-stack forth-thing)))
                          (if char?
                              (display (integer->char char?)))))
     ((eq? word 'CR) (display #\newline))
     ((eq? word 'CALL) (evaluate (pop-data-stack forth-thing)))
     ((eq? word 'IF) (let* ((FALSE-CASE (pop-data-stack forth-thing))
                            (TRUE-CASE (pop-data-stack forth-thing)))
                       (if (pop-data-stack forth-thing)
                           (evaluate TRUE-CASE)
                           (evaluate FALSE-CASE)))))))

;;; removes conditions
(define remove-conditionals
  (lambda (expr)
    (let* ((removed-IF (convert-conditionals expr 'IF))
           (removed-ELSE (convert-conditionals removed-IF 'ELSE))
           (finalised-expr (convert-conditionals removed-ELSE 'THEN)))
      finalised-expr)))

;;; converts conditionals IF/ELSE/THEN into ?BRANCH and BRANCH
(define convert-conditionals
  (lambda (expr condit)
    (if (null? expr)
        nil
        (case condit
          ((IF) (append (if (eq? (car expr) condit)
                            (list '?BRANCH (next-thing (cdr expr) 0))
                            (list (car expr)))                
                        (convert-conditionals (cdr expr) condit)))
          ((ELSE) (append (if (eq? (car expr) condit)
                              (list 'BRANCH (next-thing (cdr expr) 0))
                              (list (car expr)))                 
                          (convert-conditionals (cdr expr) condit)))
          ((THEN) (if (member 'THEN expr)
                      (convert-conditionals (remove-item 'THEN expr) 'THEN)
                      expr))))))

;;; "deletes" item from a list, BAD BAD BAD
;;; http://stackoverflow.com/questions/1905222/how-to-delete-an-element-from-a-list-in-scheme
(define remove-item
  (lambda (item list)
    (cond
     ((equal? item (car list)) (cdr list))
     (else (cons (car list) (delete item (cdr list)))))))

;;; count how many things until the next one
(define next-thing
  (lambda (expr count)
    (if (null? expr)
        nil
        (if (or (eq? (car expr) 'ELSE)
                (eq? (car expr) 'THEN))
            (+ count 2)
            (next-thing (cdr expr) (addone count))))))

;;; defines a forth-processor
(define forth-thing (new-forth-processor))

(controller)


;;; awesome function definitions


(: zero? #|n -- ?|# 0 =?)                                                                    
(: half 2 /)                                                             
(: double 2 *)                                                           
Quadrescence-forth-lib                                                   
(: when #|? thing --|# () if)                                            
(: unless #|? thing --|# () swap if)                                     
(: not #|? -- ?|# (#f) (#t) if)                                          
(: loop #|thing --|# dup call loop)
(: negative? 0 <)
(: abs dup negative? (-) () if)                 
(: even? dup zero? (drop #t) (abs 1 - odd?) if) 
(: odd? dup zero? (drop #f) (abs 1 - even?) if) 
