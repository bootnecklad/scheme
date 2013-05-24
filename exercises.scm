; Sum of squares

(define (perf n) ; defines function
  (let loop ((i 0) (x 0))
    (if (> i n) ; checks iteration number
        (display x) ; displays final result
        (loop (+ i 1) (+ x (expt i 2)))))) ; performs the 1^2 + ... + N^2 and increments iteration counter

; Output:
;
; > (perfect 30)
; 9456


; Approximating pi

(define pi 3.1415926535897932384626433832795028841971693993751058209)

(define (approx-pi n)
  (let loop ((i 0) (x 0))
    (if (eq? i n)
        (* (sqrt 12) x)
        (loop (+ i 1) (+ x (/ (expt -3 (- i))(+ 1 (* 2 i))))))))

; Output:
;
; > (approx-pi 1000)
; 3.141592653589793
;
; I can't be bothered to extend the length, this will do!


(define (pi-list n)
  (let loop ((i 0))
    (if (> i n)
        '()
        (cons (approx-pi i) (loop (+ i 1))))))

; Output (I'll sort out that 0 later...):
;
; > (pi-list 20)
; (0
; 3.4641016151377544
; 3.0792014356780038
; 3.156181471569954
; 3.1378528915956805
; 3.1426047456630846
; 3.141308785462883
; 3.1416743126988376
; 3.141568715941784
; 3.141599773811506
; 3.1415905109380797
; 3.1415933045030813
; 3.141592454287646
; 3.1415927150203795
; 3.1415926345473135
; 3.141592659521713
; 3.141592651733997
; 3.1415926541725754
; 3.141592653406165
; 3.141592653647826
; 3.141592653571403)

(define (accuracy n)
  (let loop ((i 0))
    (if (> i n)
        '()
        (cons (- pi (approx-pi i)) (loop (+ i 1))))))

; Output
;
; > (cdr (accuracy 5))
; (-0.32250896154796127 0.062391217911789365 -0.014588817980160762 0.0037397619941126337 -0.0010120920732914485)
; > (cdr (pi-list 5))
; (3.4641016151377544 3.0792014356780038 3.156181471569954 3.1378528915956805 3.1426047456630846)
;
; Looks correct?

; Hailstone sequences

(define (hailstone n)
  (let loop ((i n))
    (cons i (cond ((eq? i 1) '())
                  ((even? i) (loop (/ i 2)))
                  (else (loop (+ 1 (* 3 i))))))))

; Output
;
; > (hailstone 1)
; (1)
; > (hailstone 2)
; (2 1)
; > (hailstone 3)
; (3 10 5 16 8 4 2 1)
;
; I guess it works!

(define (hailstone-lst n)
  (let ((n (+ n 1)))
  (let loop ((i 1))
    (if (eq? i n)
        '()
        (cons (hailstone i) (loop (+ i 1)))))))

; Output:
;
; > (hailstone-lst 10)
;((1)
; (2 1)
; (3 10 5 16 8 4 2 1)
; (4 2 1)
; (5 16 8 4 2 1)
; (6 3 10 5 16 8 4 2 1)
; (7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
; (8 4 2 1)
; (9 28 14 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
; (10 5 16 8 4 2 1))
        
; generates next value of hailstone sequence
; dont do n=0 ... bad things!
(define (hailstone-next n)
  (if (even? n)
       (/ n 2)
       (+ 1 (* 3 n))))

; returns length of hailstone sequence
(define (hailstone-length n)
  (+ 1 (if (eq? n 1)
       0
       (hailstone-length(hailstone-next n)))))

; returns (0 1 ... n-1)
(define (iota n)
  (let loop ((i 0))
    (if (> i (- n 1))
        '()
        (cons i (loop (+ i 1))))))

; returns lengths of hailstone sequences
(define (hailstone-length-gen n)
  (map hailstone-length (cdr (iota (+ n 1)))))

; returns average length of hailstone sequence up to hailstone n
(define (avg-lst input-list)
  (/ (apply + input-list) (length input-list)))

; returns the shortest sequence between hailstone 1 and hailstone n
(define (hailstone-shortest input-list)
  
--------------------------------------------


;(define (print-full x)
;  (if (pair? x)
;      (begin
;        (display "(")
;        (print-full (car x))
;        (display " . ")
;        (print-full (cdr x))
;        (display ")"))
;      (write x)))
;
;(define nil '())
;(define cons-test (cons 1 2))
;(define (reverse-cons c) (cons (cdr c) (car c)))
;(define (list-one input-thing) (cons input-thing nil))
;
;; (define (<function> <input-list>)
;;  (if (null? <input-list>)
;;      <return>
;;      (<do this> (car <input-list>) (<function> (cdr <input-list>)))))
;
;(define (reverse-list input-list)
;  (if (null? input-list)
;      nil
;      (append (reverse-list (cdr input-list)) (cons (car input-list) nil))))
;
;(define (string-join input-list)
;  (if (null? input-list)
;      ""
;      (string-append (string-append (car input-list) " ") (string-join (cdr input-list)))))
;
;(define (string-split str i) (list (substring str 0 i) (substring str i (string-length str))))
;(define (delete-char str i) (list (string-append (substring str 0 i) (cadr (string-split (cadr (string-split str i)) 1)))))
;
;(define str '("hue" "land" "rover"))
;(string-join str)
;(define test-str (string-join str))
;(delete-char test-str 14)


;;; I DIDN'T WRITE THIS PART!
;;; My scheme interperter did not have a random function
;;; http://www.math.grin.edu/~stone/events/scheme-workshop/random.html

(define random-maker
  (let* ((multiplier 48271)
         (modulus 2147483647)
         (apply-congruence
          (lambda (current-seed)
            (let ((candidate (modulo (* current-seed multiplier)
                                     modulus)))
              (if (zero? candidate)
                  modulus
                  candidate))))
         (coerce
          (lambda (proposed-seed)
            (if (integer? proposed-seed)
                (- modulus (modulo proposed-seed modulus))
                19860617))))  ;; an arbitrarily chosen birthday
  (lambda (initial-seed)
    (let ((seed (coerce initial-seed)))
      (lambda args
        (cond ((null? args)
               (set! seed (apply-congruence seed))
               (/ (- modulus seed) modulus))
              ((null? (cdr args))
               (let* ((proposed-top
                       (ceiling (abs (car args))))
                      (exact-top
                       (if (inexact? proposed-top)
                           (inexact->exact proposed-top)
                           proposed-top))
                      (top
                       (if (zero? exact-top)
                           1
                           exact-top)))
                 (set! seed (apply-congruence seed))
                 (inexact->exact (floor (* top (/ seed modulus))))))
              ((eq? (cadr args) 'reset)
               (set! seed (coerce (car args))))
              (else
               (display "random: unrecognized message")
               (newline))))))))

(define delete
  (lambda (item list)
    (cond
     ((equal? item (car list)) (cdr list))
     (else (cons (car list) (delete item (cdr list)))))))

;;; Can you tell this is mine? Crude.

(define nil '()) ; makes code look cleaner
(define random (random-maker 4343543))
(define i 0)
(define c 0)
;(define a 0)
(define some-list '(1 2 3 4 5 6 7 8 9)) ; this is the list to be worked on

(define (create-lst n)
  (let loop ((i 1))
    (if (> i n)
        '()
        (cons i (loop (+ i 1))))))

(define (display-list input-list)
  (if (null? input-list)
      nil
      (begin (display (car input-list)) (display-list (cdr input-list)))))

(define (count-list input-list)
  (if (null? input-list)
      i
      (begin (set! i (+ 1 i)) (count-list (cdr input-list)))))

(define (fetch-nth input-list a)
  (if (eq? a 0)
      (car input-list)
      (fetch-nth (cdr input-list) (- a 1))))

(define (replace-nth list n elem)
  (cond 
    ((null? list) '())
    ((eq? n 0) (cons elem (cdr list)))
    (#t (cons(car list) (replace-nth (cdr list) (- n 1) elem)))))

(define (fisher-yates input-list)
  (let loop ((n (count-list input-list)))
  (if (eq? n 0)
      '()
      ((replace-nth input-list n (fetch-nth input-list (random n))) (loop (- n 1))))))

;(cons (fetch-nth input-list (random n)) (loop (- n 1)))

(define (derp n)
  (set! c (random n))
  ;(set! some-list (delete c some-list))
   c)

(display-list some-list)
;(count-list some-list)
;(fisher-yates some-list)

(define (doagain) (begin (set! i 0) (fisher-yates some-list)))


  
    
    
    