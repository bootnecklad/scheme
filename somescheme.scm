(define (string-split str i)
  (list
   (substring str 0 i)
   (substring str i (string-length str))))

(define (break-string input-string position)
  (delete-char (car (string-split input-string position)) (- position 1)))

(define (delete-char str i)
  (string-append (substring str 0 i) (cadr (string-split (cadr (string-split str i)) 1))))

(define (lower->upper input-string)
  (integer->char (+ -32 (char->integer (string-ref input-string 0)))))

(define (string-join input-list)
  (if (null? input-list)
      ""
      (string-append (car input-list) (string-join (cdr input-list)))))

(define (change-case input-string)
  (string-join (list (string (lower->upper input-string)) (delete-char input-string 0))))

(define (make-sentence input-list)
  (if (null? input-list)
      ""
      (string-append (car input-list) " " (make-sentence (cdr input-list)))))


(define (string-titlecase input-string)
  
(define input-list (string->string-list input-string))
  
(define (change-titlecase input-list)
  (if (null? input-list)
      ""
      (string-append (change-case (car input-list)) " " (change-titlecase (cdr input-list)))))
  

(delete-char (change-titlecase input-list) (+ -1 (string-length (change-titlecase input-list)))))

;;; below not made by bnl
;;; i have no idea how to lay it out
;;; http://stackoverflow.com/questions/7691769/string-split-function
;;; Quadrescence | nonportable implementation specific

(define (char->string c)
    (make-string 1 c))

  (define (string-first-char str)
    (string-ref str 0))

  (define (string-first str)
    (char->string (string-ref str 0)))

  (define (string-rest str)
    (substring str 1 (string-length str)))

(define (string->string-list str)

  (define (string-split-helper str chunk lst)
  (cond 
    ((string=? str "") (reverse (cons chunk lst)))
    (else
       (cond
          ((char=? (string-first-char str) #\space) (string-split-helper (string-rest str) "" (cons chunk lst)))
    (else
       (string-split-helper (string-rest str) (string-append chunk (string-first str)) lst))))))

  (string-split-helper str "" '()))

