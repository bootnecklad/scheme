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
      (string-append (car input-list) " " (string-join (cdr input-list)))))



