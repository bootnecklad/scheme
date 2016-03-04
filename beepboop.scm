(define nil '())

(define beep-boop
  (lambda (input-string)
    (intlist->beops (string->intlist (recursive-append (string->binary input-string))))))

(define string->intlist
  (lambda (input-string)
    (map char->integer (string->list input-string))))

(define string->binary
  (lambda (input-string)
    (map (lambda (char-value) (number->string char-value 2))
         (string->intlist input-string))))

(define recursive-append
  (lambda (input-list)
    (if (null? input-list)
        ""
        (string-append (car input-list) (recursive-append (cdr input-list))))))

(define intlist->beops
  (lambda (int-list)
    (if (null? int-list)
        "*"
        (string-append (case (car int-list)
                         ((48) "*BOOP")
                         ((49) "*BEEP"))
                       (intlist->beops (cdr int-list))))))
