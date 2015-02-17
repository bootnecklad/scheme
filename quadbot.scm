;;; the beginnings of a quadbot
;;;
;;; when bnl wanted Quadrescence around when Quadrescence isnt actually around
;;;

(use (srfi 1))
(use (srfi 13))
(use (srfi 14))

;;; qu1j0t3 | i just use '() like a Big Boy
(define nil '())

;;; used when writing in the middle of writing/testing functions
(define (not-implemented)
  (error "not implemented yet"))

;;; defines the allowed character set 
(define allowed-charset
  (char-set-union char-set:lower-case char-set:digit (string->char-set " ")))

;;; checks if input character is in the allowed character set
(define (allowed-char? char)
  (char-set-contains? allowed-charset char))

;;; removes everything that isnt in the allowed character set
(define (remove-unwanted input-string)
  (list->string (filter allowed-char? (string->list input-string))))

;;; "write a function called 'words' which takes a sentence and
;;; produces a list of words in that sentence without punctuation"
(define (words input-string)
  (string-split (remove-unwanted (string-downcase input-string))))
