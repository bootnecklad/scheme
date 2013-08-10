(define test '((1 2 3)
               (4 5 6)))

(define (pr . items)
  (for-each display items)
  (newline))


;;; displays the size of a matrix
;;;
;;; #;35> (matrix-size matrix)
;;; Columns: 2
;;; Rows   : 3

(define (print-matrix-size matrix)
  (pr "Rows   : " (matrix-height matrix))
  (pr "Columns: " (matrix-width matrix)))

;;; returns a list of the size of a matrix
;;; (columns rows)
;;;
;;; #;37> (matrix-size-lst matrix)
;;; (2 3)

(define (matrix-height matrix)
  (length matrix))

(define (matrix-width matrix)
  (if (null? matrix)
      0
      (length (car matrix))))

(define (matrix-dimensions matrix)
  (list (matrix-height matrix)
        (matrix-width matrix)))

;;; returns total number of elements in the matrix
;;;
;;; #;18> (define matrix '((0 0) (0 0)))
;;; #;19> (matrix-no-element matrix)
;;; 4

(define (matrix-total-size matrix)
  (apply * (matrix-dimensions matrix)))

;;; gets the nth element in a list
;;;

(define (list-element lst n)
  (if (null? lst)
      #f
      (if (zero? n)
          (car lst)
          (list-element (cdr lst) (- n 1)))))

;;; gets the m,n element in a matrix
;;;

(define (matrix-element matrix row column)
  (list-element (list-element matrix row) column))

(define (get-column matrix n)
  (map (lambda (row)
         (list-element row n))
       matrix))

(define (do-thing matrix n)
  (if (null? matrix)
      '()
      (if (= (matrix-width matrix) n)
          '()
          (cons (get-column matrix n) (do-thing matrix (+ n 1))))))

(define (transpose matrix)
  (do-thing matrix 0))

;;; '((1 2 3)
;;;   (4 5 6))
;;;
;;; | 1 2 3 |
;;; | 4 5 6 |

;;; '((1 4)
;;;   (2 5)
;;;   (3 6))
;;;
;;; | 1 4 |
;;; | 2 5 |
;;; | 3 6 |