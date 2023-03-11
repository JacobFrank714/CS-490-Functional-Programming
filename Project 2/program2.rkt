#lang racket
(require 2htdp/batch-io)
;;; turns the input file name and turns it into a list of strings
(define (process-file f1)
  (string-split (string-normalize-spaces (clean-string (string-downcase(read-file f1)))) " "))
;;; takes all unwanted characters out of the input string
(define (clean-string str)
  ;;; replaces the first character in the marks list with a space until marks empty
  (define (clean marks str)
    (if (empty? marks)
    str
    (clean (rest marks) (string-replace str (first marks) " "))))
  (let
    ([deletes (list "\n" "\"" "?" "," "." "!" ";" "-" "_" ":")])
    (clean deletes str)))


(process-file "Lovecraft.txt")

(define (make-reference-hash lst)
  (define (iter so-far to-go)
    (if (empty? to-go)
    so-far
    (iter (hash-set so-far (first to-go) 1) (rest to-go))))
  (let ([table (hash)])
    (iter table lst)))

(make-reference-hash (process-file "Doyle.txt"))