#lang racket
(require 2htdp/batch-io)

(define (process-file f1)
  (string-split (string-normalize-spaces (clean-string (string-downcase(read-file f1)))) " "))

(define (clean-string str)
  (define (clean marks str)
    (if (empty? marks)
    str
    (clean (rest marks) (string-replace str (first marks) " "))))
  (let
    ([deletes (list "\n" "\"" "?" "," "." "!" ";" "-" "_" ":")])
    (clean deletes str)))

(process-file "Doyle.txt")
(process-file "Lovecraft.txt")