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

(define (make-reference-hash lst)
  (define (iter so-far to-go count)
    (if (empty? to-go)
    (list so-far count)
    (if (hash-ref so-far (first to-go) #f)
    (iter (hash-set so-far (first to-go) (+ (hash-ref so-far (first to-go)) 1)) (rest to-go) (+ count 1))
    (iter (hash-set so-far (first to-go) 1) (rest to-go) (+ count 1)))))
  (let ([table (hash)])
    (iter table lst 0)))

(define (normalize table lst count)
    (if (empty? lst)
    table
    (normalize (hash-set table (first lst) (* (log (/ (hash-ref table (first lst)) count) 10) -1)) (rest lst) count)))

(define lovecraft (process-file "Lovecraft.txt"))
(define Doyle (process-file "Doyle.txt"))

(define pre-normal-lovecraft(make-reference-hash lovecraft))
(define pre-normal-doyle(make-reference-hash Doyle))

(normalize (first pre-normal-doyle) Doyle (second pre-normal-doyle))