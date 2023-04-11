#lang racket
(require 2htdp/batch-io)

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
;;; takes a string and outputs a hash of (word: amount-seen)
  (define (iter so-far to-go count)
    (if (empty? to-go)
      (list so-far count)
      (if (hash-has-key? so-far (first to-go))
        (iter (hash-set so-far (first to-go) (+ (hash-ref so-far (first to-go)) 1)) (rest to-go) (+ count 1))
        (iter (hash-set so-far (first to-go) 1) (rest to-go) (+ count 1)))))
  (let ([table (hash)])
    (iter table lst 0)))

(define (normalize table count)
;;; gets a hash table and changes the values into a normalized frequency of each word in the table
  (define (iter table lst count)
  (if (empty? lst)
    table
    (iter (hash-set table (first lst) (* (log (/ (hash-ref table (first lst)) count) 10) -1)) (rest lst) count))
    )
  (let ([lst (hash-keys table)])
  (iter table lst count)))