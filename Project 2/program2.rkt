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
    (if (hash-has-key? so-far (first to-go))
    (iter (hash-set so-far (first to-go) (+ (hash-ref so-far (first to-go)) 1)) (rest to-go) (+ count 1))
    (iter (hash-set so-far (first to-go) 1) (rest to-go) (+ count 1)))))
  (let ([table (hash)])
    (iter table lst 0)))

(define (normalize table count)
  (define (iter table lst count)
  (if (empty? lst)
    table
    (iter (hash-set table (first lst) (* (log (/ (hash-ref table (first lst)) count) 10) -1)) (rest lst) count))
    )
  (let ([lst (hash-keys table)])
  (iter table lst count)))
    

(define (compare text-1 text-2)
  (define (iter text-1 text-2 keys compared)
    (if (empty? keys)
      compared
      (if (hash-has-key? text-1)
        (hash-set compared (first keys) (abs(- (hash-ref text-1 (first keys)) (hash-ref text-2 (first keys)))))
        (iter text-1 text-2 (rest keys) compared))))
  (let ([keys (hash-keys text-2)]
        [compared (hash-copy text-2)])
    (iter text-1 text-2 keys compared)))

(define (decision from-lovecraft from-doyle)
  from-doyle)

(define pre-normal-lovecraft(make-reference-hash (process-file "Lovecraft.txt")))
(define pre-normal-doyle(make-reference-hash (process-file "Doyle.txt")))
(define pre-normal-mystery1(make-reference-hash (process-file "mystery1.txt")))
(define pre-normal-mystery2(make-reference-hash (process-file "mystery2.txt")))

(define normal-doyle (normalize (first pre-normal-doyle) (second pre-normal-doyle)))
(define normal-lovecraft (normalize (first pre-normal-lovecraft) (second pre-normal-lovecraft)))
(define normal-mystery1 (normalize (first pre-normal-mystery1) (second pre-normal-mystery1)))
(define normal-mystery2 (normalize (first pre-normal-mystery2) (second pre-normal-mystery2)))
