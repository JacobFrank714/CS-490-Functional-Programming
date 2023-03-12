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
    

(define (compare text-1 text-2)
;;; takes in the hashes from the inputs and outputs the difference between the word frequencies of the two texts
  (define (iter text-1 text-2 keys)
    (if (empty? keys)
      text-2
      (if (hash-has-key? text-1 (first keys))
        (hash-set text-2 (first keys) (abs(- (hash-ref text-1 (first keys)) (hash-ref text-2 (first keys)))))
        (iter text-1 text-2 (rest keys)))))
  (let ([keys (hash-keys text-2)])
    (iter text-1 text-2 keys)))

(define (decision from-lovecraft from-doyle)
;;; compare the average of the frequencies from both authors
  (let ([lovecraft (/ (foldl + 0 (hash-values from-lovecraft)) (hash-count from-lovecraft))]
        [doyle (/ (foldl + 0 (hash-values from-doyle)) (hash-count from-doyle))])
    (if (< lovecraft doyle)
      (displayln (string-append "Probably a lovecraft " (number->string lovecraft) " " (number->string doyle)))
      (displayln (string-append "Probably a Doyle " (number->string lovecraft) " " (number->string doyle))))))
      
;;; MAIN PROGRAM
;;; get the hash tables for all the txt files
(define pre-normal-lovecraft(make-reference-hash (process-file "Lovecraft.txt")))
(define pre-normal-doyle(make-reference-hash (process-file "Doyle.txt")))
(define pre-normal-mystery1(make-reference-hash (process-file "mystery1.txt")))
(define pre-normal-mystery2(make-reference-hash (process-file "mystery2.txt")))
;;; normalizes the hash tables from above
(define normal-doyle (normalize (first pre-normal-doyle) (second pre-normal-doyle)))
(define normal-lovecraft (normalize (first pre-normal-lovecraft) (second pre-normal-lovecraft)))
(define normal-mystery1 (normalize (first pre-normal-mystery1) (second pre-normal-mystery1)))
(define normal-mystery2 (normalize (first pre-normal-mystery2) (second pre-normal-mystery2)))
;;; makes the prediction of the mystery files
(decision (compare normal-lovecraft normal-mystery1) (compare normal-doyle normal-mystery1))
(decision (compare normal-lovecraft normal-mystery2) (compare normal-doyle normal-mystery2))