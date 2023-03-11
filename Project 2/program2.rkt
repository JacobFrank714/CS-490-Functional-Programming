#lang racket
(require 2htdp/batch-io)

(define (process-files f1 f2)
  (let
      ([file-string1 (string-downcase(read-file f1))]
       [file-string2 (string-downcase(read-file f2))])
    (string-trim file-string1 #:repeat? #t)))
    
(process-files "Doyle.txt" "Lovecraft.txt")