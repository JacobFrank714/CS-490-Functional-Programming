#lang racket

(require srfi/1)

; Read stop words
(define stop-words
  (with-input-from-file "stop_words.txt"
    (lambda () (string-split (read-line) " "))))

; Preprocessing functions
(define (remove-punctuation str)
  (regexp-replace* #rx"[[:punct:]]" str ""))

(define (process-file filename)
  (with-input-from-file filename
    (lambda ()
      (let* ([content (read-line)]
             [words (map string->symbol (string-split (remove-punctuation content) " "))])
        (for/fold ([word-hash (make-hash)]) ([word words])
          (when (not (member word stop-words))
            (hash-update word-hash word add1 0))
          (for/hash ([(word count) (in-hash word-hash)])
            (values word (/ (- (log 10 count)) (log 10 (length words)))))
          )))))

; Process all files and create a hash of hashes
(define (process-files file-list)
  (for/hash ([filename file-list])
    (values filename (process-file filename))))

; User interaction functions
(define (search corpus search-words)
  (for/list ([filename (hash-keys corpus)])
    (let ([word-scores (hash-ref corpus filename)])
      (define matches
        (for/sum ([word search-words] #:when (hash-has-key? word-scores word))
          1))
      (define score
        (for/sum ([word search-words] #:when (hash-has-key? word-scores word))
          (hash-ref word-scores word)))
      (values filename matches score))))

(define (display-results ranked-results)
  (for ([result ranked-results])
    (printf "~a~%" (with-input-from-file (first result) read-line))))

; Main program
(define corpus (process-files '("Files/001.txt" "Files/002.txt" "Files/003.txt" ... "Files/025.txt")))

(define (main)
  (printf "Enter search words (separated by space): ")
  (define search-words (map string->symbol (string-split (read-line) " ")))
  (define search-results (search corpus search-words))
  (define ranked-results
    (sort search-results (lambda (a b) (or (> (second a) (second b)) (< (third a) (third b))))))
  (display-results ranked-results))

(main)
