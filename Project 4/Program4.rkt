#lang racket

(require srfi/1)

; Read stop words
(define stop-words
  (with-input-from-file "Project 4/stop_words.txt"
    (lambda () (string-split (read-line) " "))))

; Preprocessing functions
(define (remove-punctuation str)
  (regexp-replace* #rx"[[:punct:]]" str ""))

(define (process-file filename)
  (with-input-from-file filename
    (lambda ()
      (let* ([content (read-line)]
             [words (map string->symbol (string-split (remove-punctuation content) " "))])
        (define word-hash
          (for/fold ([word-hash (make-hash)]) ([word words])
            (when (not (member word stop-words))
              (hash-update! word-hash word add1 0))
            word-hash))
        (for/hash ([(word count) (in-hash word-hash)])
          (values word (if (and (> (length words) 0) (> count 0))
                           (/ (- (log count)) (log (length words)))
                           0)))
        ))))

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
      (list filename matches score))))

(define (display-results ranked-results)
  (for ([result ranked-results])
    (printf "~a~%" (with-input-from-file (first result) read-line))))

; Main program
(define corpus (process-files '(
  "Project 4/Files/001.txt" 
  "Project 4/Files/002.txt" 
  "Project 4/Files/003.txt" 
  "Project 4/Files/004.txt" 
  "Project 4/Files/005.txt" 
  "Project 4/Files/006.txt" 
  "Project 4/Files/007.txt" 
  "Project 4/Files/008.txt" 
  "Project 4/Files/009.txt" 
  "Project 4/Files/010.txt" 
  "Project 4/Files/011.txt" 
  "Project 4/Files/012.txt" 
  "Project 4/Files/013.txt" 
  "Project 4/Files/014.txt" 
  "Project 4/Files/015.txt" 
  "Project 4/Files/016.txt" 
  "Project 4/Files/017.txt" 
  "Project 4/Files/018.txt" 
  "Project 4/Files/019.txt" 
  "Project 4/Files/010.txt"
  "Project 4/Files/021.txt" 
  "Project 4/Files/022.txt" 
  "Project 4/Files/023.txt" 
  "Project 4/Files/024.txt" 
  "Project 4/Files/025.txt")))

(define (string-contains? str substr)
  (regexp-match? (regexp-quote substr) str))

(define (main)
  (let loop ()
    (printf "Enter search words (separated by space) or type 'exit' to quit: ")
    (define input (read-line))
    (cond
      [(string-contains? input "exit")
       (printf "Goodbye!~%")]
      [else
       (define search-words (map string->symbol (string-split input " ")))
       (define search-results (search corpus search-words))
       (define ranked-results
         (sort search-results (lambda (a b) (or (> (second a) (second b)) (< (third a) (third b))))))
       (display-results ranked-results)
       (loop)])))

(main)
