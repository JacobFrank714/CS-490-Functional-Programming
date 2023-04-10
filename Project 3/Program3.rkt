#lang racket
(require data/maybe)
(require data/either)
(require typed-stack)

(define (CLR)
    (empty-stack))

(define (SHOW stack)
    (displayln (in-stack stack)))

(define (TOP stack)
    (displayln (top stack)))

(define (SIZ stack)
    (stack-length stack))

(define (DUP stack)
    (push stack (top stack)))

(define (ADD stack)
    (let
    ([x (pop! stack)]
    [y (pop! stack)])
    (push stack (+ x y))))

(define (SUB stack)
    (let
    ([x (pop! stack)]
    [y (pop! stack)])
    (push stack (- x y))))

(define (MUL stack)
    (let
    ([x (pop! stack)]
    [y (pop! stack)])
    (push stack (* x y))))

(define (DIV stack)
    (let
    ([x (pop! stack)]
    [y (pop! stack)])
    (push stack (/ x y))))

;;; (define (END stack)
;;;     )

(let loop ()
    (display "Input: ")
    (define a (read-line (current-input-port) 'any))
    (define (iter stack to-do)
        (if (empty? to-do)
            stack
            (iter (push stack (first to-do)) (rest to-do))))
    (let ([in (reverse (string-split a " "))])
        (define stack (iter (empty-stack) in))
        (displayln (in-stack stack)))
    
    
    (loop))