#lang racket
(require data/maybe)
(require data/either)
(require typed-stack)

(define (CLR)
    (empty-stack))

(define (SHOW stack)
    (in-stack stack))

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
(define stack (empty-stack))
(let loop ()
    (display "Input: ")
    (define a (read-line (current-input-port) 'any))
    (let ([in (string-split a " ")])
        
        (print (push stack in)))
    
    (loop))