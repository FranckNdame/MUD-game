#lang racket

(define responses
  '(
    (a "Expression 1")
    (b "Expression 2")
    (c "Expression 3")))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))
(define (get-response id)
  (car (assq-ref responses id)))

;;Testing the function
;===============================
;; Returns the whole pair as a list
(assq 'b responses)

;; Returns ONLY the second element of the pair as a list
(assq-ref  responses 'b)

;; Returns ONLY the second element of the pair as a string
(get-response 'b)

