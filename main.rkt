#lang racket
;; Dependencies
(require srfi/1)
(require srfi/13)
(require srfi/48)

;; Association list: list of paired cons forming a table
;; This maps the car of the list to its cdrb
(define descriptions '((1 "You are in the lobby")
                       (2 "You are in the hallway")
                       (3 "You are in a swamp")))

;; Actions including quasiquote and unquote-splicing
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define actions `(,@look ,@quit))

;; Decisiontable including quasiquote and unquote-splicing
(define decisiontable `((1 ((north) 2) ((north west) 3) ,@actions)
                        (2 ((south) 1) ,@actions)
                        (3 ,@actions)))


#| assq is a derivative of assoc and looks for the first element of a pair in a list which
is equal to a given atom according to 'eq?'. If such an argument exists, its pair is returned|#

;; Returns ONLY the second element of the pair as a list
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

;; Returns ONLY the second element of the pair as a string
(define (get-response id)
  (car(assq-ref descriptions id)))

;; Generates a keyword list based on the given id
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))




;; This function will match a list of keywords against a list of tokens
(define (list-of-lengths keylist tokens) (map
                                          (lambda (x)
                                            (let ((set (lset-intersection eq? tokens x)))
                                              ;; apply some weighting to the result
                                              (* (/ (length set) (length x)) (length set)))) keylist ))

;; This function will return the position of the largest integer in the list
(define (index-of-largest-number list-of-numbers) (let ((n (car (sort list-of-numbers >))))
                                                    (if (zero? n) #f
                                                        (list-index (lambda (x) (eq? x n)) list-of-numbers))))



;; assv is similar to assq but works according to eqv?
(define (assv-ref assvlist id)
  (cdr (assv id assvlist)))

;; This wrapper function will take an id and a list of tokens
(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens)))) (if index
                                                                                 (cadr (list-ref record index)) #f)))



(define (recommend initial-id)
  (let loop ((id initial-id))
    (format #t "~a\n> " (get-response id))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((eq? #f response)
               (format #t(loop id))
               ((eq? 'gory (format #t "huh? I didn’t understand that! ")
                     response)
                "Searching for gory horror films ....\n")
               (exit))
              ((eq? 'non-gory response)
               (format #t "Searching for non-gory scarey films ....\n")
               (exit))
              ((eq? 'quit response)
               (format #t "So Long, and Thank you master...\n")
               (exit)) (else
                        (loop response)))))))

(recommend 1)