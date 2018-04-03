#lang racket
;; Dependencies
(require srfi/1)
(require srfi/13)
(require srfi/48)


(define responses
  '(
    ;;Room 1
    (1 "You are in a room, you see a bag, a coffin and a door")
    (2 "You found a key. What do you do now?")
    (3 "The coffin is empty and has trace of a giant fingerprint.")
    ;;Corridor
    (4 "You are in a corridor with two doors, one at your left and one at your right.")
    ;; Room 2A
    (5 "It's a trap! you fell off in dark hole.")
    ;; Room 2B'
    (6 "It's your lucky day! There is a weapon on the floor")
    (7 "You are now equiped. Watch this space!")
    ))

;; Defining actions
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define actions `(,@look ,@quit))

(define decisiontable
  '((1 ((open search bag) 2) ((open search coffin) 3) ((open search door) 4) ((look around)1))
  
    (2 ((open search bag) 2) ((open search coffin) 3) ((open search door) 4) ((look around)1))
    (3 ((open search bag) 2) ((open search coffin) 3) ((open search door) 4) ((look around)1))
    (4 ((go left) 5) ((go right) 6))
    (6 ((pick) 7))))


#| assq is a derivative of assoc and looks for the first element of a pair in a list which
is equal to a given atom according to 'eq?'. If such an argument exists, its pair is returned|#

;; Returns ONLY the second element of the pair as a list
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

;; Returns ONLY the second element of the pair as a string
(define (get-response id)
  (car(assq-ref responses id)))

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