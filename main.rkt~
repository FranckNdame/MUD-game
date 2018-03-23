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



;; Getting the user's input
(define (command)
  (let* ((input (read-line))
         (string-tokens (string-tokenize input))
         (tokens (map string->symbol string-tokens))
         (cmd (car tokens)))   
    ;; Conditional expressions based on the input
    [cond
      ;; Command explanation
      ((eq? cmd 'help)
       (format #t "
\nInstructions:\n=============================\n
Start: Starts the game.\n
Search <term>: Search an item\n
Quit: Quit the game\n=============================\n \n"))
     

      ((eq? cmd 'start)
       (format #t "You are in a room, you can see a bag and a coffin\n"))
      ;; Break the loop
      ((eq? cmd 'quit)
       (exit))
      ;; Search for something
      ((eq? cmd 'search)
       (format #t "Searching for ~a...\n" (cadr tokens)))
      ;; Error handling
      (else
       (format #t "Sorry, I can't quite understand what you mean.\n"))])

  (command))
(command)