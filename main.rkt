#lang racket
;; Dependencies
(require srfi/1)
(require srfi/13)
(require srfi/48)

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