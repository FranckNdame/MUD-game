#lang racket
;; Dependencies
(require srfi/1)
(require srfi/13)
(require srfi/48)

;; Association list
(define objects '((1 "a knife") (1 "a piece of paper")))

;; Creating the object and inventory database
(define objectdb (make-hash))
(define inventorydb (make-hash))


(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

;; Define a function to add objects into a database
(define (add-objects db)
  (for-each
   (lambda (r)
     (add-object db (first r) (second r))) objects))

;; Load objects data into our defined object database
(add-objects objectdb)

;; Displaying objects
(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag)
            (printf "You are currently carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))

;; Removing objects from the room
(define (remove-object-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "I don't see that item in the room!\n"))
            (else
             (printf "Added ~a to your bag.\n" (first item))
             (add-object inventorydb 'bag (first item))
             (hash-set! db id result))))))

;; Removing objects from the inventory
(define (remove-object-from-inventory db id str)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "You are not carrying that item!\n"))
            (else
             (printf "Removed ~a from your bag.\n" (first item))
             (add-object objectdb id (first item))
             (hash-set! db 'bag result))))))



;; Association list: list of paired cons forming a table
;; This maps the car of the list to its cdr
(define descriptions '((1 "You are in the lobby")
                       (2 "You are in the hallway")
                       (3 "You are in a swamp")))

;; Actions including quasiquote and unquote-splicing
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((put) drop) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory))

;; Decisiontable including quasiquote and unquote-splicing
(define decisiontable `((1 ((north) 2) ((north west) 3) ,@actions)
                        (2 ((south) 1) ,@actions)
                        (3 ,@actions)))

;;Converts lists to mutable string
(define (slist->string l)
  (string-join (map symbol->string l)))


;; Obtaining room directions
(define (get-directions id)
  (let ((record (assq id decisiontable)))
    ;; HOF filter returns direction entries
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           ;; Set n to the length of the result
           (n (length result)))
      ;; If there is no result
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ;; If there is one result
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            ;If there is more than one result
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))


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
;; outputs a list in the form: (0 0 0 2 0 0)
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



;; Game loop
(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        (printf "~a\n> " (get-response id))
        (printf "> "))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (format #t "Sorry, I'm not sure I understand\n")
               (loop id #f))

              ((eq? response 'look)
               (get-directions id)
               (loop id #f))
              ((eq? response 'quit)
               (format #t "So Long Franck...\n")
               (exit)))))))

(startgame 1)