#lang racket
;; Dependencies
(require srfi/1)
(require srfi/13)
(require srfi/48)


#|===================================================== DATA =================================================|#
;; Association list
(define objects '((1 "a key")
                  (2 "a piece of paper")
                  (3 "a potion flask")
                  (5 "a teleporter")))


;; Association list: list of paired cons forming a table
;; This maps the car of the list to its cdr
(define descriptions '((1 "You are in the prison cell. The guard appears to be asleep.")
                       (2 "You are in the hall \nBoy: James.... I have been waiting for so long. Don't ask any question and take what is in my pocket.")
                       (3 "You are in an ancient church.")
                       (4 "You are in a graveyard.")
                       (5 "You are in a mystic room.")))


;; Actions including quasiquote and unquote-splicing
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((put) drop) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define help '(((help) help) ((instructions) help)))
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory ,@help))


;; Decisiontable including quasiquote and unquote-splicing
(define decisiontable `((1 ((a little boy standing at the end of the hall) 2) ,@actions)
                        (2 ((an entrance to a hall) 1) ((a half open door) 3) ((a tunnel) 4) ,@actions)
                        (3 ((a front door) 4) ((a back door) 2) ,@actions)
                        (4 ((a door with a *DO NOT ENTER!* sign) 5) (( an entrance to the east) 3) ,@actions)
                        (5 ((south west) 3) ,@actions)))


#|============================================================================================================|#

#|===================================================== FUNCTIONS =================================================|#

;; i)   Objects
;;------------------------------------------------------------------------------------------------------------------

;; Creating the object database
(define objectdb (make-hash))
;; Creating the inventory database
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


;; Displaying objects
(define (display-objects db id)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or bag)
    (let* ((record (hash-ref db id))
           ;; Formats the output(list of items in the room)
           (output (string-join record " and ")))
      ;; Shows items in inventory or in the ground. Adds treatment to cases where the room or the inventory are empty
      (cond
        ((and (equal? output "") (eq? id 'bag)) (printf "Your inventory is empty.\n"))
        ((and (equal? output "") (number? id)) (printf "The room is empty.\n"))
        ((and (not (equal? output "")) (eq? id 'bag)) (printf "You are carrying ~a.\n" output))
        (else (printf "You see ~a.\n" output))))))

;; Removing objects from the room
(define (remove-object-from-room db id str)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room)
    (let* ((record (hash-ref db id))
           ;; Remove the occurrence of the item(based on the sufix, which is the most probable user input e.g. dagger) from the room
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           ;; Return the items that record have and result don't
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             ;; If item is null(item is not in the room), reports error
             (printf "I don't see that item in the room!\n"))
            (else
             (printf "Added ~a to your bag.\n" (first item))
             ;; Adds item to inventorydb
             (add-object inventorydb 'bag (first item))
             ;; Checks if the item interacted with is the interdimensional communicator. If it is, the game is over
             (if (eq? (first item) "a teleporter")
                 (begin
                   ;; Shows message and exits game
                   (printf "This is all for the moment. Watch this space.\n")
                   )
                 ;; Removes item from the ground  
                 (hash-set! db id result))
             (exit))))))

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

;; Dropping objects
(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))            


;; Calling the functions to the main loop
(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

(define (display-inventory)
  (display-objects inventorydb 'bag))


;;------------------------------------------------------------------------------------------------------------------


;; ii)   Instructions
;;------------------------------------------------------------------------------------------------------------------

(define (display-help)
  (printf "\nINSTRUCTIONS
=================
Welcome to Logic Invation MUD.\n

* GAME OBJECTIVE
  ===============
  The game objective is to activate the nether portal to escape the maze. 
  To be able to open the portal, you must find the Interdimensional Communicator
  in one of the maze rooms.\n\n

* VALID COMMANDS
 ================
          - Enter look, directions or examine room): Get information about the current room.
          - Enter pick, get or pickup [item-name] : Pick the item.
          - Enter drop, put, place or remove [item-name] : Drop the item in your bag on the ground.
          - Enter inventory or bag : Display a list of available items in the bag.
          - Enter help or instructions : Display instructions.
          - Enter quit, exit, quit game or exit game) : Quit the application.\n
          "))

;;------------------------------------------------------------------------------------------------------------------


;; iii)   Location
;;------------------------------------------------------------------------------------------------------------------

(define (get-location id)
  (printf "~a\n" (car (assq-ref descriptions id)))
  ;; Describe objects that are present in the room
  (display-objects objectdb id)
  (printf "> "))



;; Obtaining room directions
(define (get-directions id)
  ;; Describe objects that are present in the room
  (display-objects objectdb id)
  ;; HOF filter returns direction entries
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      ;; If there is no result
      (cond ((= 0 n)
             ;If there is more than one result
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             ;; Extract the directions from result using our slist->string function
             (printf "You can see ~a.\n" (slist->string (caar result))))
            ;If there is more than one result
            (else
             ;; losym in let* will remove the numbers from the directions. The second one transforms the list in a lat with the directions.
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               ;; This will take the atoms from lostr and transform them into a string separated by " and "
               (printf "You can see ~a.\n" (string-join lostr " and "))))))))

;;------------------------------------------------------------------------------------------------------------------

;; iv)   Input
;;------------------------------------------------------------------------------------------------------------------

;;Converts lists to mutable string
(define (slist->string l)
  (string-join (map symbol->string l)))

#| assq is a derivative of assoc and looks for the first element of a pair in a list which
is equal to a given atom according to 'eq?'. If such an argument exists, its pair is returned|#

;; Returns ONLY the second element of the pair as a list
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

;; Returns ONLY the second element of the pair as a string
;(define (get-response id)
; (car(assq-ref descriptions id)))

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

;;------------------------------------------------------------------------------------------------------------------

#|=================================================================================================================|#




#|===================================================== GAME LOOP =================================================|#

(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        ;; If there is an available description, shows it on the screen
        (get-location id)
        ;; Else statement. Don't show location(because there isn't any description). Just shows the greater than symbol to incite user to type in text field
        (printf "> "))
    ;; Read input from the keyboard
    (let* ((input (read-line))
           ;; Function contained in the srfi/13 library, tokenize the input into substrings where a space character is found
           (string-tokens (string-tokenize input))
           ;; Creates a list of symbols(not strings) with the input. This is needed to compare the entry with our predefined lists
           (tokens (map string->symbol string-tokens)))
      ;; Decides which action response corresponds to. One of the most important calls in the code 
      (let ((response (lookup id tokens)))
        ;; (printf "Input: ~a\nTokens: ~a\nResponse: ~a\n" input tokens response)
        (cond ((number? response)
               (loop response #t))
              ;; If response meaning couldn't be found after the lookup function, shows error message
              ((eq? #f response)
               (format #t "Huh? I didn't understand that!\n")
               (loop id #f))
              ;; Response action is look at around the room for directions
              ((eq? response 'look)
               ;; Retrieve possible directions
               (get-directions id)
               (loop id #f))
              ;; Response action is to pick an item
              ((eq? response 'pick)
               ;; Pick up item
               (pick-item id input)
               (loop id #f))
              ;; Response action is to drop an item
              ((eq? response 'put)
               ;; Drop item
               (put-item id input)
               (loop id #f))
              ;; Response action is to show inventory
              ((eq? response 'inventory)
               ;; Displays the inventory
               (display-inventory)
               (loop id #f))
              ;; Response action is to display the help file
              ((eq? response 'help)
               ;; Displays Help text on the screen
               (display-help)
               (loop id #f))
              ;; Exit game command
              ((eq? response 'quit)
               ;; Exit the application
               (format #t "So long Franck....")
               (exit)))))))
               
#|=================================================================================================================|#

;; Adds the objects to the database before the game starts
(add-objects objectdb)

(startgame 1)