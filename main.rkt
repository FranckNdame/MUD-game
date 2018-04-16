#lang racket
;; Dependencies
(require (prefix-in import: racket/gui))
(require racket/draw)
(require srfi/1)
(require srfi/13)
(require srfi/48)
(require rsound)
(define stream (make-pstream))


(define menu-sound (rs-read "sounds/menu-sound3.wav"))
(define miss-racket (rs-read "sounds/help-ai.wav"))

(define width 50)
(define height 33)
(define done 16)
(define message "Welcome To the Dungeon | Please enter a command: ")
(define screen_width (* width done))
(define screen_height (* height done))


(define key (read-bitmap "images/key.png"))
(define startscreen (read-bitmap "./images/startscreen.jpg"))
(define room2 (read-bitmap "./images/guard.jpg"))
(define beam (read-bitmap "./images/beam.jpg"))
(define wake-up (read-bitmap "./images/wakeup.jpg"))
(define guard (read-bitmap "./images/guard.jpg"))

(define LEAD-TIME (* 1/10 44100))

(struct pos (x y))

(define frame (new import:frame%
                   [label "LOGIC INVASION"]
                   [width screen_width]
                   [height screen_height])
  )

(define (draws-sprite sprite poss)
  (send dc draw-bitmap sprite (pos-x  poss) (pos-y poss))
  )

(define msg (new import:message% [parent frame] [label ""]))
(define (canvas-key frame) (class import:canvas%
                             (define/override (on-char key-event)
                               (cond
                                 [else (send msg set-label "Others")]))
                             (super-new [parent frame])))


(define canvas ( new (canvas-key frame)))
(define dc (send canvas get-dc))
(send frame show #t)



#|===================================================== DATA =================================================|#
;; Association list
;; Describes the objects
(define objects '(
                  (3 "a key")
                  (4 "a piece of paper")
                  (6 "a teleporter")))



;; Association list: list of paired cons forming a table
;; This describes the room
(define descriptions '((1 "Test")
                       (2 "What..... \nWhat is happening?... \nWhere am I?.....\nI should probably 'look' around.")
                       ;; Prison Cell
                       (3 "OH!... \nI am in the prison cell! \nHow did I get here?....\nI need to get out! \nThis guard appears to be asleep.\n")
                       (4 ">>> You are in the hall <<<
                         \nBoy: James.... I have been waiting for so long. Don't ask any question and take what is in my pocket.\n")
                       (5 ">>> You are in an ancient church where a woman approaches you<<< \nWoman: Well well hello Mr James..\nYou probably wonder who I am right? ")
                       ;; If the user answers YES
                       (6 "Woman: Your curiosity does not surprise me.\nJane: I am Jane and I am the reason you are here...")
                       ;; ELSE
                       (7 "How rude! I knew I shouldn't have left you alive.\n>>The woman pushes you in a sea of sharks<<\n GAME OVER!")))


;; Actions 
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((put) drop) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define help '(((help) help) ((instructions) help)))

;; List of pairs constructed with quasiquote and unquote-splicing
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory ,@help))


;; Decisiontable constructed with quasiquote and unquote-splicing
(define decisiontable `((1 ((start) 2) ,@help ,@quit)
                        (2 ((a beam of light at the end of the room) 3) ,@actions)
                        (3 ((an entrance to a hall) 4) ((a tunnel) 5) ,@actions)
                        (4 ((a front door) 5) ((a back door) 3) ,@actions)
                        (5 ((yes) 6) ((no) 7) ((I don't care) 7))
                        (6 ((south west) 4) ,@actions)
                        (7 ((south west) 4) ,@quit)))

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


;; Displaying objects in the room and inventory
(define (display-objects db id)
  ;; When the id has something stored in db, proceed
  (when (hash-has-key? db id)
    ;; This assigns to record the content of the key id inside the db hash table
    (let* ((record (hash-ref db id))
           ;; This will format the output
           (output (string-join record " and ")))
      ;; Shows items in inventory or in the ground.
      (cond
        ((and (equal? output "") (eq? id 'bag)) (printf "Your inventory is empty.\n"))
        ((and (equal? output "") (number? id)) (printf "The room is empty.\n"))
        ((and (not (equal? output "")) (eq? id 'bag)) (printf "You are carrying ~a.\n" output))
        (else (printf ">>> You see ~a <<<\n" output))))))

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
             ;; Checks if the user interacted with is the teleporter
             (if (eq? (first item) "a teleporter")
                 (begin
                   (printf "This is all for the moment. Watch this space.\n")
                   )
                 ;; Removes item from the ground  
                 (hash-set! db id result))
             )))))

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
(define (drop-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))            


;; Picking objects
(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

;; Display bag
(define (display-inventory)
  (display-objects inventorydb 'bag))


;;------------------------------------------------------------------------------------------------------------------


;; ii)   Instructions
;;------------------------------------------------------------------------------------------------------------------

(define (display-help)
  (stop)
           (play miss-racket)
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

;; This helps in obtaining the user's location through a unique id
(define (get-location id)
  (printf "~a\n" (car (assq-ref descriptions id)))
  ;; This describe the objects present in the room
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
             (printf ">>>> You can see ~a <<<<\n" (slist->string (caar result))))
            ;If there is more than one result
            (else
             ;; losym in let* will remove the numbers from the directions. The second one transforms the list in a lat with the directions.
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               ;; This will take the atoms from lostr and transform them into a string separated by " and "
               (printf ">>>>> You can see ~a <<<<<\n" (string-join lostr " and "))))))))

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
    ;; Returns ONLYS the accepted keywords(not their actions)
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
    ;; Assigns to record a list with the possible actions for the current room
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens)))) (if index
                                                                                 (cadr (list-ref record index)) #f)))

;;------------------------------------------------------------------------------------------------------------------

#|=================================================================================================================|#




#|===================================================== GAME LOOP =================================================|#


(define (gamestart initial-id)
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
                (cond
          ((eq? response 1 )
           ((draws-sprite wake-up (pos 0 0)))))  
        (cond
          ((eq? response 2 )
           (draws-sprite wake-up (pos 0 0))))
        (cond
          ((eq? response 3 )
           (stop)
           (draws-sprite room2 (pos 0 0))          
           (draws-sprite key (pos 370 350))
           ))
        (cond
          ((eq? response 4 )
           (draws-sprite wake-up (pos 0 0))))
        (cond
          ((eq? response 5 )
           (draws-sprite wake-up (pos 0 0))))
        ;(printf "Input: ~a\nTokens: ~a\nResponse: ~a\n" input tokens response)
        (cond ((number? response)
               (loop response #t))
              ;; If response meaning couldn't be found after the lookup function, shows error message
              ((eq? #f response)
               (format #t "Huh? I didn't understand that!\n")
               (loop id #f))
              ;; Response action is look at around the room for directions
              ((eq? response 'look)
               ;; Retrieve possible directions
               (cond
          ((eq? id 2 )
           (draws-sprite beam (pos 0 0))))
               (get-directions id)
               
               (loop id #f))
              
              ;; Response action is to pick an item
              ((eq? response 'pick)
               (cond
               [(eq? id 3) (draws-sprite guard (pos 0 0))])
               ;; Pick up item
               (pick-item id input)
               (loop id #f))
              ;; Response action is to drop an item
              ((eq? response 'drop)
               ;; Drop item
               
               (set! key (read-bitmap "./key.png"))
               (cond

                 [(eq? id 3) (draws-sprite key (pos 370 350))])
                                           
               (drop-item id input)
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
               (import:message-box "Bye" "Bye bye" #f '(ok))
               (send frame show #f)
               (stop)
               (exit)))))))

;(set! sword (read-bitmap "./monster.png"))


#|=================================================================================================================|#



;; Adds the objects to the database before the game starts
(add-objects objectdb)
(draws-sprite startscreen (pos 0 0))
(play menu-sound)
(send (gamestart 1) start 100)


