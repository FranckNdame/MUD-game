#lang racket
;; Dependencies
(require (prefix-in import: racket/gui))
(require racket/draw)
(require srfi/1)
(require srfi/13)
(require srfi/48)
(require rsound)
(define stream (make-pstream))

;; Include external files

(include "components/objects.rkt")
(include "components/gui.rkt")
(include "components/data.rkt")
(include "components/location.rkt")
(include "components/maze.rkt")


#|====== IMPORT SOUND ======|#
(define menu-sound (rs-read "sounds/menu-sound3.wav"))
(define miss-racket (rs-read "sounds/help-ai.wav"))

#|====== DATABASE ======|#
;; Creating the object database
(define objectdb (make-hash))
;; Creating the inventory database
(define inventorydb (make-hash))
;; Creating a database to store room names
(define rooms (make-hash))

#|====== MAZE ======|#
;; Build the maze
(define m (build-maze X Y))
;; Define gatekey
(define gatekey "")

#|====== PLAYER LOCATION ======|#
(define (startpoint)
  (let*((start_x (random X))
        (start_y (random Y)))
  (list start_x start_y)))

;; Randomly allocates something to a position in the maze
(define (random-allocator db types rate)
  (for ((j X))
    (for ((i Y))
      (cond ((<= (random 100) rate)
             (cond((equal? db rooms) ; add the name to the room
                   (hash-set! db (list j i) (car( ass-ref types (random (- (length types) 1)) assq))))
                  (else ;add to objectdb
                   (add-object db (list j i) (car (ass-ref types (random (- (length types) 1)) assq))))))))))


;; This function will place one unit of each type of key randomly on the maze
(define (random-key-location db types)
  (for ((i (length types)))
    (add-object db (list (random X) (random Y)) (car (ass-ref types i assq)))))


#|====== RANDOM ALLOCATIONS ======|#
;; Allocate names to the rooms
(random-allocator rooms room-type 100)
;; Allocate objects to the rooms
(random-allocator objectdb objects 50)
;; Allocate keys to the rooms
(random-key-location objectdb key_objects)   



(define (display-help)
  (stop)
  (play miss-racket)
  (printf "\nINSTRUCTIONS
=================
Welcome to Logic Invation MUD.\n

* GAME OBJECTIVE
  ===============
  The game objective is to escape the maze. 
  To be able to open the gate, you must find a key
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

(define (startgame-maze)
  ;(even? 5)
  (let* ((gatekey (car (ass-ref key_objects (random(length key_objects)) assq)))
         (gate_x 4)
         (gate_y 4)
         (start '(0 0)))
   ;;the following prints will help with testing, telling the developer where the gate is located and what key is the right one
    ;(printf "~a \n" gate_x)
    ;(printf "~a \n" gate_y)
    ;(printf "~a \n" gatekey)
    ;(printf "~a \n " start)
    ;(printf "\n")
    
    
    (let loop ((rid start))
      (show-maze m rid)

           (cond

             [(equal? rid '(0 0)) (printf "hello\n")]
             [(equal? rid '(0 1)) (printf "world\n")]
             [(equal? rid '(0 2)) (printf "bye\n")])
             

             

      (cond

        [(equal? (hash-ref rooms rid) "Entrance") (draws-sprite image3 (pos 0 0))]
        [(equal? (hash-ref rooms rid) "hall") (draws-sprite room1 (pos 0 0))]
        [(equal? (hash-ref rooms rid) "hallway") (draws-sprite room2 (pos 0 0))]
        [(equal? (hash-ref rooms rid) "corridor") (draws-sprite room2 (pos 0 0))]
        [(equal? (hash-ref rooms rid) "lobby") (draws-sprite room2 (pos 0 0))]
        [(equal? (hash-ref rooms rid) "court") (draws-sprite room2 (pos 0 0))]
        [(equal? (hash-ref rooms rid) "pass") (draws-sprite room2 (pos 0 0))])
      
      
      (printf "You are in the ~a \n>" (hash-ref rooms rid))

      (let* ((input (read-line))
             (string-tokens (string-tokenize input))
             (tokens (map string->symbol string-tokens))
             (response (call-actions rid tokens cadr))) ;;get action
        


      
        (cond ((eq? response 'direction)
               (let* ((direction (call-actions rid tokens caar)) ;get direction typed
                      (newlocation (move-room rid direction)))  ;get future location after move
                 (cond((member direction (paths rid)) ;check if direction is in path
                       (cond ((equal? newlocation (list gate_x gate_y)) ;end of game condition
                              (cond ((not (door-handle gatekey))
                                     (printf "It seems that you don't have the key to open the gate. \n")
                                     (loop newlocation))
                                    (else
                                     (printf "You used the key to open the gate. You are free! \n")
                                     (exit))))
                         (else
                          (loop newlocation))));;not in the gate
   
                      (else ;;direction not in path
                       (printf "You can not go that way!\n")
                       (loop rid)))))

              
              ((eq? #f response)
               (format #t "I am sorry, but I didn't understand that!\n")
               (loop rid))

              
              ((eq? response 'start)
               (printf "heloooooo")
               (loop rid))
            
              ((eq? response 'look)
              (show-maze m rid)
               (display-objects objectdb rid)
               (loop rid))
              ((eq? response 'mazemap)
               (show-maze m rid)
              (display-objects objectdb rid)
               (loop rid))
            
              ((eq? response 'pick)
             ;remove item from room and put into inventory
               (handle-item 'room rid input)
               (loop rid))


            
              ((eq? response 'inventory)
               (display-inventory) ;;show inventorydb
               (loop rid))
            
              ((eq? response 'quit)
               (format #t "So Long Franck...\n")
               (stop)
               (exit))
            
              ((eq? response 'drop)
               ;remove item from inventory and drop on the current room
               (handle-item 'bag rid input)
               (loop rid)))))))





#|====== START GAME ======|#
(play menu-sound)
(startgame-maze)

