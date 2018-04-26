#lang racket
;; Dependencies
(require (prefix-in import: racket/gui))
(require racket/draw)
;(require racket)
(require (prefix-in import2: srfi/1))
;(require (except-in srfi/1 remove))
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
(include "components/main-function.rkt")


#|====== IMPORT SOUND ======|#
(define menu-sound (rs-read "sounds/ending.wav"))
(define miss-racket (rs-read "sounds/help-ai.wav"))
(define get-item (rs-read "sounds/get-item.wav"))
(define no-item (rs-read "sounds/no-item.wav"))
(define selectionb (rs-read "sounds/start.wav"))
(define low-health (rs-read "sounds/low-health.wav"))
(define rip-sound (rs-read "sounds/rip.wav"))

#|====== HASH-TABLES ======|#
;; Creating the object hash-table
(define objectdb (make-hash))
;; Creating the inventory hash-table
(define inventorydb (make-hash))
;; Creating the rooms hahs-table
(define rooms (make-hash))

#|====== MAZE ======|#
;; Build the maze
(define m (build-maze X Y))
;; Define gatekey
(define gatekey "")


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
(random-allocator objectdb objects 100)

;(random-key-location objectdb key_objects)   



(define (display-help loop rid)
  ;; stops any sound currently playing
  (stop)
  ;; plays the voice service
  (play miss-racket)
  (printf "\nINSTRUCTIONS
=================
Welcome to Logic Invation MUD.\n

* GAME OBJECTIVE
  ===============
  - The game objective is to escape the maze. 
    To be able to open the gate, you must find a key
    in one of rooms.\n

  - Don't forget to restore your health by picking up edible items.\n\n

* VALID COMMANDS
 ================
          - Enter look, directions or examine room: Get information about the current room.
          - Enter pick, get or pickup [item-name] : Pick the item.
          - Enter drop, put, place or remove [item-name] : Drop the item in your bag on the ground.
          - Enter inventory or bag : Display a list of available items in the bag.
          - Enter health or life : Check your health level.
          - Enter help or instructions : Display instructions.
          - Enter solve maze or solution : Solves the maze
          - Enter mute : mutes the audio
          - Enter quit, exit, quit game or exit game) : Quit the application.\n")
  (loop rid))



;;===== MAIN LOOP =====
(define (startgame-maze)
  (let* ((gatekey (car (ass-ref key_objects (random(length key_objects)) assq)))
         ;; define the ending point
         (gate_x 7)
         (gate_y 7)
         ;; define the starting point
         (start '(0 0)))
    
    ;; starting the loop
    (let loop ((rid start))
      ;; display images of the room
      (room_images rid)
      ;(show-path m '(0 0) '(4 4))
      ;; displays the player's location on launch
      (disp-loc-onlaunch rid)
      ;; assign the variable 'input' to the read-line function
      (let* ((input (read-line))
             (string-tokens (string-tokenize input))
             (tokens (map string->symbol string-tokens))
             (response (call-actions rid tokens cadr))) ;;get action
        (printf "\n")
        ;; if the user's health goes below 1%
        (game-over)

        ;; handle user input
        (handle-user-input response rid tokens gate_x gate_y loop input)))))

;; run the game
(play-game)


