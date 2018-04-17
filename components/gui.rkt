

(define width 50)
(define height 33)
(define done 16)
(define message "Welcome To the Dungeon | Please enter a command: ")
(define screen_width (* width done))
(define screen_height (* height done))

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

(define key (read-bitmap "./images/key.png"))
(define room1 (read-bitmap "./images/beam.jpg"))
(define room2 (read-bitmap "./images/wakeup.jpg"))
(define image3 (read-bitmap"./images/guard.jpg"))

(define startscreen (read-bitmap "./images/startscreen.jpg")) ;done
(define corridor (read-bitmap "./images/corridor.jpg")) ;done
(define hallway (read-bitmap "./images/hallway.jpg")) ;done
(define hall (read-bitmap "./images/hall.jpg")) ;done
(define entrance (read-bitmap "./images/entrance.jpg")) ;done
(define ancient-fac (read-bitmap "./images/ancient-factory.jpg")) ;dom
(define hs (read-bitmap "./images/hs.jpg")) ;done
(define prison (read-bitmap "./images/prison.jpg")) ;done
(define lobby (read-bitmap "./images/lobby.jpg"))

