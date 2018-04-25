(define width 50)
(define height 31)
(define screen_width (* width 16))
(define screen_height (* height 16))

(struct pos (x y))

;; create a new top-level window
;; instantiate the frame% class
(define frame (new import:frame%
                   [label "LOGIC INVASION"]
                   [width screen_width]
                   [height screen_height]))

(define (draws-sprite sprite poss)
  (send dc draw-bitmap sprite (pos-x  poss) (pos-y poss)))



(define (canvas-key frame) (class import:canvas%
                             (define/override (on-char key-event)
                               (cond
                                 [else (printf "Others")]))
                             (super-new [parent frame])))

;; define canvas
(define canvas ( new (canvas-key frame)))
(define dc (send canvas get-dc))
;; show the frame by calling its show method
(send frame show #t)

;; import images
(define startscreen (read-bitmap "./images/startscreen.jpg"))
(define corridor (read-bitmap "./images/corridor.jpg")) 
(define hallway (read-bitmap "./images/hallway.jpg")) ;
(define hall (read-bitmap "./images/hall.jpg")) ;
(define entrance (read-bitmap "./images/entrance.jpg")) ;
(define ancient-fac (read-bitmap "./images/ancient-factory.jpg"))
(define hs (read-bitmap "./images/hs.jpg")) ;
(define prison (read-bitmap "./images/prison.jpg")) ;
(define lobby (read-bitmap "./images/lobby.jpg"))
(define rip (read-bitmap "./images/rip.jpg"))


