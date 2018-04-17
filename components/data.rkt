#|====== DATA ======|#
;; Association list
;; Describes the objects
(define objects '((0 "a steel bar")
                  (1 "a bulb")
                  (2 "a piece of bread")
                  (3 "a rope")
                  (4 "a broken key")
                  (5 "a pass")
                  (6 "a bottle of water")))

(define key_objects '((0 "a key")
                      (1 "a pass")
                      (2 "a white key")
                      (3 "a black key")))

(define room-type '((0 "Entrance")
                    (1 "hall")
                    (2 "hallway")
                    (3 "corridor")
                    (4 "lobby" )
                    (5 "prison cell")
                    (6 "court" )
                    (7 "pass" )))



;; Actions 
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((put) drop) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define help '(((help) help) ((instructions) help)))
(define directions '(((south) direction) ((north) direction) ((west) direction) ((east) direction)))
(define mazemap '(((map) mazemap) ((show map) mazemap)((see map) mazemap) ((look map) mazemap)))

;; List of pairs constructed with quasiquote and unquote-splicing
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory ,@help ,@mazemap))


;; Decisiontable constructed with quasiquote and unquote-splicing
(define decisiontable `((1 ,@actions)
                        (2 ((south) 1) ,@actions )
                        (3 ,@actions)))