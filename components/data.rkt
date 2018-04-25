#|====== DATA ======|#
;; Association list
;; Describes the objects
(define objects '((0 "a steel bar")
                  (1 "a bottle of water")
                  (2 "a piece of bread")
                  (3 "a torch")
                  (4 "a key")
                  (5 "a pass")))



(define room-type '((0 "Entrance")
                    (1 "hall")
                    (2 "hallway")
                    (3 "corridor")
                    (4 "lobby" )
                    (5 "prison cell")
                    (6 "ancient factory" )
                    (7 "patient room" )))


(define key_objects '((0 "a broken key")
                      (1 "a pass")
                      (2 "a white key")
                      (3 "a black key")))




;; Actions 
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((put) drop) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define help '(((help) help) ((instructions) help)))
(define directions '(((south) direction) ((north) direction) ((west) direction) ((east) direction)))
(define mazemap '(((map) mazemap) ((show map) mazemap)((see map) mazemap) ((look map) mazemap)))
(define mute '(((mute) mute)))
(define health '(((energy) health) ((health) health)))
(define start '(((start) start) ((go) start)))

;; List of pairs constructed with quasiquote and unquote-splicing
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory ,@help ,@mazemap ,@directions ,@mute ,@health ,@start ))


;; Decisiontable constructed with quasiquote and unquote-splicing
(define decisiontable `((1 ,@actions)
                        (2 ((south) 1) ,@actions )
                        (3 ,@actions)))