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