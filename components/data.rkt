
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