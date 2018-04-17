
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

