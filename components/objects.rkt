#|====== OBJECTS ======|#

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
  (cond ((hash-has-key? db id)
         ;; This assigns to record the content of the key id inside the db hash table
         (let* ((record (hash-ref db id))
                ;; This will format the output
                (output (string-join record " and ")))
           (cond ((not(equal? output ""))
                       (if (eq? id 'bag)
                           (printf "You are carrying ~a. \n" output)
                           (printf "You can see ~a. \n" output))))))
        (else
         (if (eq? id 'bag)
             (printf "Your bag is empty! \n")
             (printf "The room is empty! \n")))))

;; This function avoids repetition in dropping items
(define (evaluate a b id)
  (cond ((eq? a b)
         'bag)
        (else
         id)))


;; Removing objects from the room
(define (remove-object db id from input)
  (let*((str (string-join (cdr (string-split input)))) 
        (newid (evaluate from 'bag id)))
    ;; When key(id) has something stored in db, proceed
    (when (hash-has-key? db newid)
      ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room)
      (let* ((record (hash-ref db newid))
             ;; Remove the occurrence of the item(based on the sufix, which is the most probable user input e.g. dagger) from the room
                 (result (remove (lambda (x) (string-suffix-ci? str x)) record))
                 ;; Return the items that record have and result don't
                 (item (lset-difference equal? record result)))
        (cond ((null? item)
               ;; If item is null(item is not in the room), reports error
               (printf "I don't see that item in the ~a! \n" from))
              (else
               (cond((eq? from 'room)
                     (printf "Added ~a to your bag.\n" (first item))
                     ;; Adds item to inventorydb
                     (add-object inventorydb 'bag (first item))
                     (hash-set! db id result))
                    (else
                     (printf "Removed ~a from your bag . \n" (first item))
                     (add-object objectdb id (first item))
                     (hash-set! db 'bag result)))))))))



(define (handle-item from id input)
  (if(eq? from 'bag)
    (remove-object inventorydb id 'bag input)
    (remove-object objectdb id 'room input)))


;; Display bag
(define (display-inventory)
  (display-objects inventorydb 'bag))


;;------------------------------------------------------------------------------------------------------------------

