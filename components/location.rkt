
;; iii)   Location
;;------------------------------------------------------------------------------------------------------------------

#|
;; This helps in obtaining the user's location through a unique id
(define (get-location id)
  (printf "~a\n" (car (assq-ref descriptions id)))
  ;; This describe the objects present in the room
  (display-objects objectdb id)
  (printf "> "))
|#

;; Returns ONLY the second element of the pair as a list
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))



;; Obtaining room directions
(define (get-directions id)
  ;; Describe objects that are present in the room
  (display-objects objectdb id)
  ;; HOF filter returns direction entries
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      ;; If there is no result
      (cond ((= 0 n)
             ;If there is more than one result
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             ;; Extract the directions from result using our slist->string function
             (printf ">>>> You can see ~a <<<<\n" (slist->string (caar result))))
            ;If there is more than one result
            (else
             ;; losym in let* will remove the numbers from the directions. The second one transforms the list in a lat with the directions.
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               ;; This will take the atoms from lostr and transform them into a string separated by " and "
               (printf ">>>>> You can see ~a <<<<<\n" (string-join lostr " and "))))))))


;;Converts lists to mutable string
(define (slist->string l)
  (string-join (map symbol->string l)))

#| assq is a derivative of assoc and looks for the first element of a pair in a list which
is equal to a given atom according to 'eq?'. If such an argument exists, its pair is returned|#


;; Returns ONLY the second element of the pair as a string
;(define (get-response id)
; (car(assq-ref descriptions id)))

;; Generates a keyword list based on the given id
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    ;; Returns ONLYS the accepted keywords(not their actions)
    (map (lambda (key) (car key)) keys)))




;; This function will match a list of keywords against a list of tokens
;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       (* (/ (length set) (length x)) (length set))))
   keylist))

;return the index of the highest number on the list provided by the function
;(list-of-lenghts)
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (lambda (x) (eq? x n)) list-of-numbers))))




;; assv is similar to assq but works according to eqv?
(define (assv-ref assvlist id)
  (cdr (assv id assvlist)))

(define (ass-ref assqlist id x)
  (cdr (x id assqlist)))
;; This wrapper function will take an id and a list of tokens
(define (lookup id tokens)
    ;; Assigns to record a list with the possible actions for the current room
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens)))) (if index
                                                                                 (cadr (list-ref record index)) #f)))





(define (call-actions id tokens func)
  (let* ((record (ass-ref decisiontable 1 assv)) ;;get the references
         (keylist (get-keywords 1)) ;;get the keywords
         ;;description in the functions
         (index (index-of-largest-number (list-of-lengths keylist tokens)))) 
    (if index 
        (func (list-ref record index)) ;;return result if match, return false if dont
        #f)))


;; Check if the palyer has the required key to unlock the door
(define (door-handle gatekey)
  (printf "You can see the exit gate, but it is locked. \n")
  (cond ((hash-has-key? inventorydb 'bag)
         (let* ((record (hash-ref inventorydb 'bag)) ;;get items list in bag
                (result (remove (lambda (x) (string-suffix-ci? gatekey x)) record)) ;;result = record - bag
                (item (lset-difference equal? record result))) ;; compare them
           (cond ((null? item) ;;if there is no difference, the key was removed, return true
               #t))))
        (else
         #f)))



;;------------------------------------------------------------------------------------------------------------------

#|=================================================================================================================|#
