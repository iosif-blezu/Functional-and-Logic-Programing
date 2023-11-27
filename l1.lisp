; A
; is-linear(l1,l2,...,ln) ---> true if n = 0 (lits is empty)
;                         ---> false if l1 is a list
;                         ---> is-linear(l2,l3,...,ln) otherwise

(defun is-linear (lst)
  (cond ((null lst) t)                        ; an empty list is considered linear
        ((atom (car lst))                     ; if the first element is not a list, check the rest of the list
         (is-linear (cdr lst)))
        (t nil)))                             ; if the first element is a list, it's not linear


; B
; replace-first-occurrence((l1,l2,...,ln), e, o) ---> [] if n = 0
;                                                ---> (o,l2,l3,...,ln) if l1 = e
;                                                ---> replace-first-occurrence ((l2,l3,...,ln), e, o) otherwise
; this doesn't replace in nested lists

(defun replace-first-occurrence (lst e o)
  (cond ((null lst) nil)                                          ; if the list is empty, return nil
        ((equal (car lst) e) (cons o (cdr lst)))                  ; if the first element is E, replace it with O
        (t (cons (car lst)                                        ; otherwise keep the first element
                 (replace-first-occurrence (cdr lst) e o)))))     ; and recurse on the rest of the list

; replace-first-occurrence-nested((l1,l2,...,ln), e, o) ---> [] if n = 0
;                                                       ---> (o,l2,l3,...,ln) if l1 = e
;                                                       ---> ((r1,r2,...,rm), replace-first-occurrence-nested((l2,l3,...,ln), e, o)) if l1 is a list and 
;                                                                                                    replace-first-occurrence-nested(l1, e, o) = (r1,r2,...,rm) 
;                                                       ---> (l1, replace-first-occurrence-nested((l2,l3,...,ln), e, o)) otherwise

(defun replace-first-occurrence-nested (lst e o)
  (cond ((null lst) nil)                                                         ; if the list is empty, return nil
        ((equal (car lst) e) (cons o (cdr lst)))                                 ; if the first element is E, replace it with O
        ((listp (car lst))                                                       ; if the first element is a list itself
                                                                                 ; then recursively process this sublist and check if replacement was made
         (let ((subresult (replace-first-occurrence-nested (car lst) e o)))
           (if (equal subresult (car lst))                                       ; if the sublist didn't change, replacement wasn't made
               (cons subresult (replace-first-occurrence-nested (cdr lst) e o))  ; check the rest of the list
               (cons subresult (cdr lst)))))                                     ; if the sublist changed, replacement was made, keep the rest as is
        (t (cons (car lst)                                                       ; if the first element is not E and not a sublist
                 (replace-first-occurrence-nested (cdr lst) e o)))))             ; recurse on the rest of the list


; C
; replace-sublist-with-last((l1,l2,...,ln)) ---> [] if n = 0
;                                           ---> (l1, replace-sublist-with-last((l2,l3,...,ln))) if l1 is not a list
;                                           ---> (replace-sublist-with-last-element(l1), replace-sublist-with-last((l2,l3,...,ln))) otherwise 

(defun replace-sublist-with-last (lst)
  (cond ((null lst) nil)                                      ; if the list is empty, return nil
        ((atom (car lst))                                     ; if the first element is not a list, keep it
         (cons (car lst)
               (replace-sublist-with-last (cdr lst))))        ; and recurse on the rest of the list
        (t                                                    ; if the first element is a list
         (cons (replace-sublist-with-last-element (car lst))  ; replace it with the last element
               (replace-sublist-with-last (cdr lst))))))      ; and recurse on the rest of the list

; replace-sublist-with-last-element(l1,l2,...,ln) ---> [] if n = 0
;                                                 ---> l1 if n = 1 and l1 is an atom
;                                                 ---> replace-sublist-with-last-element(l2,l3,...,ln) if l1 is an atom
;                                                 ---> replace-sublist-with-last-element(replace-sublist-with-last(l1)) if l1 is a list

(defun replace-sublist-with-last-element (sublst)
  (cond ((null sublst) nil)                                     ; if the sublist is empty, return nil
        ((atom sublst) sublst)                                  ; if the sublist is an atom, return it
        ((atom (car sublst))                                    ; if the head of the sublist is an atom
         (if (null (cdr sublst))                                ; and it's the only element
             (car sublst)                                       ; return it
             (replace-sublist-with-last-element (cdr sublst)))) ; otherwise, recurse
        (t                                                      ; if the head is a sublist
         (replace-sublist-with-last-element                     ; recurse to flatten it before finding the last element
          (replace-sublist-with-last sublst)))))                ; this flattens one level of sublists


; D
; merge-without-duplicates((l1,l2,...,ln), (b1,b2,...,bm)) ---> (b1,b2,...,bn) if n = 0
;                                                          ---> (l1,l2,...,ln) if m = 0
;                                                          ---> (l1 U merge-without-duplicates((l2,l3,...,ln), (b2,b3,...,bn))) if l1 = b1
;                                                          ---> (l1 U merge-without-duplicates((l2,l3,...,ln), (b1,b2,...,bm))) if l1 < b1
;                                                          ---> (b1 U merge-without-duplicates((l1,l2,...,ln), (b2,b3,...,bm))) otherwise

(defun merge-without-duplicates (list1 list2)
  (cond
    ((null list1) list2)                                       ; if the first list is empty, return the second list
    ((null list2) list1)                                       ; if the second list is empty, return the first list
    ((equal (car list1) (car list2))                           ; if the heads of both lists are equal
     (cons (car list1)                                         ; take one of the heads
           (merge-without-duplicates (cdr list1)               ; and merge the rest, skipping one duplicate
                                      (cdr list2))))
    ((< (car list1) (car list2))                               ; if the head of the first list is smaller
     (cons (car list1)                                         ; take the head of the first list
           (merge-without-duplicates (cdr list1) list2)))      ; and merge the rest of the first list with the second
    (t                                                         ; if the head of the second list is smaller
     (cons (car list2)                                         ; take the head of the second list
           (merge-without-duplicates list1 (cdr list2))))))    ; and merge the first list with the rest of the second

(defvar lst1 nil)
(defvar lst nil)
(defvar list1 nil)
(defvar list2 nil)
(defvar merged-list nil)

(setq lst1 '(1 2 3 4 5 6))
(setq lst '(1 2 48  (1 2 3) (a b (c x)) b (d e) f (g h)))
(setq list1 '(1 3 5 7))
(setq list2 '(1 2 3 4 5 6 8))
(setq merged-list nil)
(setq merged-list (merge-without-duplicates list1 list2))

(print (is-linear lst1))
(print (is-linear lst))
(print (replace-first-occurrence lst 'b 'o))
(print (replace-first-occurrence-nested lst 'b 'o))
(print (replace-sublist-with-last lst))
(print merged-list)
