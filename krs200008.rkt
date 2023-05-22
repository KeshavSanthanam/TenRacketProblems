#lang racket
(provide (all-defined-out))

; 1. divisible-by-x?
(define divisible-by-x? (
  lambda (x) (lambda (y)(integer? (/ y x)))
  ; if the quotient is an integer, we return true
))

; 2. function-4
(define function-4 (
  lambda (four) (first (map four '(4))) ; using first index of array of one index (4)
   ))

; 3. my-map
(define my-map (lambda (function my-list)
  (cond[(empty? my-list) empty] ; return empty array if input is empty
       [else (cons
              (function (first my-list)) (my-map function (rest my-list))
              ; recursively call the same array
              ; without the beginning (using rest)
              )
             ])
       )
  )

; 4. pair-up
(define pair-up (lambda (sublist1 sublist2)
  (cond ((null? sublist1) '())
        ((null? sublist2) '())
        ; if a given index does not contain an element for both,
        ; then we disregard this pair
        (else
         (cons (list (first sublist1) (first sublist2))
               ; construct a pair with the beginnings of each sublist
               (pair-up (rest sublist1) (rest sublist2)))))))

; 5. classify
(define classify(lambda (function my-list)
; in-group handles the data for when the condition is met                  
(define in-group(lambda (function my-list)
                                  (if (empty? my-list)
                                      '()
                                                (if (function  (first my-list))
                                                    (append  (list (first my-list)) (in-group function (rest my-list)))
                                                    (append (in-group function (rest my-list)) '())
                                                    )      
                                      )
                                  ))
; out-group handles the data for when the condition isn't met                                    
(define out-group(lambda (function my-list)
                                  (if (empty? my-list)
                                      '()
                                                (if (function  (first my-list))
                                                     (append (out-group function (rest my-list)) '())
                                                    (append  (list (first my-list))
                                                             (out-group function (rest my-list)))
                                                   
                                                    )      
                                      )
                                  ))
                  (if (empty? my-list) (cons '() (list'())) ; print 2 groups since the problem asks for it
                      (cons (in-group function my-list) (cons (out-group function my-list) '()) )
                      )
                  ))
 
; 6. is-member?
(define is-member? (lambda (item my-list)
  (if (null? my-list) #f ; null lists have no members
      (if (equal? item (first my-list)) #t
          (is-member? item (rest my-list))))))

; 7. is-sorted?
(define is-sorted? (lambda (function my-list)
  (if (<= (length my-list) 1) #t ( ; length of 0 or 1 --> always sorted!
    if (function (first my-list) (first (rest my-list))) (
    is-sorted? function (rest my-list)) #f
    ))
  )
)
 
; 8. my-flatten
(define my-flatten (lambda (my-list)
                    (if (empty? my-list)'()
                      (if (list? (first my-list))
                          (append
                           (my-flatten (first my-list))
                           (my-flatten (rest my-list)))
                          (cons (first my-list) (my-flatten ; make new list to hold the values regardless of "layer"
                                              (rest my-list)
                                              ))
                          )
                      )
                    )
  )

; 9. my-list-ref
(define my-list-ref (lambda (my-list integer)
  (cond ((null? my-list) (display "ERROR: Index out of bounds."))
        ((= integer 0) (first my-list)) ; first corresponds to 0th index
        (else (my-list-ref (rest my-list) (- integer 1) ; input index - 1 (prefix/polish)
        )
        )
   )
))

; 10. deep-reverse
(define deep-reverse(lambda (my-list)
                      (if (empty? my-list) '()
                          (if (list? (first my-list))
                              (append
                               (deep-reverse(rest my-list))
                               ( cons(deep-reverse (first my-list))  '() ))
                              ; makes new lists as necessary
                              (append
                               (deep-reverse(rest my-list))
                               (list (first my-list)))
                              )
                          )
                      ))




