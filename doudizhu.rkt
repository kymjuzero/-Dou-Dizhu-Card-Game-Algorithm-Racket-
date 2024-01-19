;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |doudizhu (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;    Josephina Kim (21092172)
;;    CS 135 Fall 2023
;;    Assignment 05, Question 3
;; ***************************************************
;;

;; Helper Functions

;; (card=? card1 card2): produces true or false depending on if card1 and card2 are equal
;; Examples:
(check-expect (card=? 3 3) true)

;; card=?: Any -> Bool 
(define (card=? card1 card2)
  (cond
    [(symbol? card1) (cond
                       [(symbol? card2) 
                        (cond
                          [(symbol=? card1 card2) true]
                          [else false])]
                       [else false])]
    [(number? card1) (cond
                       [(number? card2)
                        (cond
                          [(= card1 card2) true]
                          [else false])]
                       [else false])]
    
    [else false]))

;; (symbols->numbers card) produces a number value for symbols and 2
;; Examples:
(check-expect (symbols->numbers 'Jack) 11)

;; symbols->numbers: Any -> Nat
(define (symbols->numbers card)
  (cond
    [(card=? card 'Jack) 11]
    [(card=? card 'Queen) 12]
    [(card=? card 'King) 13]
    [(card=? card 'Ace) 14]
    [(card=? card 2) 15]
    [(card=? card 'Black) 16]
    [(card=? card 'Red) 17]
    [else card]))

;; (find-kind n sorted) produces a sorted list of Card
;; containing the card values with at least n occurrences in the consumed list
;; Examples:
(check-expect
 (find-kind 3 (list 4 4 4 6 7 7 8 8 10 10 'Jack 'Jack 'Jack 'King 'King 'Ace 'Ace))
 (list 4 'Jack))

;; find-kind: Nat (listof Any) -> (listof Any)
(define (find-kind n sorted)
  (cond
    [(empty? sorted) empty]
    [(>= (count-duplicates (first sorted) sorted) n)
     (cons (first sorted) (find-kind n (remove-duplicates (first sorted) sorted)))]
    [else (find-kind n (remove-duplicates (first sorted) sorted))]))

;; (remove-duplicates first-loa loa) produces a list of Any were duplicates of each card is removed
;; Examples:
(check-expect (remove-duplicates 4 (list 4 5 6)) (list 5 6))

;; remove-duplicates: Any (listof Any) -> (listof Any)
(define (remove-duplicates first-loa loa)
  (cond
    [(empty? loa) empty]
    [(member? first-loa loa) (remove-duplicates first-loa (rest loa))]
    [else loa]))

;; (count-duplicates first-loa loa) produces the number of duplicates in the list of Any
;; Examples:
(check-expect (count-duplicates 4 (list 4 5 6)) 1)

;; count-duplicates: Any (listof Any) -> Num
(define (count-duplicates first-loa loa)
  (cond
    [(empty? loa) 0]
    [(member? first-loa loa) (+ 1 (count-duplicates first-loa (rest loa)))]
    [else 0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3a)
;; (solos hand): consumes a Hand and produces a (listof Hand) where
;; each produced hand consists of a single card from the consumed hand 
;; Examples:
(check-expect
 (solos
  (list 3 3 4 4 4 5 6 6 6 7 8 9
        'Jack 'Queen 'King 'King 'Ace 2 2 2))
 (list (list 3) (list 4) (list 5) (list 6) (list 7) (list 8)
       (list 9) (list 'Jack) (list 'Queen) (list 'King)
       (list 'Ace) (list 2)))
(check-expect (solos (list 3 4 5 6 7 7 7 8 8 10 'Jack 'Jack 'Jack 'Jack 'Black))
              (list (list 3) (list 4) (list 5) (list 6)
                    (list 7) (list 8) (list 10) (list 'Jack) (list 'Black)))
(check-expect (solos (list 10 'Jack 'Queen 'King 'King 'King 'Ace 'Ace 'Ace 'Ace 2 'Black 'Red))
              (list (list 10) (list 'Jack) (list 'Queen)
                    (list 'King) (list 'Ace) (list 2) (list 'Black) (list 'Red)))

;; solos: Hand -> (lstof Hand)
(define (solos hand)
  (helper-function (find-kind 1 hand)))

(define (helper-function lst)
  (cond
    [(empty? lst) empty]
    [else (cons (list (first lst)) (helper-function (rest lst)))]))

;; Tests
(check-expect (solos
               (list 4 4 5 9 9 'Black))
              (list (list 4) (list 5) (list 9) (list 'Black)))
(check-expect (solos
               (list 'Jack 'Jack 'Jack 'King 'Red))
              (list (list 'Jack) (list 'King) (list 'Red)))
(check-expect (solos
               (list 4 4 6 9 9 'Jack 'Ace 'Ace))
              (list (list 4) (list 6) (list 9) (list 'Jack) (list 'Ace)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3b)

;; (duo-list hand-lst) produces all possible duos in a list
;; Example:
(check-expect (duo-list (list 8 9)) (list (list 8 8) (list 9 9)))

(define (duo-list hand-lst)
  (cond
    [(empty? hand-lst) empty]
    [else (cons (list (first hand-lst) (first hand-lst)) (duo-list (rest hand-lst)))]))

;; (pairs hand) produces a list of all possible pairs in a hand
;; Examples:
(check-expect
 (pairs (list 3 4 4 5 5 6 7 7 7 8
              'Jack 'Queen 'Queen 'King 'King 2))
 (list (list 4 4) (list 5 5) (list 7 7) (list 'Queen 'Queen) (list 'King 'King)))
(check-expect (pairs (list 3 4 5 6 7 8 9 10)) empty)
(check-expect (pairs (list 3 3 3 3 4 4 4 4 5 5 5 5)) (list (list 3 3) (list 4 4) (list 5 5)))

;; pairs: Hand -> (listof Hand)
(define (pairs hand)
  (duo-list (find-kind 2 hand)))

;; Tests
(check-expect (pairs (list 4 5 6 7 8 9 10 10 10)) (list (list 10 10)))
(check-expect (pairs (list 5 5 5 5 6 6 6 7 7 7 7 8 8 8)) (list (list 5 5)
                                                               (list 6 6) (list 7 7) (list 8 8)))
(check-expect (pairs (list 3 4 5 6 7 8 9 10 'Jack 'Queen 'King 'Ace 'Black 'Red)) empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3c)

;; (trio-lst hand-lst) produces all the trios in a given list of hands
;; Example:
(check-expect (trio-lst (list 3 4)) (list (list 3 3 3) (list 4 4 4)))

(define (trio-lst hand-lst)
  (cond
    [(empty? hand-lst) empty]
    [else (cons (list (first hand-lst) (first hand-lst) (first hand-lst)) (trio-lst (rest hand-lst)))]))

;; (trios hand) produces list of all possible trios in a hand without duplicates
;; Examples: 
(check-expect (trios (list 3 3 3 4 5 6 7 7 7)) (list (list 3 3 3) (list 7 7 7)))
(check-expect (trios (list 3 4 5 6 7 8 9 10 'Jack 'Queen 'King 'Ace 'Black 'Red)) empty)
(check-expect (trios (list 3 3 3 3 4 4 4 4 5 2 2 2)) (list (list 3 3 3) (list 4 4 4) (list 2 2 2)))

;; trios: Hand -> (lstof Hand)
(define (trios hand)
  (trio-lst (find-kind 3 hand)))

;; Tests
(check-expect (trios (list 3 'Queen 'Queen 'Queen 2 2 2))
              (list (list 'Queen 'Queen 'Queen) (list 2 2 2)))
(check-expect (trios (list 3 3 3 5 5 7 7 8 8 9 9 9 10 10 'Jack 'Jack 'Black 'Black))
              (list (list 3 3 3) (list 9 9 9)))
(check-expect (trios (list 5 5 5 6 6 6 6 7 7 'King 'King 2 2))
              (list (list 5 5 5) (list 6 6 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3d)

;; (rocket? hand) produces true if hand is a rocket, false if otherwise
;; Example:
(check-expect (rocket? (list 'Black 'Red)) true)

(define (rocket? hand)
  (cond
    [(and (= (length hand) 2) (card=? 'Black (first hand)) (card=? 'Red (second hand)))
     true]
    [else false]))

;; (bomb? hand) produces true if hand is bomb
;; Example: 
(check-expect (bomb? (list 4 4 4 4)) true)

(define (bomb? hand)
  (and (= (length hand) 4) (= (length (find-kind 4 hand)) 1)))

;; (hand<? hand1 hand2) produces true if hand1 is less than hand2, false if otherwise
;; Example: 
(check-expect (hand<? (list 3) (list 4)) true)

(define (hand<? hand1 hand2)
  (cond
    [(empty? hand2) false]
    [(rocket? hand1) false]
    [(rocket? hand2) true]
    [(and (bomb? hand1) (bomb? hand2)
          (< (symbols->numbers (first hand1))
             (symbols->numbers (first hand2)))) true] 
    [(bomb? hand1) false]
    [(bomb? hand2) true]
    [(= (length hand1) (length hand2))
     (cond
       [(< (symbols->numbers (first hand1)) (symbols->numbers (first hand2))) true]
       [(card=? (first hand1) (first hand2)) (hand<? (rest hand1) (rest hand2))]
       [else false])]
    [(< (length hand1) (length hand2)) true]
    [else false]))


;; (hand=? hands1 hands2) produces true if hands1 and hands2 are equal. false otherwise
;; Example:
(check-expect (hand=? (list 3 4) (list 3 4)) true)

(define (hand=? hands1 hands2)
  (cond
    [(and (empty? hands1) (empty? hands2)) true]
    [(and (= (length hands1) (length hands2))
          (card=? (first hands1) (first hands2))) (hand=? (rest hands1) (rest hands2))]
    [else false]))

;; (insert-hands hand lstof-hands): inserts hand into strength order of hand
;; Example: 
(check-expect (insert-hands (list 4 4 4) (list (list 3 5) (list 5 6 7)))
              (list (list 3 5) (list 4 4 4) (list 5 6 7)))

(define (insert-hands hand lstof-hands)
  (cond
    [(empty? lstof-hands) (cons hand empty)]
    [(hand<? hand (first lstof-hands)) (cons hand lstof-hands)]
    [(hand=? hand (first lstof-hands)) lstof-hands]
    [else (cons (first lstof-hands) (insert-hands hand (rest lstof-hands)))]))


;; (sort-hands hands): consumes a (listof Hand) and produces a sorted (listof Hand
;;containing the same hands, excluding duplicates
;; Examples:
(check-expect
 (sort-hands (list (list 6 6 6) (list 5 5 5) (list 4 4)
                   (list 6 6) (list 3 4) (list 'Black 'Red)
                   (list 4 5 6) (list 'Jack 'Queen)))
 (list (list 3 4) (list 4 4) (list 6 6) (list 'Jack 'Queen) (list 4 5 6) (list 5 5 5)
       (list 6 6 6) (list 'Black 'Red)))
(check-expect (sort-hands (list
                           (list 4 4 4) (list 4 4) (list 'King) (list 'Queen 'Queen 'Queen)))
              (list
               (list 'King) (list 4 4) (list 4 4 4) (list 'Queen 'Queen 'Queen)))
(check-expect (sort-hands (list
                           (list 7 7 7) (list 'Black 'Red) (list 6 7 8) (list 3)))
              (list
               (list 3) (list 6 7 8) (list 7 7 7) (list 'Black 'Red)))


;; sort-hands: (listof Hand) -> (listof Hand)
(define (sort-hands hands)
  (cond
    [(empty? hands) empty]
    [else (insert-hands (first hands) (sort-hands (rest hands)))]))

;; Tests
(check-expect (sort-hands (list (list 'Jack 'Queen 'King) (list 3 3 3)
                                (list 5 5 5) (list 3 3 3)))
              (list (list 3 3 3) (list 5 5 5)
                    (list 'Jack 'Queen 'King)))
(check-expect (sort-hands (list (list 7 7) (list 'Black 'Red) (list 2 2)))
              (list (list 7 7) (list 2 2) (list 'Black 'Red)))
(check-expect (sort-hands (list (list 10 10) (list 'Jack 'King) (list 4 4 4 4)))
              (list (list 10 10) (list 'Jack 'King) (list 4 4 4 4)))
(check-expect (sort-hands (list
                           (list 3) (list 5 5) (list 'Red 'Red 'Red)))
              (list
               (list 3) (list 5 5) (list 'Red 'Red 'Red)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; e)

;; (banned hand) eliminates that are not in straights
;; Example:
(check-expect (banned (list 2 'Black 'Red)) empty)

(define (banned hand)
  (cond
    [(empty? hand) empty]
    [(or
      (card=? 'Black (first hand))
      (card=? 'Red (first hand))
      (card=? 2 (first hand))) (banned (rest hand))]
    [else (cons (first hand) (banned (rest hand)))]))

;; (sub-seq begin hand) generates all possible subsequences for one hand using a base case
;; Example:
(check-expect (sub-seq (list 3) (list 4 5 6 7)) (list (list 3 4)
                                                      (list 3 4 5) (list 3 4 5 6) (list 3 4 5 6 7)))

(define (sub-seq begin hand)
  (cond
    [(empty? hand) empty]
    [(> (- (symbols->numbers (first hand)) (symbols->numbers (first begin))) 0)
     (cons (append begin (list (first hand)))
           (sub-seq (append begin (list (first hand))) (rest hand)))]
    [else empty]))

;; (all-subseq hand) generates all the possible subsequences in a hand
;; Example:
(check-expect (all-subseq (list 3 4 5 6))
              (list (list 3 4) (list 3 4 5) (list 3 4 5 6) (list 4 5) (list 4 5 6) (list 5 6)))

(define (all-subseq hand)
  (cond 
    [(empty? hand) empty]
    [else (append (sub-seq (list (first hand)) (rest hand))
                  (all-subseq (rest hand)))]))

;; (if-straight hand) checks if a hand is a straight
;; Example:
(check-expect (if-straight (list 6 7 8 9 10)) true)

(define (if-straight hand)
  (cond
    [(empty? (rest hand)) true]
    [(= (- (symbols->numbers (second hand))
           (symbols->numbers (first hand))) 1) (if-straight (rest hand))]
    [else false]))

;; (if-straight-all n hands) checks if all hands are straights depending on how long it needs to be
;; Example:
(check-expect (if-straight-all 5 (list (list 3 4) (list 3 4 5) (list 3 4 5 6) (list 3 4 5 6 7)))
              (list (list 3 4 5 6 7)))

(define (if-straight-all n hands)
  (cond
    [(empty? hands) empty]
    [(and (>= (length (first hands)) n)
          (if-straight (first hands)))
     (cons (first hands) (if-straight-all n (rest hands)))]
    [else (if-straight-all n (rest hands))]))


;; (straights hand): consumes a Hand and produces a (listof Hand) containing
;; all the straights that can be construcuted from that hand
;; Examples:
(check-expect
 (straights (list 3 3 3 4 5 6 7 8 10
                  'Jack 'Queen 'Queen 'King 'Ace))
 (list (list 3 4 5 6 7) (list 4 5 6 7 8) (list 10 'Jack 'Queen 'King 'Ace)
       (list 3 4 5 6 7 8)))
(check-expect (straights (list 3 4 5 6 7 8 9 10))
              (list (list 3 4 5 6 7) (list 4 5 6 7 8) (list 5 6 7 8 9) (list 6 7 8 9 10)
                    (list 3 4 5 6 7 8) (list 4 5 6 7 8 9) (list 5 6 7 8 9 10)
                    (list 3 4 5 6 7 8 9) (list 4 5 6 7 8 9 10)
                    (list 3 4 5 6 7 8 9 10)))
(check-expect (straights (list 9 10 'Jack 'Queen 'King 'Ace))
              (list (list 9 10 'Jack 'Queen 'King) (list 10 'Jack 'Queen 'King 'Ace)
                    (list 9 10 'Jack 'Queen 'King 'Ace)))
(check-expect (straights (list 3 4 6 7 8 9 10))
              (list (list 6 7 8 9 10)))

;; straights: Hand -> (listof Hand)
(define (straights hand)
  (sort-hands (if-straight-all 5 (all-subseq (banned (find-kind 1 hand))))))

;; Tests
(check-expect (straights (list 3 3 3 3 4 5 6 7 8 9 'Jack 'Jack 'Queen 'King 'Ace 2 2))
              (list (list 3 4 5 6 7) (list 4 5 6 7 8) (list 5 6 7 8 9) (list 3 4 5 6 7 8)
                    (list 4 5 6 7 8 9) (list 3 4 5 6 7 8 9)))
(check-expect (straights (list 3 4 5 6 6 7 8 'King 'Ace))
              (list (list 3 4 5 6 7) (list 4 5 6 7 8) (list 3 4 5 6 7 8)))
(check-expect (straights (list 3 3 3 4 5 5 6 7 'Jack 'Ace))
              (list (list 3 4 5 6 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; f)

;; (double hand) doubles the cards in a hand
;; Example:
(check-expect (double (list 3 4)) (list 3 3 4 4))

(define (double hand)
  (cond
    [(empty? hand) empty]
    [else (cons (first hand) (cons (first hand) (double (rest hand))))]))

;; (double-all hand) doubles all the cards in all the hands
;; Example:
(check-expect (double-all (list (list 3 4) (list 5 6))) (list (list 3 3 4 4) (list 5 5 6 6)))

(define (double-all hand)
  (cond
    [(empty? hand) empty]
    [else (cons (double (first hand)) (double-all (rest hand)))]))

;; (straight-pairs hand): consumes a Hand and produces a (listof Hand) containing
;; all the straight pairs that can be constructed from that hand
;; Examples:
(check-expect (straight-pairs (list 3 3 3 5 5 5 6 7 7 7 8 8 9 9 'King 'King))
              (list (list 7 7 8 8 9 9)))
(check-expect (straight-pairs (list 3 3 4 4 5 5 6 6))
              (list (list 3 3 4 4 5 5) (list 4 4 5 5 6 6) (list 3 3 4 4 5 5 6 6)))
(check-expect (straight-pairs (list 4 5 5 6 6 7 7 8 8 9 9 10))
              (list
               (list 5 5 6 6 7 7)
               (list 6 6 7 7 8 8)
               (list 7 7 8 8 9 9)
               (list 5 5 6 6 7 7 8 8)
               (list 6 6 7 7 8 8 9 9)
               (list 5 5 6 6 7 7 8 8 9 9)))
(check-expect (straight-pairs (list 3 3 4 5 6 6 7 8 9 10 'Jack 'Jack))
              empty)

;; straight-pairs: Hand -> (listof Hand)
(define (straight-pairs hand)
  (double-all (sort-hands (if-straight-all 3 (all-subseq (banned (find-kind 2 hand)))))))

;;Tests 
(check-expect (straight-pairs (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 9 'Ace 'Ace))
              (list (list 5 5 6 6 7 7) (list 6 6 7 7 8 8)
                    (list 5 5 6 6 7 7 8 8)))
(check-expect (straight-pairs (list 3 3 4 4 5 5 6 6 7 7 8 9 9 10 10))
              (list
               (list 3 3 4 4 5 5)
               (list 4 4 5 5 6 6)
               (list 5 5 6 6 7 7)
               (list 3 3 4 4 5 5 6 6)
               (list 4 4 5 5 6 6 7 7)
               (list 3 3 4 4 5 5 6 6 7 7)))
(check-expect (straight-pairs (list 3 4 5 5 6 6 6 7 7 7 8 9 9 10 10 'Jack 'Jack))
              (list (list 5 5 6 6 7 7) (list 9 9 10 10 'Jack 'Jack)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; g)
;; (triple hand) triples the cards in a hand
;; Example:
(check-expect (triple (list 3 4)) (list 3 3 3 4 4 4))

(define (triple hand)
  (cond
    [(empty? hand) empty] 
    [else (cons (first hand) (cons (first hand) (cons (first hand) (triple (rest hand)))))]))

;; (triple-all hand) doubles all the cards in all the hands
;; Example:
(check-expect (triple-all (list (list 3 4) (list 5 6))) (list (list 3 3 3 4 4 4) (list 5 5 5 6 6 6)))

(define (triple-all hand) 
  (cond
    [(empty? hand) empty]
    [else (cons (triple (first hand)) (triple-all (rest hand)))]))

;; (airplanes hand): consumes a Hand and produces a (listof Hand) containing all the airplanes
;; that can be constructed from that hand
;; Examples:
(check-expect (airplanes (list 5 5 5 5 6 6 6 6 7 7 7 7))
              (list (list 5 5 5 6 6 6) (list 6 6 6 7 7 7) (list 5 5 5 6 6 6 7 7 7)))
(check-expect (airplanes (list 'Jack 'Jack 'Jack 'Queen 'Queen 'Queen))
              (list (list 'Jack 'Jack 'Jack 'Queen 'Queen 'Queen)))
(check-expect (airplanes (list 6 7 7 7 8 8 8))
              (list (list 7 7 7 8 8 8)))

;; airplanes: Hand -> (listof Hand)
(define (airplanes hand)
  (triple-all (sort-hands (if-straight-all 2 (all-subseq (banned (find-kind 3 hand)))))))

;;Tests
(check-expect (airplanes (list 6 6 6 6 7 7 7 7))
              (list (list 6 6 6 7 7 7)))
(check-expect (airplanes (list 10 10 10 'Jack 'Jack 'Jack 'Queen 'Queen 'Queen))
              (list (list 10 10 10 'Jack 'Jack 'Jack) (list 'Jack 'Jack 'Jack 'Queen 'Queen 'Queen)
                    (list 10 10 10 'Jack 'Jack 'Jack 'Queen 'Queen 'Queen)))
(check-expect (airplanes (list 3 3 3 4 4 4 5 5))
              (list (list 3 3 3 4 4 4)))

                                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      

