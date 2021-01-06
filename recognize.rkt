;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Starter Code
;; ***************************************************
;;
;;
;;----------------------------
;;Frank Yu
;;CS135
;;Assignment 04 Problem 3
;;-----------------------------
;;

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num) that represents an (x, y) coordinate

;; A Gesture is a (listof Point)

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       



;; 3a)
;; These are helper functions. See assignment for design recipe requirements.

;;(get-x lst) takes a list of 2 numbers and outputs the x coordinate value
;;Examples:
(check-expect (get-x (list 5 6)) 5)
(check-expect (get-x (list 3 34)) 3)

;;get-x: Point -> Num
(define (get-x lst)
  (first lst))

;;(get-y lst) takes  list length=2 and outputs the y coordinate value
;;requires lst= Listof Num
;;Examples:
(check-expect (get-y (list 2 4)) 4)
(check-expect (get-y (list 100 425)) 425)

;;get-y: point -> Num
(define (get-y lst)
  (second lst))

;;(translate-gesture point-lst offset-lst) takes in 2 lists
;;point-lst is a gesture
;;offset-lst is a amount each point in the gesture will be translated
;;Examples:
(check-expect (translate-gesture (list (list 1 2)
                                       (list 2 3)
                                       (list 5 10))
                                 2 3)
              (list (list 3 5)
                    (list 4 6)
                    (list 7 13)))
;;translate-gesture: Gesture-> Gesture
(define (translate-gesture point-lst x y)
  (cond
    [(empty? point-lst) empty]
    [else (cons (cons (+ (get-x (first point-lst))
                   x)
                 (cons (+ (get-y (first point-lst))
                                y) empty))
                      (translate-gesture (rest point-lst) x y))]
    )
  )
;;(scale-gesture point-lst x y) takes in 2 lists
;;point-lst is a gesture
;;scale-lst is the amount each point in the gesture will be scaled by
;;Examples:
(check-expect (scale-gesture (list (list 1 2)
                                   (list 2 3 )
                                   (list 4 5)
                                   )
                             2 2)
              (list (list 2 4)
                    (list 4 6)
                    (list 8 10))) 
;;scale-gesture: Gesture Num Num-> Gesture
(define (scale-gesture point-lst x y)
  (cond
    [(empty? point-lst) empty]
    [else (cons (cons (* (get-x (first point-lst))
                   x)
                (cons (* (get-y (first point-lst))
                                y) empty))
                      (scale-gesture (rest point-lst) x y))]
    )
  )
;;(get-b-box gesture) takes in a gesture and outputs its max x coordinate and max y coordinate
;;Examples:
(check-expect (get-b-box (list (list 100 0)
                               (list 200 140)
                               (list 0 2)
                               (list 500 40)))
              (list (list 0 0) (list 500 140)))
;;get-b-box: Gesture-> Gesture
(define (get-b-box gesture)
  (list (get-min gesture) (get-max gesture)))

;;(get-min lst) takes in a list of Num and outputs the smallest x and y coordinates in the list
;;Examples:
(check-expect (get-min (list (list 2 3)
                             (list 4 5)
                             (list 1 5)))
              (list 1 3))
;;get-min: Gesture-> point
(define (get-min lst)
  (cond
    [(empty? lst) empty]
    [(= (length lst) 1) (first lst)]
    [(and (< (get-x (second lst)) (get-x (first lst)))
          (<(get-y (second lst)) (get-y (first lst))))
     (get-min (rest lst))]
    [(< (get-x (second lst)) (get-x (first lst)))
     (get-min (cons (cons (get-x (second lst)) (cons (get-y (first lst)) empty))
                    (rest (rest lst))))]
     [(< (get-y (second lst)) (get-y (first lst)))
     (get-min (cons (cons (get-x (first lst)) (cons (get-y (second lst)) empty))
                    (rest (rest lst))))]
     [else (get-min (cons (first lst) (rest (rest lst))))]
     )
  )
    

;;(get-max lst) takes in a list of Num and outputs the largest x and y coordinates in the list
;;Examples:
(check-expect (get-max (list (list 2 5)
                             (list 100 400)
                             (list 10000 4)
                             (list 23 551)
                             (list 1 10000000000)))
              (list 10000 10000000000))
;;get-max: Gesture-> point
  (define (get-max lst)
  (cond
    [(empty? lst) empty]
    [(= (length lst) 1) (first lst)]
    [(and (> (get-x (second lst)) (get-x (first lst)))
          (>(get-y (second lst)) (get-y (first lst))))
     (get-max (rest lst))]
    [(> (get-x (second lst)) (get-x (first lst)))
     (get-max (cons (cons (get-x (second lst)) (cons (get-y (first lst)) empty))
                    (rest (rest lst))))]
     [(> (get-y (second lst)) (get-y (first lst)))
     (get-max (cons (cons (get-x (first lst)) (cons (get-y (second lst)) empty))
                    (rest (rest lst))))]
     [else (get-max (cons (first lst) (rest (rest lst))))]
     )
  )
;; 3b)
;; Full design recipe required.

;;i)

;;(gesture-length lst): takes a list of Numbers. Outputs the total length of the points
;;Examples:
(check-within (gesture-length (list (list 2 3)
                                    (list 3 4)
                                    (list 1 4)))
              3.41 0.01)
(check-within (gesture-length (list) ) 0 0.01)

;;gesture-length: Gesture-> Num
(define (gesture-length lst)
  (length-adder lst 0))

;;Tests:
(check-expect (gesture-length (list (list 1 2))) 0)

;;(length-adder lst total): takes in a list of Numbers and total=0
;;outputs the total length of each point in the list.
;;outputs 0 if number of points is < 2
;;Tests:
(check-within (length-adder (list (list 2 3)
                                  (list 4 5)
                                  (list 3 4)) 0)
              4.24 0.01)
(define (length-adder lst total)
  (cond
    [(< (length lst) 2) total]
    [else (length-adder (rest lst)
                        (+ (sqrt
                            (+
                            (sqr (- (get-x (second lst)) (get-x (first lst))))
                            (sqr (- (get-y (second lst)) (get-y (first lst))))
                            )
                            ) total))]
    )
  )
;;Tests:
(check-expect (length-adder (list ) 0) 0
             )
(check-expect (length-adder (list (list 100000 24425252)) 0) 0)

;;ii)
;;(get-points gesture getlst) takes in 2 list of numbers
;;gesture is the list of points
;getlst provides the points that need to be outputted
;;requires each value in getlst to be less than or equal to (sub1 (length gesture))
;;Examples:
(check-expect (get-points (list (list 2 2)
                                (list 1 3)
                                (list 3 4))
                          (list 0 0 1 2))
              (list (list 2 2)
                    (list 2 2)
                    (list 1 3)
                    (list 3 4)))
;;get-points: Gesture Listof Num-> Gesture
(define (get-points gesture getlst)
  (cond
    [(empty? getlst) empty]
    
    [else (cons (point-getter gesture (first getlst))
                (get-points gesture (rest getlst)))]
    )
  )
;Tests:
(check-expect (get-points (list )
                          (list))
              empty)


;;(point-getter lst index) takes a list of points (nums) and Num index
;;gets the value located at index of the list
;;Examples:
(check-expect (point-getter (list (list 1 1 )
                                  (list 2 2)
                                  (list 3 3)) 2)
              (list 3 3))
;;point-getter: Gesture Num-> point                                   
(define (point-getter lst index)
  (cond
    [(zero? index) (first lst)]
    [else (point-getter (rest lst) (sub1 index))]
    )
  )


;; 3c) Starter code definitions

;; 3ci)
;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))



;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty
(define (five-sample gesture)
  (cons
   (first gesture)
   (cons
    (point-getter gesture (floor (* 0.25 (length gesture))))
    (cons
     (point-getter gesture (floor (* 0.5 (length gesture))))
     (cons
      (point-getter gesture (floor (* 0.75 (length gesture))))
      (cons
       (point-getter gesture (sub1 (length gesture))) empty))))))


;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))


    
   
    
;; 3cii)

;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0

(define (move-and-scale gesture x-scale y-scale)
  (scale-gesture (translate-gesture gesture
                                    (* (get-x (get-min gesture)) -1) (* (get-y (get-min gesture)) -1))
                 x-scale
                 y-scale))


;; Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))



;; 3ciii)

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size
;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)

;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty

(define (normalize-gesture gesture)
  (cond
    [(< (- (get-x (get-max gesture))
        (get-x (get-min gesture))) min-width)
     (move-and-scale gesture 1 (/ norm-size (get-y (get-max (move-and-scale gesture 1 1)))))]
    [(< (- (get-y (get-max gesture))
        (get-y (get-min gesture))) min-height)
     (move-and-scale gesture (/ norm-size (get-x (get-max (move-and-scale gesture 1 1)))) 1)]
    [else (move-and-scale gesture
                          (/ norm-size (get-x (get-max (move-and-scale gesture 1 1))))
                          (/ norm-size (get-y (get-max (move-and-scale gesture 1 1)))))]
    )
  )
    
     
     


;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)



;; 3civ)

;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               16.16 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-5match gesture1 gesture2)
  (cond
  [(= (length gesture1) 1) 0]
  [else (/ (special-list-adder
   (normalize-gesture (five-sample gesture1))
   (normalize-gesture (five-sample gesture2)) 0) 5)]
)
  )

  
  



;; Tests:
(check-within (geometric-5match
               (list (list 5 5))
               (list (list 5 5))) 0 0.01)
(check-within (geometric-5match (list (list 5 5) (list 2 2))
                                (list (list 3 3) (list 5 5)))
              200.01 0.01)

  ;;(special-list-adder gesture1 gesture2 total) produces the total distance of 2 gestures
  ;;Requires length of gesture1=gesture2, total=0 on input

;;Examples:
  (check-within (special-list-adder (list (list 1 1) (list 2 2) (list 3 3))
                                    (list (list 2 2) (list 3 3) (list 4 4)) 0) 4.24 0.01)
  
;;special-list-adder: gesture gesture-> Num
(define (special-list-adder gesture1 gesture2 total)
  (cond
    [(empty? gesture1) total]
    
    [else (special-list-adder (rest gesture1) (rest gesture2)
                        (+ (sqrt
                            (+
                            (sqr (- (get-x (first gesture2)) (get-x (first gesture1))))
                            (sqr (- (get-y (first gesture2)) (get-y (first gesture1))))
                            )
                            ) total))]
    )
  )
                
                
                


;; 3cv)

;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)


;; five-point-rec Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal
(define (five-point-rec candidate template-library)
  (first (first (get-points template-library (5match-searcher candidate template-library 0 0 0)))))

;;(5match-searcher candidate template min index total)takes in 5 variables
;; candidate is the candidate of the image
;;template= template-library
;;min is the smallest 5match distance recorded
;;index is the index location of the letter that possesses the smallest 5match
;;total takes in the number of times the loop is undergone
;;outputs the index-1 to get the letter

(define (5match-searcher candidate template min index total)
  (cond
    [(empty? template) (list index)]
    [(zero? total) (5match-searcher candidate (rest template)
                                    (geometric-5match candidate (second (first template)))
                                    total (add1 total))]
    [(< (geometric-5match candidate (second (first template))) min)
     (5match-searcher candidate (rest template)
                      (geometric-5match candidate (second (first template)))
                      total (add1 total))]
    [else (5match-searcher candidate (rest template) min index (add1 total))]
    )
  )
  

;; Tests
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)

;;d)
;;(sub-sample gesture k): takes a gesture and outputs points located at (floor) index 0, 1/(k-1) ...(k-2)/(k-1) and k-1
;;Examples:
(check-expect (sub-sample (list (list 2 2) (list 3 3) (list 4 4 ) (list 5 5) (list 6 6 ) (list 7 7)) 4)
              (list (list 2 2) (list 4 4) (list 6 6) (list 7 7)))
(check-expect (sub-sample (list (list 2 2) (list 3 3)) 5)
              (list (list 2 2) (list 2 2) (list 3 3) (list 3 3) (list 3 3)))

;;sub-sample: gesture Num-> gesture
(define (sub-sample gesture k)
  (sub-point-finder gesture k 0))

;;sub-point-finder takes a gesture, num k value and total=0
;;outputs the list at a certain dex relative to value of k
;;Examples are the same as (sub-sample)

;;sub-point-finder: gesture Num Num-> gesture
(define (sub-point-finder gesture k total)
  (cond
    [(= total (sub1 k)) (cons (point-getter gesture (sub1 (length gesture))) empty)]
    [(zero? total) (cons (first gesture) (sub-point-finder gesture k (add1 total)))]
    [else (cons (point-getter gesture (floor (* (/ total (sub1 k)) (length gesture))))
                (sub-point-finder gesture k (add1 total)))]
    )
  )

;;(geometric-match gesture1 gesture2 k): takes 2 gestures an num K.
;;outputs the average distange between the two gestures
;;Examples:
(check-within (geometric-match (list (list 50 300) (list 250 44)
                                     (list 14 50) (list 80 99)
                                     (list 30 54) (list 255 90))
                               (list (list 245 188) (list 142 88))
                               4)
              167.29 0.01)
;;geometric-match: gesture gesture Num-> Num

   (define (geometric-match gesture1 gesture2 k)
  (cond
  [(= (length gesture1) 1) 0]
  [else (/ (special-list-adder
   (normalize-gesture (sub-sample gesture1 k))
   (normalize-gesture (sub-sample gesture2 k)) 0) k)]
)
  )
;;(k-point-rec candidate template-library k): takes a gesture: candidate
;;outputs the symbol for what candidatre could be from template-library
;;k is the number of points that will be taken from canidate
;;Examples:
(check-expect (k-point-rec
               (list (list 142 152.1999969482422) (list 145 152.1999969482422)
                     (list 154 153.1999969482422) (list 165 154.1999969482422)
                     (list 178 155.1999969482422) (list 197 155.1999969482422)
                     (list 213 155.1999969482422) (list 229 155.1999969482422)
                     (list 248 156.1999969482422) (list 268 156.1999969482422)
                     (list 284 156.1999969482422) (list 297 155.1999969482422)
                     (list 303 155.1999969482422) (list 304 155.1999969482422)
                     (list 304 154.1999969482422) (list 305 154.1999969482422)
                     (list 305 158.1999969482422) (list 306 164.1999969482422)
                     (list 307 178.1999969482422) (list 307 196.1999969482422)
                     (list 307 212.1999969482422) (list 307 223.1999969482422)
                     (list 306 233.1999969482422) (list 305 238.1999969482422)
                     (list 305 243.1999969482422) (list 305 247.1999969482422)
                     (list 305 251.1999969482422) (list 305 253.1999969482422)
                     (list 305 255.1999969482422) (list 305 256.1999969482422)
                     (list 305 257.1999969482422)) templates 20) 't)
;;k-point-rec: gesture TL Num-> Sym
(define (k-point-rec candidate template-library k)
  (first (first (get-points template-library (match-searcher candidate template-library 0 0 0 k))))
  )
;;(match-searcher candidate template min index total k)
;; candidate is the candidate of the image
;;template= template-library
;;min is the smallest 5match distance recorded
;;index is the index location of the letter that possesses the smallest 5match
;;total takes in the number of times the loop is undergone. 
;;k is the number of points taken to get the result
;;outputs the index-1 to get the letter
(define (match-searcher candidate template min index total k)
  (cond
    [(empty? template) (list index)]
    [(zero? total) (match-searcher candidate (rest template)
                                    (geometric-match candidate (second (first template)) k)
                                    total (add1 total) k)]
    [(< (geometric-match candidate (second (first template)) k) min)
     (match-searcher candidate (rest template)
                      (geometric-match candidate (second (first template)) k)
                      total (add1 total) k)]
    [else (match-searcher candidate (rest template) min index (add1 total) k)]
    )
  )
  

              