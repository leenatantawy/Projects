#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require typed/test-engine/racket-tests)

;; An (Option T) is either 'None or (Some x),
;; where x has type T
(define-type (Option T) (U 'None (Some T)))
(define-struct (Some T) ([value : T]))

(define-struct (Pair A B)
  ([fst : A]
   [snd : B]))

(define-type Placement (U 'placed 'in 'out 'unknown))

(define-struct Tile
  ([letter : (Option Char)]
   [placement : Placement]))


(: hiddensearch : Char (Listof Char) -> Boolean)
;helper that searches hidden
;checks if the first tile is placed
(define (hiddensearch guessfirst hidden)
           (match hidden
             [(cons hiddenfirst hiddenrest) (cond
                                              [(char=? guessfirst hiddenfirst)
                                               #t]
                                              [else #f])]
             ['() #f]))
;tests for hiddensearch
(check-expect (hiddensearch #\a (list #\p #\a #\p #\e #\r)) #f)
(check-expect (hiddensearch #\a (list #\a #\p #\p #\l #\e
                                      )) #t)
(check-expect (hiddensearch #\a '()) #f)
(check-expect (hiddensearch #\a (list #\a)) #t)


(: createplacedlist : (Listof Char) (Listof Char) -> (Listof (U Char Tile)))
;helper function for compute-placed-and-leftover that creates Pair-fst
;computes whcih letters are placed in the guess
;keeps the unplaced letters as Chars
(define (createplacedlist guess hidden)
  (match hidden
    [(cons hiddenfirst hiddenrest)
     (match guess
       [(cons guessfirst guessrest) (cond
                                      [(hiddensearch guessfirst hidden)
                                       (cons (Tile (Some guessfirst)
                                                   'placed)
                                             (createplacedlist
                                              guessrest hiddenrest))]
                                      [else (cons guessfirst
                                                  (createplacedlist
                                                   guessrest hiddenrest))]

                                      )]
       ['() '()])]
     ['() '()]))
   
;tests for createplacedlist
(check-expect (createplacedlist (string->list "spade") (string->list "heart"))
              (list #\s #\p (Tile (Some #\a) 'placed) #\d #\e))
(check-expect (createplacedlist (string->list "modem") (string->list "smarm"))
              (list #\m #\o #\d #\e (Tile (Some #\m) 'placed)))
(check-expect (createplacedlist (string->list "spade") '()) '())
(check-expect (createplacedlist '() (string->list "spade")) '())

 
(: createleftoverlist : (Listof Char) (Listof Char) -> (Listof Char))
;helper function that creates Pair-snd, leftover
;creates a list of leftover unplaced letters in the hidden word
(define (createleftoverlist guess hidden)
  (match guess
        [(cons guessfirst guessrest)
         (match hidden
           [(cons hiddenfirst hiddenrest) (cond
                                            [(hiddensearch guessfirst hidden)
                                             (createleftoverlist
                                              guessrest hiddenrest)]
                                             [else (cons hiddenfirst
                                                    (createleftoverlist
                                                    guessrest hiddenrest))])
                                          ]
           ['() '()])]
  ['() '()]))
;tests for createleftoverlist
(check-expect (createleftoverlist (string->list "smarm") (string->list"modem"))
              '(#\m #\o #\d #\e))
(check-expect (createleftoverlist (string->list "smarm") (string->list "smarm"))
              '())
(check-expect (createleftoverlist (string->list "s") (string->list "s")) '())
(check-expect (createleftoverlist '() (string->list "smarm")) '())
(check-expect (createleftoverlist (string->list "smarm") '()) '())



(: compute-placed-and-leftover : (Listof Char) (Listof Char)
   -> (Pair (Listof (U Char Tile)) (Listof Char)))
;;checks which letters are un the right place and which are leftover
;;helper function for evaluate-guess
;;takes in a hidden word and a guess (both lists of characters), outputs a pair
;;Pair-fst: a list for the guess, placed letters are tile, unplaced are Char
;;Pair-snd: a list of letters from the hidden word that were not placed
(define (compute-placed-and-leftover guess hidden)
      (Pair (createplacedlist guess hidden) (createleftoverlist guess hidden)))

;tests for compute-placed-and-leftover
(check-expect (compute-placed-and-leftover (string->list "smarm")
                                           (string->list "modem"))
              (Pair (list #\s #\m #\a #\r (Tile (Some #\m) 'placed))
                     '(#\m #\o #\d #\e)))
              
(check-expect (compute-placed-and-leftover (string->list "spade")
                                           (string->list "heart"))
              (Pair (list #\s #\p (Tile (Some #\a) 'placed) #\d #\e)
                     '(#\h #\e #\r #\t)))
(check-expect (compute-placed-and-leftover (string->list "mummy")
                                           (string->list "smarm"))
              (Pair '(#\m #\u #\m #\m #\y) '(#\s #\m #\a #\r #\m)))
(check-expect (compute-placed-and-leftover '() '()) (Pair '() '()))
(check-expect (compute-placed-and-leftover (string->list "smarm")
                                           (string->list "smarm"))
              (Pair
 (list
  (Tile (Some #\s) 'placed)
  (Tile (Some #\m) 'placed)
  (Tile (Some #\a) 'placed)
  (Tile (Some #\r) 'placed)
  (Tile (Some #\m) 'placed))
 '()))
(check-expect (compute-placed-and-leftover (string->list "smarm") '())
              (Pair '() '()))
(check-expect (compute-placed-and-leftover '() (string->list "smarm"))
              (Pair '() '()))
(compute-placed-and-leftover '(#\h #\e #\a #\r #\t) '(#\s #\p #\a #\d #\e))


                                        
(: fillhidden : Char (Listof Char) -> Boolean)
;;helper function for fill-unplaced
;checks if the letters are in the hidden word and returns a boolean
;;retunrs false if not in the hidden word 
(define (fillhidden placedf hidden)
  (match hidden
    [(cons hiddenf hiddenr)(cond
                             [(char=? placedf hiddenf) #t]
                             [else
                              (fillhidden placedf hiddenr)])]
                                   
    ['() #f]))

;tests for fillhidden
(check-expect (fillhidden #\a (list #\p #\a #\p #\e #\r)) #t)
(check-expect (fillhidden #\a '()) #f)
(check-expect (fillhidden #\t (list #\p #\a #\p #\e #\r)) #f)

(: removeletter : (Listof (U Char Tile)) (Listof Char) -> (Listof Char))
;helper function to remove appropriate letter from hidden word
;;removes letters once they are established to be in or placed in hidden
;;returns a new hidden word with the removed 'placed or 'in letters
(define (removeletter placed hidden)
  (match hidden
    [(cons hiddenf hiddenr)
     (match placed
       [(cons placedf placedr) (cond
                                 [(char? placedf) (cond
                                  [(fillhidden placedf hidden)
                                   (removeletter placedr hiddenr)]
                                  [else (cons hiddenf
                                              (removeletter placedr hiddenr))])]
                               [else (removeletter placedr hiddenr)]
                                 )]
       ['() '()]
       )]
    ['() '()]))
;tests for removeletter
(check-expect (removeletter (list #\s #\p (Tile (Some #\a) 'placed) #\d #\e)
                     '()) '())
(check-expect (removeletter '()
                     (list #\s #\p #\d #\e)) '())
(check-expect (removeletter (list #\s #\p (Tile (Some #\a) 'placed) #\d #\e)
                            (string->list "heart")) '(#\h #\e #\r #\t))
(check-expect (removeletter (list
  (Tile (Some #\s) 'placed)
  (Tile (Some #\m) 'placed)
  (Tile (Some #\a) 'placed)
  (Tile (Some #\r) 'placed)
  (Tile (Some #\m) 'placed)) (string->list "smarm")) '())


(: fill-unplaced : (Listof (U Char Tile)) (Listof Char) -> (Listof Tile))
;; fills tiles for the leftover letters from compute-placed-and-leftover
;; helper function for evaluate guess
;; output will be the output of evaluate-guess
(define (fill-unplaced placed hidden)
  (match placed
    [(cons placedf placedr) (cond
                              [(char? placedf)
                               (cond             
                                 [(fillhidden placedf hidden)
                                  (cons (Tile (Some placedf) 'in)
                                         (fill-unplaced placedr
                                                        (removeletter
                                                         placed hidden)))
                                   ]
                                 [else (cons (Tile (Some placedf)'out)
                                             (fill-unplaced placedr hidden))])]
                              [else (cons placedf (fill-unplaced placedr hidden)
                                          )])]
    ['() '()]))

;tests for fill-unplaced
(check-expect (fill-unplaced (list #\s #\p (Tile (Some #\a) 'placed) #\d #\e)
               '(#\h #\e #\r #\t)) (list
 (Tile (Some #\s) 'out)
 (Tile (Some #\p) 'out)
 (Tile (Some #\a) 'placed)
 (Tile (Some #\d) 'out)
 (Tile (Some #\e) 'in)))
              
(check-expect (fill-unplaced (string->list "mummy") (string->list "smarm"))
              (list
 (Tile (Some #\m) 'in)
 (Tile (Some #\u) 'out)
 (Tile (Some #\m) 'in)
 (Tile (Some #\m) 'out)
 (Tile (Some #\y) 'out)))
(check-expect (fill-unplaced (list (Tile (Some #\s) 'placed) (Tile (Some #\m)
                                                                   'placed)
                     (Tile (Some #\a) 'placed) (Tile (Some #\r) 'placed)
                     (Tile (Some #\m) 'placed))
               '()) (list
 (Tile (Some #\s) 'placed)
 (Tile (Some #\m) 'placed)
 (Tile (Some #\a) 'placed)
 (Tile (Some #\r) 'placed)
 (Tile (Some #\m) 'placed)))



(: evaluate-guess : String String -> (Listof Tile))
;;evaluate-guess takes in the hidden and guess words
;;computes placements for each letter of the guess
;returns a list of tiles
(define (evaluate-guess word guess)
  (compute-placed-and-leftover (string->list guess) (string->list word))
  (local
    {(define placed
       (compute-placed-and-leftover (string->list guess) (string->list word)))}
  (fill-unplaced (Pair-fst placed) (string->list word))))

;tests for evaluate-guess
(check-expect (evaluate-guess "heart" "spade") (list
 (Tile (Some #\s) 'out)
 (Tile (Some #\p) 'out)
 (Tile (Some #\a) 'placed)
 (Tile (Some #\d) 'out)
 (Tile (Some #\e) 'in)))
(check-expect (evaluate-guess "a" "a") 
(list (Tile (Some #\a) 'placed)))
(check-expect (evaluate-guess "smarm" "mummy") (list
 (Tile (Some #\m) 'in)
 (Tile (Some #\u) 'out)
 (Tile (Some #\m) 'in)
 (Tile (Some #\m) 'out)
 (Tile (Some #\y) 'out)))
(check-expect (evaluate-guess " " " ") (list (Tile (Some #\space) 'placed)))
(evaluate-guess "heart" "spade")
(evaluate-guess "smarm" "mummy")



;; (AssocList K V) is the type of an association list mapping keys of
;; type K to values of type V.
(define-type (AssocList K V) (Listof (Pair K V)))

;; (ColorSpec text-color bg-color outline-color) documents the colors
;; that should be used to draw a box with text.
(define-struct ColorSpec
  ([text-color : (Option Image-Color)] ;; If 'None, don't show text
   [bg-color : (Option Image-Color)] ;; If 'None, transparent
   [outline-color : (Option Image-Color)])) ;; If 'None, no outline

;; (BoxProps tile-width tile-height spacer-size placement-to-colorspec)
;; specifies how to draw a tile as a box. The box should have the specified
;; tile-width and tile-height, and the colors are determined by looking
;; up the tile's placement in the association list placement-to-colorspec.
;; When drawing a grid of tiles, the tile should be separated vertically
;; and horizontally by invisible spacer squares of side length spacer-size.
(define-struct BoxProps
  ([tile-width : Real]
   [tile-height : Real]
   [spacer-size : Real]
   [placement-to-colorspec : (AssocList Placement ColorSpec)]))

(: to-byte : Integer -> Byte)
;; Converts an integer to a byte (used for specifying a font size).
(define (to-byte n)
  (if (byte? n)
      n
      (error "to-byte: n is not a byte")))

(: bold-text : String Real Image-Color -> Image)
;; Makes an image of the string str using the provided
;; font-size and text color, using a bold font.
(define (bold-text str font-size text-color)
  (text/font str
             (to-byte (exact-round font-size))
             text-color                    
             "Gill Sans"                    
             'swiss                    
             'normal                    
             'bold                    
             #f))

(define standard-colors-partial : (AssocList Placement ColorSpec)
  (list (Pair 'placed (ColorSpec (Some 'white) (Some 'seagreen) 'None))
        (Pair 'in (ColorSpec (Some 'white) (Some 'goldenrod) 'None))
        (Pair 'out (ColorSpec (Some 'white) (Some 'dimgray) 'None))))

(define standard-gameboard-colors : (AssocList Placement ColorSpec)
  (cons (Pair 'unknown (ColorSpec (Some 'black) 'None (Some 'black)))
        standard-colors-partial))

(define standard-keyboard-colors : (AssocList Placement ColorSpec)
  (cons (Pair 'unknown (ColorSpec (Some 'black) (Some 'lightgray) 'None))
        standard-colors-partial))

(define standard-gameboard-props : BoxProps
  (BoxProps 50 50 5 standard-gameboard-colors))

(define standard-keyboard-props : BoxProps
  (BoxProps 24.3 40 3 standard-keyboard-colors))

(: find : All (K V) (AssocList K V) K (K K -> Boolean) -> (Option V))
;; matches tile placement with it's appropriate colorspecs
;;uses an Association List to match the key with its colorspecs
(define (find assoc-lst search-key k=?)
  (match assoc-lst
    [(cons (Pair key value) rest-of-list)
     (cond
       [(k=? key search-key)
        (Some value)]
       [else (find rest-of-list search-key k=?)])]
    ['() 'None]))

;tests for find
(check-expect (find  (list (Pair 'placed (ColorSpec (Some 'white)
                                                    (Some 'seagreen) 'None))
        (Pair 'in (ColorSpec (Some 'white) (Some 'goldenrod) 'None))
        (Pair 'out (ColorSpec (Some 'white) (Some 'dimgray) 'None))) 'placed
                                                                     symbol=?)
              (Some (ColorSpec (Some 'white) (Some 'seagreen) 'None)))
(check-expect (find standard-gameboard-colors 'unknown symbol=?) (Some
                                                                  (ColorSpec
                                                          (Some 'black) 'None
                                                          (Some 'black))))
(check-expect (find standard-gameboard-colors 'placed symbol=?)
              (Some (ColorSpec (Some 'white) (Some 'seagreen) 'None)))

(check-expect (find standard-keyboard-colors 'unknown symbol=?)
              (Some (ColorSpec (Some 'black) (Some 'lightgray) 'None)))

(check-expect (find standard-keyboard-colors 'placed symbol=?)
              (Some (ColorSpec (Some 'white) (Some 'seagreen) 'None)))
(check-expect (find standard-colors-partial 'placed symbol=?)
              (Some (ColorSpec (Some 'white) (Some 'seagreen) 'None))
)
(check-expect (find standard-colors-partial 'in symbol=?)
              (Some (ColorSpec (Some 'white) (Some 'goldenrod) 'None))
)
(check-expect (find standard-colors-partial 'out symbol=?)
              (Some (ColorSpec (Some 'white) (Some 'dimgray) 'None))
)

           

(: computevalue : (U 'None (Some ColorSpec)) -> ColorSpec)
;matches none and some colorspec
;returns the colorspec for (Some ColorSpec)
;takes in an option and returns its value
(define (computevalue option)
  (match option
    [(Some ColorSpec) ColorSpec]
    ['None (ColorSpec 'None 'None 'None)])

  )
;tests for computevalue
(check-expect (computevalue (Some (ColorSpec (Some 'white)
                                             (Some 'seagreen) 'None)))
              (ColorSpec (Some 'white) (Some 'seagreen) 'None))
(check-expect (computevalue 'None) (ColorSpec 'None 'None 'None))


(: computeimage-color : (U 'None (Some Image-Color)) -> Image-Color)
;computes an image-color from (Some Image-Color) to put into draw-tile
;;cpmputes the value for an option
;if option is none then returns transparent image color
(define (computeimage-color colr)
  (match colr
    [(Some Image-Color) Image-Color]
    ['None (color 0 0 0 0)]))

;tests for computeimage-color
(check-expect (computeimage-color (Some 'white)) 'white)
(check-expect (computeimage-color 'None) (color 0 0 0 0))



(: computeoutline-color : (U 'None (Some Image-Color)) -> Image-Color)
;computeoutline-color takes in an option and returns its value
;returns a transparent image color if given none
(define (computeoutline-color colr)
  (match colr
    [(Some Image-Color) Image-Color]
    ['None (color 0 0 0 0)])
  )
;tests for computeoutline-color
(check-expect (computeoutline-color (Some 'white)) 'white)
(check-expect (computeoutline-color 'None) (color 0 0 0 0))

(: computechar : (U 'None (Some Char)) -> Char)
;takes in an option and returns its value
;if given (Some Char) returns Char
;;if given none returns a space to represent theres no character
(define (computechar letter)
(match letter
  [(Some Char) Char]
  ['None #\ ]

  ))
;tests computeoutline-color
(check-expect (computeoutline-color (Some 'white)) 'white)
(check-expect (computeoutline-color 'None) (color 0 0 0 0))


(: draw-tile : BoxProps Tile -> Image)
;drawtile takes in boxprops and a tile
;produces an image with the corresponding colorspecs to Tile-placement
;puts Tile-letter Char text on top
(define (draw-tile createtile tile)
  (local{
         (: associatefind : BoxProps Tile -> (U 'None (Some ColorSpec)))
         (define (associatefind createtile tile)
           (find (BoxProps-placement-to-colorspec createtile)
                 (Tile-placement tile) symbol=?))}
    (underlay
     (rectangle (BoxProps-tile-width createtile)
                (BoxProps-tile-height createtile)
                "outline"
                (computeoutline-color (ColorSpec-outline-color
                                       (computevalue (associatefind createtile
                                                                    tile)))))
     (rectangle (BoxProps-tile-width createtile)
                (BoxProps-tile-height createtile)
                "solid"
                (computeimage-color (ColorSpec-bg-color (computevalue
                                                         (associatefind
                                                          createtile tile
                                                          )))))
     (bold-text (string-upcase (string (computechar (Tile-letter tile))))
                (to-byte 38)
                (computeimage-color (ColorSpec-text-color
                                     (computevalue (associatefind createtile
                                                                  tile))
                                     ))
                ))
    ))
;tests for draw-tile
(draw-tile (BoxProps 50 50 10 standard-colors-partial) (Tile (Some #\s)
                                                               'unknown))
(draw-tile (BoxProps 50 50 10 standard-colors-partial) (Tile (Some #\s)
                                                               'placed))
(draw-tile (BoxProps 50 50 10 standard-colors-partial) (Tile (Some #\s)
                                                               'in))
(draw-tile (BoxProps 50 50 10 standard-colors-partial) (Tile (Some #\s)
                                                               'out))

(define spacer-opacity : Byte 100)

(: spacer : Real -> Image)
;; Makes a fully transparent square of the given side length.
(define (spacer size) (square size spacer-opacity "red"))

(: draw-row : BoxProps (Listof Tile) -> Image)
;;draw row takes in a guess and bosprops
;returns an image of the guess tile list letters
;using draw-tile to correspond the tiles to their appropriate colorspec
;;also places a spacer square, length specified by spacer-size between each tile
(define (draw-row boxprops guesslst)
  (foldl (lambda ([tile : Tile] [img : Image])
           (beside
            img
            (spacer (BoxProps-spacer-size boxprops))
            (draw-tile boxprops tile)
            ))
         (draw-tile standard-gameboard-props (first guesslst))
         (rest guesslst)))

;test for draw-row
(draw-row standard-gameboard-props (evaluate-guess "heart" "spade"))
(draw-row standard-gameboard-props (evaluate-guess "a" "a"))

(: draw-grid : BoxProps (Listof (Listof Tile)) -> Image)
;;turns a list of tiles which is a list of rows
;;returns an image depicting a grid of tiles
;;uses the size and colors specified by the BoxProps value
;;puts a spacer between each depiction of a row
(define (draw-grid boxprops allguesses)
  (foldr (lambda ([tiles : (Listof Tile)] [img : Image])
           (above
            (draw-row boxprops tiles)
            (rectangle
             (BoxProps-spacer-size boxprops)
             (BoxProps-spacer-size boxprops)
             spacer-opacity "red")
            img
            ))
         (draw-row standard-gameboard-props (first allguesses))
         (rest allguesses)))

;test for draw-grid
(draw-grid standard-gameboard-props
           (list (evaluate-guess "funny" "paren")
                 (evaluate-guess "funny" "folds")
                 (evaluate-guess "funny" "typed")))
(draw-grid standard-gameboard-props
           (list (evaluate-guess "a" "a")
                 (evaluate-guess "at" "the")
                 (evaluate-guess " " " ")))

(: char-to-tilelst : (Listof Char) -> (Listof Tile))
;;string to tile list takes in a list of characters
;; returns a list of tiles all with the placement 'unknown
;;helper function for make-new-keyboard-tiles
(define (char-to-tilelst lst)
  (match lst
    [(cons letter string)
     (cons (Tile (Some letter) 'unknown) (char-to-tilelst string))]
    ['() '()]))
  
;tests for string-to-tilelst
(check-expect (char-to-tilelst (string->list "zxcvbnm")) (list
 (Tile (Some #\z) 'unknown)
 (Tile (Some #\x) 'unknown)
 (Tile (Some #\c) 'unknown)
 (Tile (Some #\v) 'unknown)
 (Tile (Some #\b) 'unknown)
 (Tile (Some #\n) 'unknown)
 (Tile (Some #\m) 'unknown)))
(check-expect (char-to-tilelst (string->list " ")) (list (Tile (Some #\space)
                                                                 'unknown)))
(check-expect (char-to-tilelst (string->list "a")) (list (Tile (Some #\a)
                                                                 'unknown)))

  
 
(: make-new-keyboard-tiles : (Listof String) -> (Listof (Listof Tile)))
;takes in a list of strings where each string is a keyboard row
;returns a list of tiles which all have unknown placements
(define (make-new-keyboard-tiles stringlist)
 (map (lambda ([strng : String])
               (char-to-tilelst (string->list strng)))
          stringlist))
;tests for make-new-keyboard-tiles
(check-expect
 (make-new-keyboard-tiles (list "zxcvbnm" "asdfghjkl" "qwertyuiop"))
(list
 (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown))
 (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'unknown)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'unknown))))


(: placement->integer : Placement -> Integer)
;takes in a placemnet and gives an integer to be used in placement<?
;each placement corresponds with an integer based on its priority
(define (placement->integer p)
  (match p
    ['placed 4]
    ['in 3]
    ['out 2]
    ['unknown 1]))
;tests for placement->integer
(check-expect (placement->integer 'placed) 4)
(check-expect (placement->integer 'in) 3)
(check-expect (placement->integer 'out) 2)
(check-expect (placement->integer 'unknown) 1)

(: placement<? : Placement Placement -> Boolean)
;placement<? checks which placment has a higher priority
;set the placement in the keyboard
(define (placement<? p1 p2)
  (if (> (placement->integer p2) (placement->integer p1))
      #t
      #f))
;tests for placement<?
(check-expect (placement<? 'out 'placed) #t)
(check-expect (placement<? 'out 'out) #f)
(check-expect (placement<? 'placed 'out) #f)



(: updateplacement : Tile Tile -> Tile)
;updateplacement checks the placement<?
;updates the keyboard tile with the higher priority placement
(define (updateplacement keybrdtile guesstile)
  (cond
    [(char=? (computechar (Tile-letter keybrdtile)) (computechar (Tile-letter
                                                                  guesstile)))
     (cond
       [(placement<? (Tile-placement keybrdtile) (Tile-placement guesstile))
        (Tile (Tile-letter guesstile) (Tile-placement guesstile))]
       [else (Tile (Tile-letter keybrdtile) (Tile-placement keybrdtile))])]
    [else keybrdtile]))

;tests for updateplacement
(check-expect (updateplacement (Tile (Some #\a) 'placed) (Tile (Some #\a) 'out))
(Tile (Some #\a) 'placed))
(check-expect (updateplacement (Tile (Some #\a) 'placed) (Tile (Some #\f) 'out))
(Tile (Some #\a) 'placed))
(check-expect (updateplacement (Tile (Some #\a) 'out) (Tile (Some #\a) 'placed))
(Tile (Some #\a) 'placed))

     
(: findkeybrdtile : Tile (Listof Tile) -> (Listof Tile))
;;takes ina  tile and a list of tiles
;;finds the correspond letter in the keyboard
;updates the placement on the keyboard with the higher priority placement
(define (findkeybrdtile guesstile keybrdtiles)
 (map (lambda ([keybrd : Tile]) (updateplacement keybrd guesstile))
  keybrdtiles))

;test for findkeybrdtile
(check-expect (findkeybrdtile (Tile (Some #\a) 'placed) (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))) (list
 (Tile (Some #\a) 'placed)
 (Tile (Some #\s) 'unknown)
 (Tile (Some #\d) 'unknown)
 (Tile (Some #\f) 'unknown)
 (Tile (Some #\g) 'unknown)
 (Tile (Some #\h) 'unknown)
 (Tile (Some #\j) 'unknown)
 (Tile (Some #\k) 'unknown)
 (Tile (Some #\l) 'unknown)))
(check-expect (findkeybrdtile (Tile (Some #\d) 'in) (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))) (list
 (Tile (Some #\a) 'unknown)
 (Tile (Some #\s) 'unknown)
 (Tile (Some #\d) 'in)
 (Tile (Some #\f) 'unknown)
 (Tile (Some #\g) 'unknown)
 (Tile (Some #\h) 'unknown)
 (Tile (Some #\j) 'unknown)
 (Tile (Some #\k) 'unknown)
 (Tile (Some #\l) 'unknown)))
(check-expect (findkeybrdtile (Tile (Some #\a) 'placed) (list
  (Tile (Some #\a) 'unknown)))
              (list
               (Tile (Some #\a) 'placed)
               ))
  

(: update-keyboard : Tile (Listof (Listof Tile)) -> (Listof (Listof Tile)))
;takes in a tile from a guess and list of tiles representing a keyboard
;does a functional update on the keyboard and incorporates info from guess tile
(define (update-keyboard tile keybrdlst)
 (map (lambda ([guesslst : (Listof Tile)]) (findkeybrdtile tile guesslst))
      keybrdlst))

;tests for update-keyboard
(check-expect (update-keyboard (Tile (Some #\g) 'placed) (list
 (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown))
 (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'unknown)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'unknown)))) 
(list
 (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown))
 (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'placed)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'unknown)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'unknown))))
(check-expect (update-keyboard (Tile (Some #\z) 'unknown) (list (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown)))) (list
 (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown))))
(update-keyboard (Tile (Some #\z) 'in) (list (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown))))


(: multi-update-keyboard : (Listof Tile) (Listof (Listof Tile))
   -> (Listof (Listof Tile)))
;takes in a list of tiles from a guess and a list of list of tiles
;list of list of tiles represents a keyboard
;does a functional updare on the keyboard, incorporaying each guess tile
(define (multi-update-keyboard guesslst keybrdlst)
  (foldr update-keyboard keybrdlst guesslst))

;tests for multi-update-keyboard
(check-expect (multi-update-keyboard (evaluate-guess "heart" "spade")
                                     (list (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown))
 (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'unknown)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'unknown))))
              (list
 (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown))
 (list
  (Tile (Some #\a) 'placed)
  (Tile (Some #\s) 'out)
  (Tile (Some #\d) 'out)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'in)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'unknown)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'out))))
(check-expect (multi-update-keyboard (evaluate-guess "heart" "zoom") (list (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown))
 (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'unknown)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'unknown))))
(list
 (list
  (Tile (Some #\z) 'out)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'out))
 (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'unknown)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'out)
  (Tile (Some #\p) 'unknown))))
(check-expect (multi-update-keyboard (evaluate-guess "smarm" "mummy")
                                     (list (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown))
 (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'unknown)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'unknown)))) (list
 (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'in))
 (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'out)
  (Tile (Some #\u) 'out)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'unknown))))
(multi-update-keyboard (evaluate-guess "heart" "spade") (list (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown)
  (Tile (Some #\n) 'unknown)
  (Tile (Some #\m) 'unknown))
 (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'unknown)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'unknown))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'unknown)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'unknown))))




(test)
  