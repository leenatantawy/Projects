#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)
(require typed/2htdp/batch-io)
(require "bst-set-leenat.rkt")


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




(: compute-placed-and-leftover : (Listof Char) (Listof Char)
   -> (Pair (Listof (U Char Tile)) (Listof Char)))
;;checks which letters are un the right place and which are leftover
;;helper function for evaluate-guess
;;takes in a hidden word and a guess (both lists of characters), outputs a pair
;;Pair-fst: a list for the guess, placed letters are tile, unplaced are Char
;;Pair-snd: a list of letters from the hidden word that were not placed
(define (compute-placed-and-leftover guess hidden)
      (Pair (createplacedlist guess hidden) (createleftoverlist guess hidden)))



                                        
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

           

(: computevalue : (U 'None (Some ColorSpec)) -> ColorSpec)
;matches none and some colorspec
;returns the colorspec for (Some ColorSpec)
;takes in an option and returns its value
(define (computevalue option)
  (match option
    [(Some ColorSpec) ColorSpec]
    ['None (ColorSpec 'None 'None 'None)])

  )


(: computeimage-color : (U 'None (Some Image-Color)) -> Image-Color)
;computes an image-color from (Some Image-Color) to put into draw-tile
;;cpmputes the value for an option
;if option is none then returns transparent image color
(define (computeimage-color colr)
  (match colr
    [(Some Image-Color) Image-Color]
    ['None (color 0 0 0 0)]))




(: computeoutline-color : (U 'None (Some Image-Color)) -> Image-Color)
;computeoutline-color takes in an option and returns its value
;returns a transparent image color if given none
(define (computeoutline-color colr)
  (match colr
    [(Some Image-Color) Image-Color]
    ['None (color 0 0 0 0)])
  )


(: computechar : (U 'None (Some Char)) -> Char)
;takes in an option and returns its value
;if given (Some Char) returns Char
;;if given none returns a space to represent theres no character
(define (computechar letter)
(match letter
  [(Some Char) Char]
  ['None #\ ]

  ))


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
                (to-byte 30)
                (computeimage-color (ColorSpec-text-color
                                     (computevalue (associatefind createtile
                                                                  tile))
                                     ))
                ))
    ))

(define spacer-opacity : Byte 0)

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
         (draw-tile boxprops (first guesslst))
         (rest guesslst)))


(: draw-grid : BoxProps (Listof (Listof Tile)) -> Image)
;;turns a list of tiles which is a list of rows
;;returns an image depicting a grid of tiles
;;uses the size and colors specified by the BoxProps value
;;puts a spacer between each depiction of a row
(define (draw-grid boxprops allguesses)
  (foldl (lambda ([tiles : (Listof Tile)] [img : Image])
           (above
            (draw-row boxprops tiles)
            (rectangle
             (BoxProps-spacer-size boxprops)
             (BoxProps-spacer-size boxprops)
             spacer-opacity "red")
            img
            ))
         (draw-row boxprops (first allguesses))
         (rest allguesses)))


(: char-to-tilelst : (Listof Char) -> (Listof Tile))
;;string to tile list takes in a list of characters
;; returns a list of tiles all with the placement 'unknown
;;helper function for make-new-keyboard-tiles
(define (char-to-tilelst lst)
  (match lst
    [(cons letter string)
     (cons (Tile (Some letter) 'unknown) (char-to-tilelst string))]
    ['() '()]))
  

  
 
(: make-new-keyboard-tiles : (Listof String) -> (Listof (Listof Tile)))
;takes in a list of strings where each string is a keyboard row
;returns a list of tiles which all have unknown placements
(define (make-new-keyboard-tiles stringlist)
 (map (lambda ([strng : String])
               (char-to-tilelst (string->list strng)))
          stringlist))



(: placement->integer : Placement -> Integer)
;takes in a placemnet and gives an integer to be used in placement<?
;each placement corresponds with an integer based on its priority
(define (placement->integer p)
  (match p
    ['placed 4]
    ['in 3]
    ['out 2]
    ['unknown 1]))

(: placement<? : Placement Placement -> Boolean)
;placement<? checks which placment has a higher priority
;set the placement in the keyboard
(define (placement<? p1 p2)
  (if (> (placement->integer p2) (placement->integer p1))
      #t
      #f))



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



     
(: findkeybrdtile : Tile (Listof Tile) -> (Listof Tile))
;;takes ina  tile and a list of tiles
;;finds the correspond letter in the keyboard
;updates the placement on the keyboard with the higher priority placement
(define (findkeybrdtile guesstile keybrdtiles)
 (map (lambda ([keybrd : Tile]) (updateplacement keybrd guesstile))
  keybrdtiles))

  

(: update-keyboard : Tile (Listof (Listof Tile)) -> (Listof (Listof Tile)))
;takes in a tile from a guess and list of tiles representing a keyboard
;does a functional update on the keyboard and incorporates info from guess tile
(define (update-keyboard tile keybrdlst)
 (map (lambda ([guesslst : (Listof Tile)]) (findkeybrdtile tile guesslst))
      keybrdlst))



(: multi-update-keyboard : (Listof Tile) (Listof (Listof Tile))
   -> (Listof (Listof Tile)))
;takes in a list of tiles from a guess and a list of list of tiles
;list of list of tiles represents a keyboard
;does a functional updare on the keyboard, incorporaying each guess tile
(define (multi-update-keyboard guesslst keybrdlst)
  (foldr update-keyboard keybrdlst guesslst))


(: string-cmp : (Cmp String))
;; Comparison on strings
(define (string-cmp s1 s2)
  (cond
    [(string<? s1 s2) '<]
    [(string>? s1 s2) '>]
    [else '=]))

(: string-list->set : (Listof String) -> (Set String))
;;converts a list of strings to a set 
(define string-list->set (make-list->set string-cmp))

;test
(string-list->set '("a" "b" "c"))

;; The parameters of a Racketle game.
(define-struct GameParams
  ([title : String]
   [wordlength : Natural]
   [max-guesses : Natural]
   [hidden-set : (Set String)]
   [guess-set : (Set String)]))

(define gameparam
  (GameParams "Racketle" 5 6 (string-list->set
                              '("yield" "allow" "other" "break" "metal"))
              (string-list->set '("yield" "allow" "other" "break" "metal"
                                          "heart" "smarm" "mummy" "adieu"))))

(define gameparam2
  (GameParams "Racketle" 5 6 (string-list->set
                              '("yield"))
              (string-list->set '("yield" "allow" "other" "break" "metal"
                                          "heart" "smarm" "mummy" "adieu"))))
;; The current state of the game.
(define-struct Game
  ([params : GameParams]
   [hidden : String]
   [past-guesses : (Listof (Listof Tile))]
   [current-input : String]
   [keyboard : (Listof (Listof Tile))]
   [message : String]
   [ongoing? : Boolean]))

;;current games for testing
(define currentgame
  (Game gameparam (gen-random-element (GameParams-hidden-set
                                                         gameparam))
'() ""
                           (make-new-keyboard-tiles '("zxcvbnm"
                                                      "asdfghjkl"
                                                      "qwertyuiop"
                                                      ))
                           "" #t))
(define currentgame2
  (Game gameparam (gen-random-element (GameParams-hidden-set
                                                         gameparam))
 '()
 "yield"
                           (make-new-keyboard-tiles '("zxcvbnm"
                                                      "asdfghjkl"
                                                      "qwertyuiop"
                                                      ))
                           "" #t))
(define currentgame3
  (Game gameparam (gen-random-element (GameParams-hidden-set
                                                         gameparam))
'() "yiel"
                           (make-new-keyboard-tiles '("zxcvbnm"
                                                      "asdfghjkl"
                                                      "qwertyuiop"
                                                      ))
                           "" #t))
(define currentgame4
  (Game gameparam (gen-random-element (GameParams-hidden-set
                                                         gameparam))
'() "yiel"
                           (make-new-keyboard-tiles '("zxcvbnm"
                                                      "asdfghjkl"
                                                      "qwertyuiop"
                                                      ))
                           "" #f))

(define currentgame5
  (Game gameparam (gen-random-element (GameParams-hidden-set
                                                         gameparam))
(list (list
 (Tile (Some #\s) 'out)
 (Tile (Some #\p) 'out)
 (Tile (Some #\a) 'placed)
 (Tile (Some #\d) 'out)
 (Tile (Some #\e) 'in))
(list
 (Tile (Some #\m) 'in)
 (Tile (Some #\u) 'out)
 (Tile (Some #\m) 'in)
 (Tile (Some #\m) 'out)
 (Tile (Some #\y) 'out))
(list
 (Tile (Some #\t) 'out)
 (Tile (Some #\y) 'out)
 (Tile (Some #\p) 'out)
 (Tile (Some #\e) 'out)
 (Tile (Some #\d) 'in)))
 "yield"
                           (make-new-keyboard-tiles '("zxcvbnm"
                                                      "asdfghjkl"
                                                      "qwertyuiop"
                                                      ))
                           "" #f))
(: make-new-game : GameParams (Listof String) -> Game)
;;takes in the parameters for the game
;;also takes a list of strings describing the letters of the keyboard
;; hidden word is chosen randomly from the set of possible hidden words
;;no past guesses, current input is empty, message is blank
;;game is ongoing
(define (make-new-game gameparam keybrd)
  (Game
   gameparam
   (gen-random-element (GameParams-hidden-set gameparam))
   '()
   ""
   (make-new-keyboard-tiles keybrd)
   ""
   #t))
;test
(make-new-game gameparam '("zxcvbnm" "asdfghjkl" "qwertyuiop"))
              
   

(: restart-game : Game -> Game)
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Restarts the game, choosing a new random hidden word and clearing
;; out all guesses
(define (restart-game game)
  (Game
   (Game-params game)
   (gen-random-element (GameParams-hidden-set (Game-params game)))
   '()
   ""
   (map (lambda ([row : (Listof Tile)])
          (map (lambda ([tile : Tile]) (Tile (Tile-letter tile) 'unknown)) row))
        (Game-keyboard game))
   ""
   #t))

(: backspace : Game -> Game)
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Updates the state of the game based on the user pressing the
;; backspace key.
(define (backspace game)
  (match game
    [(Game _ _ _ _ _ _ #f) game]
    [(Game params hidden past-guesses current-input keyboard _ #t)
     (Game params
           hidden
           past-guesses
           (if (> (string-length current-input) 0)
               (substring current-input 0 (- (string-length current-input) 1))
               current-input)
           keyboard
           ""
           #t)]))

(: type-letter : Game String -> Game)
;;does a functional updarte on the game
;;updates the state of the game as player pressed a letter on the keyboard
;;if game is over then the game is not modified at all
(define (type-letter game str)
  (match game
    [(Game _ _ _ _ _ _ #f) game]
    [(Game params hidden past-guesses current-input keyboard message #t)
     (if (< (string-length current-input) (GameParams-wordlength
                                                (Game-params game)))
     (Game params
           hidden
           past-guesses
           (string-append current-input str)
           keyboard
           message
           #t)
     (Game params
           hidden
           past-guesses
           current-input
           keyboard
           "wordlength exceeded"
           #t))]))
;;tests for type-letter
(check-expect (Game-current-input (type-letter currentgame "s")) "s")
(check-expect (Game-message (type-letter currentgame2 "s"))
              "wordlength exceeded")
(check-expect (Game-current-input (type-letter currentgame3 "d")) "yield")
(check-expect (Game-current-input (type-letter currentgame4 "d"))
              (Game-current-input currentgame4))




(: game-validate-input : Game -> (Pair Boolean String))
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Determines whether the current-input of the game is a valid guess.
;; Returns a pair with a boolean indicating whether it is a valid
;; guess, and a string consisting of what the message should become
;; if the user attempts to submit this guess.
(define (game-validate-input game)
  (match game
    [(Game (GameParams _ wordlength max-guesses _ guess-set)
           _ past-guesses current-input _ old-message ongoing?)
     (cond
       [(> (string-length current-input) wordlength)
        (error "game-validate-input: current-input longer than wordlength")]
       [(not ongoing?) (Pair #f old-message)]
       [(>= (length past-guesses) max-guesses)
        (error "game-validate-input: out of guesses but still ongoing")]
       [(< (string-length current-input) wordlength) (Pair #f "")]
       [(not (member? current-input guess-set))
        (Pair #f "Not in word list")]
       [else (Pair #t "")])]))

;test game for submit-guess
(define submit-guess-game
  (Game gameparam (gen-random-element (GameParams-hidden-set
                                                         gameparam2))
(list (list
   (Tile (Some #\q) 'unknown)
   (Tile (Some #\w) 'unknown)
   (Tile (Some #\e) 'unknown)
   (Tile (Some #\r) 'unknown)
   (Tile (Some #\t) 'unknown)
   (Tile (Some #\y) 'placed)
   (Tile (Some #\u) 'unknown)
   (Tile (Some #\i) 'unknown)
   (Tile (Some #\o) 'unknown)
   (Tile (Some #\p) 'unknown))) "yield"
                           (make-new-keyboard-tiles '("zxcvbnm"
                                                      "asdfghjkl"
                                                      "qwertyuiop"
                                                      ))
                           "" #t))
;;test definitions for submit-guess
(define submit-guess-param
  (GameParams "Racketle" 5 2 (string-list->set
                              '("yield" "allow" "other" "break" "metal"))
              (string-list->set '("yield" "allow" "other" "break" "metal"
                                          "heart" "smarm" "mummy" "adieu"))))
(define submit-guess-game2
  (Game submit-guess-param (gen-random-element (GameParams-hidden-set
                                                         gameparam2))
        
 (list (list
   (Tile (Some #\q) 'unknown)
   (Tile (Some #\w) 'unknown)
   (Tile (Some #\e) 'placed)
   (Tile (Some #\r) 'unknown)
   (Tile (Some #\t) 'unknown)
   (Tile (Some #\y) 'placed)
   (Tile (Some #\u) 'unknown)
   (Tile (Some #\i) 'placed)
   (Tile (Some #\o) 'unknown)
   (Tile (Some #\p) 'unknown)))
  "allow"
                           (make-new-keyboard-tiles '("zxcvbnm"
                                                      "asdfghjkl"
                                                      "qwertyuiop"
                                                      ))
                           "" #t))
(: submit-guess : Game -> Game)
;;first checks if the current-input is a valid guess
;;the validity of the guess is determined by game-valid-input
;;if not valid message updated appropriately game state doesn't otherwise change
;;if guess is valid, add guess to list of past guesses
;;keyboard field needs to be updated to reflect the guess
;;current-input needs to be cleared out so next guess can be entered
;;if guess is correct the game needs to be declared over, message = "You win!"
;;if guess is incorrect and no more guesses need message to say game over, word
;;otherwise, game is ongoing, the message specified by game-validate-input
(define (submit-guess game)
  (match game
    [(Game params hidden past-guesses current-input keyboard _ #t)
     (if (Pair-fst (game-validate-input game))
      (cond
        [(string=? current-input hidden)
         (Game params
               hidden
               (cons (evaluate-guess hidden current-input) past-guesses)
               ""
               (multi-update-keyboard (evaluate-guess hidden current-input)
                                      keyboard
                                      )
               "You win!"
               #f)]
        [(>= (length (cons (evaluate-guess hidden current-input) past-guesses))
             (GameParams-max-guesses (Game-params game)))
         (Game params
               hidden
               (cons (evaluate-guess hidden current-input) past-guesses)
               ""
               (multi-update-keyboard (evaluate-guess hidden current-input)
                                      keyboard
                                      )
               (string-append "Game over. The word was " hidden ".")
               #f)]
        [else (Game params
                    hidden
                    (cons (evaluate-guess hidden current-input) past-guesses)
                    ""
                    (multi-update-keyboard (evaluate-guess hidden current-input)
                                           keyboard
                                           )
                    (Pair-snd (game-validate-input game))
                    #t)])
      (Game params
            hidden
            past-guesses
            current-input
            keyboard
            (Pair-snd (game-validate-input game))
            #t))]))
 

       
;tests out of guesses
(check-expect (Game-message (submit-guess submit-guess-game)) "You win!")
(check-expect (Game-ongoing? (submit-guess submit-guess-game)) #f)
(check-expect (Game-message (submit-guess submit-guess-game2))
               (string-append "Game over. The word was "
                              (Game-hidden submit-guess-game2) "."))
(check-expect (Game-keyboard (submit-guess submit-guess-game)) (list
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
   (Tile (Some #\d) 'placed)
   (Tile (Some #\f) 'unknown)
   (Tile (Some #\g) 'unknown)
   (Tile (Some #\h) 'unknown)
   (Tile (Some #\j) 'unknown)
   (Tile (Some #\k) 'unknown)
   (Tile (Some #\l) 'placed))
  (list
   (Tile (Some #\q) 'unknown)
   (Tile (Some #\w) 'unknown)
   (Tile (Some #\e) 'placed)
   (Tile (Some #\r) 'unknown)
   (Tile (Some #\t) 'unknown)
   (Tile (Some #\y) 'placed)
   (Tile (Some #\u) 'unknown)
   (Tile (Some #\i) 'placed)
   (Tile (Some #\o) 'unknown)
   (Tile (Some #\p) 'unknown))))
(check-expect (Game-past-guesses (submit-guess submit-guess-game)) (list
 (list
  (Tile (Some #\y) 'placed)
  (Tile (Some #\i) 'placed)
  (Tile (Some #\e) 'placed)
  (Tile (Some #\l) 'placed)
  (Tile (Some #\d) 'placed))
 (list
 (Tile (Some #\q) 'unknown)
 (Tile (Some #\w) 'unknown)
 (Tile (Some #\e) 'unknown)
 (Tile (Some #\r) 'unknown)
 (Tile (Some #\t) 'unknown)
 (Tile (Some #\y) 'placed)
 (Tile (Some #\u) 'unknown)
 (Tile (Some #\i) 'unknown)
 (Tile (Some #\o) 'unknown)
 (Tile (Some #\p) 'unknown))))
  

(: row-of-unknown : Natural String -> (Listof Tile))
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Makes a list of wordlength tiles with placement 'unknown,
;; where the letters in the string fill the first tiles in the
;; list, and the remaining tiles are empty).
(define (row-of-unknown wordlength current-input)
  (append (map (lambda ([letter : Char])
                 (Tile (Some letter) 'unknown))
               (string->list current-input))
          (make-list (max 0 (- wordlength (string-length current-input)))
                     (Tile 'None 'unknown))))
;test row-of-unknown
;(draw-row standard-gameboard-props (row-of-unknown 5 "hi"))


(: assemble-gameboard : Natural Natural (Listof (Listof Tile)) String ->
   (Listof (Listof Tile)))
;write recursive helper function
;takes in wordlength, max-guesses, past-guesses, and current-input
;assembles the list of list of tiles representing the gameboard
;draw a grid of tiles with max-guesses rows and wordlength columns
;the first several rows are occupied by the past guesses
;if the past guesses do not occupy all rows, next row should show current input
;all the following rows should be filled with empty tiles of placement unknown
(define (assemble-gameboard wrdlngth maxguess pastguess currentint)
  (cond
    [(>= (- maxguess (length pastguess)) 1)
     (assemble-gameboard wrdlngth maxguess
                         (cons (row-of-unknown wrdlngth currentint)
                               pastguess) "")]
    [else pastguess]))

(check-expect (assemble-gameboard 5 6 (Game-past-guesses currentgame) "yield")
              (list
 (list
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))
 (list
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))
 (list
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))
 (list
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))
 (list
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))
 (list
  (Tile (Some #\y) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\l) 'unknown)
  (Tile (Some #\d) 'unknown))))

(check-expect (assemble-gameboard 5 6 '() "hi") (list
 (list
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))
 (list
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))
 (list
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))
 (list
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))
 (list
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))
 (list
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown)
  (Tile 'None 'unknown))))
(check-expect (assemble-gameboard 5 2 (list (list
   (Tile (Some #\z) 'unknown)
   (Tile (Some #\x) 'unknown)
   (Tile (Some #\c) 'unknown)
   (Tile (Some #\v) 'unknown)
   (Tile (Some #\b) 'unknown))
  (list
   (Tile (Some #\a) 'unknown)
   (Tile (Some #\s) 'unknown)
   (Tile (Some #\d) 'placed)
   (Tile (Some #\f) 'unknown)
   (Tile (Some #\g) 'unknown))) "hi") (list
 (list
  (Tile (Some #\z) 'unknown)
  (Tile (Some #\x) 'unknown)
  (Tile (Some #\c) 'unknown)
  (Tile (Some #\v) 'unknown)
  (Tile (Some #\b) 'unknown))
 (list
  (Tile (Some #\a) 'unknown)
  (Tile (Some #\s) 'unknown)
  (Tile (Some #\d) 'placed)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown))))

              

;; /// World type definition provided
;; Keeps track of a Game as well as the size and color
;; properties needed to draw the game.
(define-struct WordWorld
  ([game : Game]
   [general-props : GeneralProps]
   [gameboard-props : BoxProps]
   [keyboard-props : BoxProps]))


  
;; General properties for drawing a world.
(define-struct GeneralProps
  ([width : Real]
   [margin : Real]
   [header-height : Real]
   [message-height : Real]
   [text-color : Image-Color]
   [bg-color : Image-Color]))


(define genprops
  (GeneralProps 270 20 50 25 'royalblue 'lightblue))
  
(define test-world1
  (WordWorld currentgame genprops standard-gameboard-props
             standard-keyboard-props))
(define test-world
  (WordWorld currentgame5 genprops standard-gameboard-props
             standard-keyboard-props))
  
(: draw-world : WordWorld -> Image)
;;a rectangle that creates the background with the title string
;;the gameboard
;;a rectangle that uses background color with the message string
;;an image of the keyboard
(define (draw-world world)
  (underlay
   (rectangle (+ (* 2 (GeneralProps-margin (WordWorld-general-props world)))
                 (GeneralProps-width (WordWorld-general-props world)))
              (+ (* 2 (GeneralProps-margin (WordWorld-general-props world)))
                 (* 2 (GeneralProps-header-height (WordWorld-general-props
                                              world)))
                 (image-height (draw-grid (WordWorld-gameboard-props world)
                                          (assemble-gameboard
                                           (GameParams-wordlength
                                            (Game-params (WordWorld-game
                                                          world)))
                                           (GameParams-max-guesses
                                            (Game-params (WordWorld-game
                                                          world)))
                                           (Game-past-guesses
                                            (WordWorld-game world))
                                           (Game-current-input
                                            (WordWorld-game world)))))
                 (image-height (draw-grid (WordWorld-keyboard-props world)
                                          (Game-keyboard (WordWorld-game
                                                          world)))))
              
              "solid"
              (GeneralProps-bg-color (WordWorld-general-props world)))
   (above
    (underlay
     (rectangle (GeneralProps-width (WordWorld-general-props world))
                (GeneralProps-header-height (WordWorld-general-props
                                             world))
                "solid"
                (GeneralProps-bg-color (WordWorld-general-props world)))
     (bold-text (GameParams-title (Game-params (WordWorld-game world)))
                (to-byte 38)
                (GeneralProps-text-color (WordWorld-general-props world)
                                         ))) 
    (draw-grid (WordWorld-gameboard-props world)
               (assemble-gameboard
                (GameParams-wordlength
                 (Game-params (WordWorld-game world)))
                (GameParams-max-guesses
                 (Game-params (WordWorld-game world)))
                (Game-past-guesses
                 (WordWorld-game world))
                (Game-current-input
                 (WordWorld-game world))))
    (underlay
     (rectangle (GeneralProps-width (WordWorld-general-props world))
                (GeneralProps-message-height (WordWorld-general-props
                                              world))
                "solid"
                (GeneralProps-bg-color (WordWorld-general-props world)))
     (bold-text (Game-message (WordWorld-game world))
                (to-byte 30)
                (GeneralProps-text-color (WordWorld-general-props world)
                                         )))
    (draw-grid (WordWorld-keyboard-props world)
               (Game-keyboard (WordWorld-game world))))))
;test
(draw-world test-world1)
(draw-world test-world)

(: world-replace-game : WordWorld Game -> WordWorld)
;;does a functional update on a world
;;replaces the world's game with the given one
;;used to update world when reacting to keyboard
(define (world-replace-game currentworld newgame)
  (match currentworld
    [(WordWorld game general-props gameboard-props keyboard-props)
     (WordWorld
     newgame
     general-props
     gameboard-props
     keyboard-props
     )]))
;tests for world-replace-game
(check-expect (Game-past-guesses (WordWorld-game (world-replace-game test-world
                                                   (submit-guess
                                                    submit-guess-game))))
              (list
 (list
  (Tile (Some #\y) 'placed)
  (Tile (Some #\i) 'placed)
  (Tile (Some #\e) 'placed)
  (Tile (Some #\l) 'placed)
  (Tile (Some #\d) 'placed))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'unknown)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'placed)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'unknown)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'unknown))))
(check-expect (Game-current-input (WordWorld-game (world-replace-game test-world
                                                   (submit-guess
                                                    submit-guess-game)))) "")
(check-expect (Game-keyboard (WordWorld-game (world-replace-game test-world
                                                   (submit-guess
                                                    submit-guess-game)))) (list
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
  (Tile (Some #\d) 'placed)
  (Tile (Some #\f) 'unknown)
  (Tile (Some #\g) 'unknown)
  (Tile (Some #\h) 'unknown)
  (Tile (Some #\j) 'unknown)
  (Tile (Some #\k) 'unknown)
  (Tile (Some #\l) 'placed))
 (list
  (Tile (Some #\q) 'unknown)
  (Tile (Some #\w) 'unknown)
  (Tile (Some #\e) 'placed)
  (Tile (Some #\r) 'unknown)
  (Tile (Some #\t) 'unknown)
  (Tile (Some #\y) 'placed)
  (Tile (Some #\u) 'unknown)
  (Tile (Some #\i) 'placed)
  (Tile (Some #\o) 'unknown)
  (Tile (Some #\p) 'unknown))))

(: is-letter? : String -> Boolean)
;; USED VERSION PROVIDED BY INSTRUCTOR
;; Checks if the given string is a letter.
(define (is-letter? str)
  (and (= (string-length str) 1)
       (or (string<=? "a" str "z") (string<=? "A" str "Z"))))

(: react-to-keyboard : WordWorld String -> WordWorld)
;;does a functional update on world based on the string input as user types
(define (react-to-keyboard world key)
  (cond
    [(is-letter? key)
     (match world
       [(WordWorld game general-props gameboard-props keyboard-props)
        (world-replace-game world
         (type-letter game (string-downcase key)))])]
     [(key=? key "\b")
      (match world
        [(WordWorld game general-props gameboard-props keyboard-props)
         (world-replace-game world
          (backspace game))])]
     [(key=? key "\r")
      (match world
        [(WordWorld game general-props gameboard-props keyboard-props)
         (world-replace-game world
          (submit-guess game))])]
     [(key=? key "escape")
      (match world
        [(WordWorld game general-props gameboard-props keyboard-props)
         (world-replace-game world
          (restart-game game)
          )])]
    
     [else world]
       ))
;tests for react-to-keyboard
(check-expect (Game-current-input (WordWorld-game
                                   (react-to-keyboard test-world1 "s"))) "s")
(check-expect (Game-current-input (WordWorld-game
                                   (react-to-keyboard test-world1 "\r"))) "")
(check-expect (Game-current-input (WordWorld-game
                                   (react-to-keyboard test-world "escape")))
              "")

(: run : GameParams (Listof String) GeneralProps BoxProps BoxProps -> WordWorld)
;;takes in the parameters of the game, a list of strings, gen props, boxprops
;;starts a new interactive game using big-bang
;;text in title bar should be title from gameparams
(define (run gameparams keyboard genprops gameprops keyprops)
  (big-bang (WordWorld (make-new-game gameparams keyboard)
                                   genprops gameprops keyprops) : WordWorld
    [to-draw draw-world]
    [on-key react-to-keyboard]
    [name (GameParams-title gameparam)]))


(define test-set-1 (string-list->set '("a" "b" "c")))
(check-expect (member? "a" test-set-1) #t)
(check-expect (member? "A" test-set-1) #f)
(check-expect (member? "c" test-set-1) #t)
(check-expect (member? "d" test-set-1) #f)

(test)
  