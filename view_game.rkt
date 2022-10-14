#lang racket

(require "button.rkt")
(require "level.rkt")
(require "utilities.rkt")
(require "view_levels.rkt")
(require "board.rkt")
(require "color.rkt")
(require "cell.rkt")
(require 2htdp/image)
(require lang/posn)
(require picturing-programs/private/map-image)

; Struct ViewGame : Button Button Button Button Button Board Number
;                                           list<Boolean> Boolean Boolean
(struct ViewGame [levels invisible reset previous next level level_index
                         unlocked change_to? invisible?])

; make-ViewGame : List<Boolean>  Number -> ViewGame
; Returns a ViewGame based on a list of unlocked levels.
(define (make-ViewGame levels unlocked level_index)
  (ViewGame (make_game_button GAME_LEVELS_X GAME_LEVELS_Y
                              GAME_LEVELS_W GAME_LEVELS_H
                              "Levels" #true)
            (make_game_button GAME_INVISIBLE_X GAME_INVISIBLE_Y
                              GAME_INVISIBLE_W GAME_INVISIBLE_H
                              "Invisible" #true)
            (make_game_button GAME_RESET_X GAME_RESET_Y
                              GAME_RESET_W GAME_RESET_H
                              "Reset" #true)
            (if (and (not (= 0 level_index))
                     (get unlocked (- level_index 1)))
                (make_game_button GAME_PREVIOUS_X GAME_PREVIOUS_Y
                                  GAME_PREVIOUS_W GAME_PREVIOUS_H
                                  "\u219E" #true)
                (make_game_button GAME_PREVIOUS_X GAME_PREVIOUS_Y
                                  GAME_PREVIOUS_W GAME_PREVIOUS_H
                                  "\u219E" #false))
            (if (and (not (= 29 level_index))
                     (get unlocked (+ level_index 1)))
                (make_game_button GAME_NEXT_X GAME_NEXT_Y
                                  GAME_NEXT_W GAME_NEXT_H
                                  "\u21A0" #true)
                (make_game_button GAME_NEXT_X GAME_NEXT_Y
                                  GAME_NEXT_W GAME_NEXT_H
                                  "\u21A0" #false))
            (get levels level_index)
            level_index
            unlocked
            #false
            #false))
            


;------------------ METHODS ---------------------;

; set-ViewGame-level : ViewGame Level
; Sets the level button of a given ViewGame.
(define (set-ViewGame-level game level)
  (ViewGame (ViewGame-levels game)
            (ViewGame-invisible game)
            (ViewGame-reset game)
            (ViewGame-previous game)
            (ViewGame-next game)
            level
            (ViewGame-level_index game)
            (ViewGame-unlocked game)
            (ViewGame-change_to? game)
            (ViewGame-invisible? game)))

; set-ViewGame-levels : ViewGame Levels
; Sets the levels button of a given ViewGame.
(define (set-ViewGame-levels game levels)
  (ViewGame levels
            (ViewGame-invisible game)
            (ViewGame-reset game)
            (ViewGame-previous game)
            (ViewGame-next game)
            (ViewGame-level game)
            (ViewGame-level_index game)
            (ViewGame-unlocked game)
            (ViewGame-change_to? game)
            (ViewGame-invisible? game)))

; set-ViewGame-next : ViewGame Button
; Sets the next button of a given ViewGame.
(define (set-ViewGame-next game next)
  (ViewGame (ViewGame-levels game)
            (ViewGame-invisible game)
            (ViewGame-reset game)
            (ViewGame-previous game)
            next
            (ViewGame-level game)
            (ViewGame-level_index game)
            (ViewGame-unlocked game)
            (ViewGame-change_to? game)
            (ViewGame-invisible? game)))

; set-ViewGame-previous : ViewGame Button
; Sets the previous button of a given ViewGame.
(define (set-ViewGame-previous game previous)
  (ViewGame (ViewGame-levels game)
            (ViewGame-invisible game)
            (ViewGame-reset game)
            previous
            (ViewGame-next game)
            (ViewGame-level game)
            (ViewGame-level_index game)
            (ViewGame-unlocked game)
            (ViewGame-change_to? game)
            (ViewGame-invisible? game)))

; set-ViewGame-level_index : ViewGame Number
; Sets the level_index of a given ViewGame.
(define (set-ViewGame-level_index game level_index)
  (ViewGame (ViewGame-levels game)
            (ViewGame-invisible game)
            (ViewGame-reset game)
            (ViewGame-previous game)
            (ViewGame-next game)
            (ViewGame-level game)
            level_index
            (ViewGame-unlocked game)
            (ViewGame-change_to? game)
            (ViewGame-invisible? game)))

; set-ViewGame-change_to? : ViewGame Boolean
; Sets the change_to? of a given ViewGame.
(define (set-ViewGame-change_to? game change_to?)
  (ViewGame (ViewGame-levels game)
            (ViewGame-invisible game)
            (ViewGame-reset game)
            (ViewGame-previous game)
            (ViewGame-next game)
            (ViewGame-level game)
            (ViewGame-level_index game)
            (ViewGame-unlocked game)
            change_to?
            (ViewGame-invisible? game)))

; set-ViewGame-reset : ViewGame Button
; Sets the reset button of a given ViewGame.
(define (set-ViewGame-reset game reset)
  (ViewGame (ViewGame-levels game)
            (ViewGame-invisible game)
            reset
            (ViewGame-previous game)
            (ViewGame-next game)
            (ViewGame-level game)
            (ViewGame-level_index game)
            (ViewGame-unlocked game)
            (ViewGame-change_to? game)
            (ViewGame-invisible? game)))

; set-ViewGame-invisible : ViewGame Button
; Sets the invisible button of a given ViewGame.
(define (set-ViewGame-invisible game invisible)
  (ViewGame (ViewGame-levels game)
            invisible
            (ViewGame-reset game)
            (ViewGame-previous game)
            (ViewGame-next game)
            (ViewGame-level game)
            (ViewGame-level_index game)
            (ViewGame-unlocked game)
            (ViewGame-change_to? game)
            (ViewGame-invisible? game)))

; ViewGame-reset-colors : ViewGame -> ViewGame
; Reset the background color of all buttons of a given ViewGame.
(define (ViewGame-reset-colors game)
  (ViewGame (set-Button-background (ViewGame-levels game) BACKGROUND_COLOR)
            (if (ViewGame-invisible? game)
                (ViewGame-invisible game)
                (set-Button-background (ViewGame-invisible game) BACKGROUND_COLOR))
            (set-Button-background (ViewGame-reset game) BACKGROUND_COLOR)
            (set-Button-background (ViewGame-previous game) BACKGROUND_COLOR)
            (set-Button-background (ViewGame-next game) BACKGROUND_COLOR)
            (ViewGame-level game)
            (ViewGame-level_index game)
            (ViewGame-unlocked game)
            (ViewGame-change_to? game)
            (ViewGame-invisible? game)))


;---------------- TESTS -----------------;

; game_completed? : ViewGame -> Boolean
; Return #true if the game is completed.
(define (game_completed? game)
  (level_completed? (ViewGame-level game)))


;---------------- PROCESS INPUT -----------------;

; process_game_previous : ViewGame Number Number String -> ViewGame
; Processes the previous button. If button is clicked, return the board
; of the previous level.
(define (process_game_previous game x y mouse)
  (cond [(string=? mouse "button-down")
         (set-ViewGame-level_index game
                                   (if (>= (- (ViewGame-level_index game) 1) 0)
                                       (- (ViewGame-level_index game) 1)
                                       (ViewGame-level_index game)))]
        [else
         (set-ViewGame-previous game
                                (mouse_on_button (ViewGame-previous game)
                                                 x y))]))

; process_game_next :  ViewGame Number Number String -> ViewGame
; Processes the next button. If button is clicked, return the board
; of the next level.
(define (process_game_next game x y mouse)
  (cond [(string=? mouse "button-down")
         (set-ViewGame-level_index game
                                   (if (< (+ (ViewGame-level_index game) 1) 30)
                                       (+ (ViewGame-level_index game) 1)
                                       (ViewGame-level_index game)))]
        [else
         (set-ViewGame-next game
                                (mouse_on_button (ViewGame-next game)
                                                 x y))]))

; process_game_levels :  ViewGame Number Number String -> ViewGame
; Processes the levels button. If button is clicked, return ViewLevels.
(define (process_game_levels game x y mouse)
  (cond [(string=? mouse "button-down")
         (set-ViewGame-change_to? game #true)]
        [else
         (set-ViewGame-levels game
                                (mouse_on_button (ViewGame-levels game)
                                                 x y))]))

; process_game_reset :  ViewGame Number Number String -> ViewGame
; Processes the reset button. If button is clicked, return the board
; without all the lines drawn before.
(define (process_game_reset game x y mouse)
  (cond [(string=? mouse "button-down")
         (let ([board (Level-board (ViewGame-level game))])
           (set-ViewGame-level game
                               (reset_level
                                (ViewGame-level game))))]
        [else
         (set-ViewGame-reset game
                                (mouse_on_button (ViewGame-reset game)
                                                 x y))]))

; process_game_invisible :  ViewGame Number Number String -> ViewGame
; Processes the invisible button. If button is clicked, change the mode
; of the game (invisible lines).
(define (process_game_invisible game x y mouse)
  (cond [(string=? mouse "button-down")
         (ViewGame (ViewGame-levels game)
                   (ViewGame-invisible game)
                   (ViewGame-reset game)
                   (ViewGame-previous game)
                   (ViewGame-next game)
                   (ViewGame-level game)
                   (ViewGame-level_index game)
                   (ViewGame-unlocked game)
                   (ViewGame-change_to? game)
                   #true)]
        [else
         (set-ViewGame-invisible game
                                (mouse_on_button (ViewGame-invisible game)
                                                 x y))]))

; process_game_not_invisible :  ViewGame Number Number String -> ViewGame
; Processes the (not)invisible button. If button is clicked, return the
; original mode of the game (visible lines).
(define (process_game_not_invisible game x y mouse)
  (cond [(string=? mouse "button-down")
         (ViewGame (ViewGame-levels game)
                   (ViewGame-invisible game)
                   (ViewGame-reset game)
                   (ViewGame-previous game)
                   (ViewGame-next game)
                   (ViewGame-level game)
                   (ViewGame-level_index game)
                   (ViewGame-unlocked game)
                   (ViewGame-change_to? game)
                   #false)]
        [else
         (set-ViewGame-invisible game
                                (mouse_on_button (ViewGame-invisible game)
                                                 x y))]))

; process_game :  ViewGame Number Number String -> ViewGame
; Evaluates on which button the mouse is.
(define (process_game game x y mouse)
  (cond [(is_on_button? (ViewGame-previous game) x y)
         (process_game_previous game x y mouse)]
        [(is_on_button? (ViewGame-next game) x y)
         (process_game_next game x y mouse)]
        [(is_on_button? (ViewGame-levels game) x y)
         (process_game_levels game x y mouse)]
        [(is_on_button? (ViewGame-reset game) x y)
         (process_game_reset game x y mouse)]
        [(is_on_button? (ViewGame-invisible game) x y)
         (if (ViewGame-invisible? game)
             (process_game_not_invisible game x y mouse)
             (process_game_invisible game x y mouse))]
        [else (ViewGame-reset-colors
               (set-ViewGame-level game
                                   (process_level (ViewGame-level game)
                                                  (- x FIELD_X)
                                                  (- y FIELD_Y)
                                                  mouse)))]))


  
;--------------------- DRAW ---------------------;

; draw_game : ViewGame -> Image
; Draws an Image of a given ViewGame.
(define (draw_game game)
  (place-images (list (draw_level (ViewGame-level game)
                                  FIELD
                                  (ViewGame-invisible? game))
                      (draw_button (ViewGame-levels game))
                      (draw_button (ViewGame-invisible game))
                      (draw_button (ViewGame-reset game))
                      (draw_arrow (ViewGame-previous game))
                      (draw_arrow (ViewGame-next game))
                      (text (string-append "Level "
                                           (number->string
                                            (real->int
                                             (add1
                                              (ViewGame-level_index game)))))
                            30 "DeepPink"))
                (list (make-posn (+ FIELD_X (/ FIELD_WIDTH 2))
                                 (+ FIELD_Y (/ FIELD_HEIGHT 2)))
                      (make-posn (+ GAME_LEVELS_X (/ GAME_LEVELS_W 2))
                                 (+ GAME_LEVELS_Y (/ GAME_LEVELS_H 2)))
                      (make-posn (+ GAME_INVISIBLE_X (/ GAME_INVISIBLE_W 2))
                                 (+ GAME_INVISIBLE_Y (/ GAME_INVISIBLE_H 2)))
                      (make-posn (+ GAME_RESET_X (/ GAME_RESET_W 2))
                                 (+ GAME_RESET_Y (/ GAME_RESET_H 2)))
                      (make-posn (+ GAME_PREVIOUS_X (/ GAME_PREVIOUS_W 2))
                                 (+ GAME_PREVIOUS_Y (/ GAME_PREVIOUS_H 2)))
                      (make-posn (+ GAME_NEXT_X (/ GAME_NEXT_W 2))
                                 (+ GAME_NEXT_Y (/ GAME_NEXT_H 2)))
                      (make-posn CENTER 65))
                SCREEN))

;----- PROVIDE -----;
(provide (all-defined-out))