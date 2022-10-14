#lang racket

(require 2htdp/image)


;----- FUNCTIONS -----;

; get : List<A> Number -> A
; Returns the nth element of a given list.
(define (get lst n)
  (cond [(empty? lst) (string-append "ERROR: get " (number->string n))]
        [(= n 0) (first lst)]
        [else (get (rest lst) (- n 1))]))

; set : List<A> Number A -> List<A>
; Replaces the nth element of a list lst with a given element x. 
(define (set lst n x)
  (cond [(empty? lst) '()]
        [(= n 0) (cons x (rest lst))]
        [else (cons (first lst) (set (rest lst) (- n 1) x))]))

; convert : Number Number Number -> Number
; Coverts (x, y) coordinates in list index.
(define (convert x y w)
  (+ x (* y w)))


;----- CONSTANTS -----;

; Base list of unlocked levels
(define BASE_UNLOCKED (list #t #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f
                            #f #f #f #f #f #f #f #f #f #f))

; View indexes
(define VIEW_NONE 0)
(define VIEW_LEVELS 1)
(define VIEW_GAME 2)
(define VIEW_MENU 3)

; Font and background colors and size
(define TEXT_COLOR "white")
(define BACKGROUND_COLOR "black")
(define BACKGROUND_SELECTED "DeepPink")
(define TEXT_SIZE 40)
(define ARROW_SIZE 80)

; Screen sizes and positions
(define SCREEN_WIDTH 630);672)
(define SCREEN_HEIGHT 830);872)
(define SCREEN (rectangle SCREEN_WIDTH SCREEN_HEIGHT "solid" BACKGROUND_COLOR))
(define CENTER (/ SCREEN_WIDTH 2))

; ViewMenu sizes and positions
(define MENU_BUTTON_W (quotient SCREEN_WIDTH 2))
(define MENU_BUTTON_H (quotient SCREEN_HEIGHT 13))
(define MENU_B_X (quotient (- SCREEN_WIDTH MENU_BUTTON_W) 2))
(define MENU_B_Y MENU_BUTTON_H)
(define MENU_B_TEXT_SIZE (ceiling (quotient (* SCREEN_WIDTH SCREEN_HEIGHT)
                                            10000)))

; ViewGame sizes and positions
(define FIELD_WIDTH (quotient (* SCREEN_WIDTH 3) 4))
(define FIELD_X (quotient (- SCREEN_WIDTH FIELD_WIDTH) 2))

(define GAME_LEVELS_X FIELD_X)
(define GAME_LEVELS_Y (quotient (* SCREEN_HEIGHT 1) 8))
(define GAME_LEVELS_W (* (quotient SCREEN_WIDTH 3) 1))
(define GAME_LEVELS_H (quotient SCREEN_HEIGHT 13))

(define GAME_INVISIBLE_X (- (+ FIELD_X FIELD_WIDTH) GAME_LEVELS_W))
(define GAME_INVISIBLE_Y (quotient (* SCREEN_HEIGHT 1) 8))
(define GAME_INVISIBLE_W (* (quotient SCREEN_WIDTH 3) 1))
(define GAME_INVISIBLE_H (quotient SCREEN_HEIGHT 13))

(define FIELD_HEIGHT FIELD_WIDTH)
(define FIELD_Y (+ GAME_LEVELS_Y GAME_LEVELS_H 40))

(define FIELD (color-frame TEXT_COLOR (rectangle FIELD_WIDTH FIELD_HEIGHT
                                                 "solid" BACKGROUND_COLOR)))

(define GAME_RESET_W (quotient SCREEN_WIDTH 4))
(define GAME_RESET_H (quotient SCREEN_HEIGHT 13))
(define GAME_RESET_X (- (quotient SCREEN_WIDTH 2) (quotient GAME_RESET_W 2)))
(define GAME_RESET_Y (+ FIELD_Y FIELD_HEIGHT 40))

(define GAME_PREVIOUS_X FIELD_X)
(define GAME_PREVIOUS_Y (+ FIELD_Y FIELD_HEIGHT 40))
(define GAME_PREVIOUS_W (* (quotient SCREEN_WIDTH 5) 1))
(define GAME_PREVIOUS_H (quotient SCREEN_HEIGHT 13))

(define GAME_NEXT_X (- (+ FIELD_X FIELD_WIDTH) GAME_PREVIOUS_W))
(define GAME_NEXT_Y (+ FIELD_Y FIELD_HEIGHT 40))
(define GAME_NEXT_W (* (quotient SCREEN_WIDTH 5) 1))
(define GAME_NEXT_H (quotient SCREEN_HEIGHT 13))

; ViewLevels sizes and position
(define LEVEL_SIZE 5) ;Number of buttons in a row

(define LEVELS_TITLE_Y (/ SCREEN_HEIGHT 10))

(define LEVELS_W (* (/ FIELD_WIDTH 3) 2))
(define LEVELS_ROW 5)
(define LEVELS_BUTTON (/ LEVELS_W LEVELS_ROW))
(define LEVELS_H (* LEVELS_BUTTON 2))

(define LEVELS_6x6_TITLE_Y (/ SCREEN_HEIGHT 4.5))
(define LEVELS_6x6_X (/ (- SCREEN_WIDTH LEVELS_W) 2))
(define LEVELS_6x6_Y (+ LEVELS_6x6_TITLE_Y 30))
(define LEVELS_7x7_TITLE_Y (/ SCREEN_HEIGHT 2.2))
(define LEVELS_7x7_X LEVELS_6x6_X)
(define LEVELS_7x7_Y (+ LEVELS_7x7_TITLE_Y 30))
(define LEVELS_8x8_TITLE_Y (+ LEVELS_7x7_TITLE_Y LEVELS_6x6_TITLE_Y 10))
(define LEVELS_8x8_X LEVELS_6x6_X)
(define LEVELS_8x8_Y (+ LEVELS_8x8_TITLE_Y 30))

(define LEVELS_SAVE_X LEVELS_6x6_X)
(define LEVELS_SAVE_Y (+ LEVELS_8x8_Y LEVELS_H 30))
(define LEVELS_SAVE_W GAME_PREVIOUS_W)
(define LEVELS_SAVE_H GAME_PREVIOUS_H)

(define LEVELS_RESET_W GAME_NEXT_W)
(define LEVELS_RESET_H GAME_NEXT_H)
(define LEVELS_RESET_X (+ LEVELS_6x6_X (- LEVELS_W LEVELS_RESET_W)))
(define LEVELS_RESET_Y (+ LEVELS_8x8_Y LEVELS_H 30))


;----- PROVIDE -----;

(provide (all-defined-out))