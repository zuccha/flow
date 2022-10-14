#lang racket

(require "button.rkt")
(require "level.rkt")
(require "utilities.rkt")
(require "view_game.rkt")
(require 2htdp/image)
(require lang/posn)

; Struct ViewMenu : Button Button Number
; A view for when a player completes a level. It has a button continue (to go to
; the next level) and a button levels to go to the list of levels. "go_to"
; indicates if it is needed to switch to another view. It also has a list of
; unlocked levels and the current level index.
(struct ViewMenu [continue levels go_to unlocked level_index])

; make-ViewMenu : List<Boolean> Number -> ViewMenu
; Return a ViewMenu based on a list of unlocked levels.
(define (make-ViewMenu unlocked level_index)
  (ViewMenu (if (<= 0 level_index 29)
                (make_menu_button "Continue" 5 #true)
                (make_menu_button "Continue" 5 #false))
            (make_menu_button "Levels" 7 #true)
            VIEW_NONE
            (set unlocked level_index #true)
            level_index))
            
;---------------- METHODS -----------------;

; set-ViewMenu-continue : ViewMenu Button -> ViewMenu
; Sets the save button of a given ViewMenu.
(define (set-ViewMenu-continue menu continue)
  (ViewMenu continue
            (ViewMenu-levels menu)
            (ViewMenu-go_to menu)
            (ViewMenu-unlocked menu)
            (ViewMenu-level_index menu)))

; set-ViewMenu-levels : ViewMenu Button -> ViewMenu
; Sets the save button of a given ViewMenu.
(define (set-ViewMenu-levels menu levels)
  (ViewMenu (ViewMenu-continue menu)
            levels
            (ViewMenu-go_to menu)
            (ViewMenu-unlocked menu)
            (ViewMenu-level_index menu)))

; set-ViewMenu-go_to : ViewMenu Button -> ViewMenu
; Sets the save button of a given ViewMenu.
(define (set-ViewMenu-go_to menu go_to)
  (ViewMenu (ViewMenu-continue menu)
            (ViewMenu-levels menu)
            go_to
            (ViewMenu-unlocked menu)
            (ViewMenu-level_index menu)))

; set-ViewMenu-colors : ViewMenu -> ViewMenu
; Reset the background color of all buttons of a given ViewMenu.
(define (ViewMenu-reset-colors menu)
  (ViewMenu (set-Button-background (ViewMenu-continue menu) BACKGROUND_COLOR)
            (set-Button-background (ViewMenu-levels menu) BACKGROUND_COLOR)
            VIEW_NONE
            (ViewMenu-unlocked menu)
            (ViewMenu-level_index menu)))

;-------------- PROCESS INPUT -------------------;

; process_menu : ViewMenu Number Number String -> ViewMenu
; Evaluates on which button the mouse is.
(define (process_menu menu x y mouse)
 (cond [(is_on_button? (ViewMenu-continue menu) x y)
        (cond [(string=? mouse "button-down")
               (set-ViewMenu-go_to menu VIEW_GAME)]
              [else 
               (set-ViewMenu-continue menu
                                      (mouse_on_button (ViewMenu-continue menu)
                                                       x y))])]
       [(is_on_button? (ViewMenu-levels menu) x y)
        (cond [(string=? mouse "button-down")
               (set-ViewMenu-go_to menu VIEW_LEVELS)]
              [else 
               (set-ViewMenu-levels menu
                                    (mouse_on_button (ViewMenu-levels menu)
                                                          x y))])]
       [else  (ViewMenu-reset-colors menu)]))


;--------------------- DRAW ---------------------;

; draw_menu : ViewMenu -> Image
; Draws an Image of a given ViewMenu.
(define (draw_menu menu)
  (local [(define (pos button)
            (make-posn (+ (Button-x button) (/ (Button-w button) 2))
                       (+ (Button-y button) (/ (Button-h button) 2))))]
    (place-images (list (text/font "Winner!" (+ 69 20) BACKGROUND_SELECTED
                                   "Gill Sans Light" 'default 'normal 'normal #f)
                        (draw_button (ViewMenu-continue menu))
                        (draw_button (ViewMenu-levels menu)))
                  (list (make-posn  CENTER (+ LEVELS_TITLE_Y 40))
                        (pos (ViewMenu-continue menu))
                        (pos (ViewMenu-levels menu)))
                  SCREEN)))

;----- PROVIDE -----;
(provide (all-defined-out))