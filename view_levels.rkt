#lang racket

(require "button.rkt")
(require "io.rkt")
(require "level.rkt")
(require "utilities.rkt")
(require 2htdp/image)
(require lang/posn)

; Struct ViewLevels : Button Button List<Button> List<Button> List<Button>
;                     Number List<Boolean>
(struct ViewLevels [save reset levels6 levels7 levels8 level_index unlocked])

; make-ViewLevels : List<Boolean> -> ViewLevels
; Returns a ViewLevels based on a list of unlocked levels.
(define (make-ViewLevels unlocked)
  (ViewLevels (make_game_button LEVELS_SAVE_X LEVELS_SAVE_Y
                                LEVELS_SAVE_W LEVELS_SAVE_H
                              "Save" #true)
              (make_game_button LEVELS_RESET_X LEVELS_RESET_Y
                                LEVELS_RESET_W LEVELS_RESET_H
                              "Reset" #true)
              (generate_XxX unlocked 0 0)
              (generate_XxX unlocked 10 0)
              (generate_XxX unlocked 20 0)
              #false
              unlocked))


;---------------- METHODS -----------------;

; set-ViewLevels-save : ViewLevels Button -> ViewLevels
; Sets the save button of a given ViewLevels.
(define (set-ViewLevels-save levels save)
  (ViewLevels save
              (ViewLevels-reset levels)
              (ViewLevels-levels6 levels)
              (ViewLevels-levels7 levels)
              (ViewLevels-levels8 levels)
              (ViewLevels-level_index levels)
              (ViewLevels-unlocked levels)))

; set-ViewLevels-reset : ViewLevels Button -> ViewLevels
; Sets the reset button of a given ViewLevels.
(define (set-ViewLevels-reset levels reset)
  (ViewLevels (ViewLevels-save levels)
              reset
              (ViewLevels-levels6 levels)
              (ViewLevels-levels7 levels)
              (ViewLevels-levels8 levels)
              (ViewLevels-level_index levels)
              (ViewLevels-unlocked levels)))

; set-ViewLevels-levels6 : ViewLevels List<Button> -> ViewLevels
; Sets the list of buttons 6x6 of a given ViewLevels.
(define (set-ViewLevels-levels6 levels levels6)
  (ViewLevels (ViewLevels-save levels)
              (ViewLevels-reset levels)
              levels6
              (ViewLevels-levels7 levels)
              (ViewLevels-levels8 levels)
              (ViewLevels-level_index levels)
              (ViewLevels-unlocked levels)))

; set-ViewLevels-levels7 : ViewLevels List<Button> -> ViewLevels
; Sets the list of buttons 7x7 of a given ViewLevels.
(define (set-ViewLevels-levels7 levels levels7)
  (ViewLevels (ViewLevels-save levels)
              (ViewLevels-reset levels)
              (ViewLevels-levels6 levels)
              levels7
              (ViewLevels-levels8 levels)
              (ViewLevels-level_index levels)
              (ViewLevels-unlocked levels)))

; set-ViewLevels-levels8 : ViewLevels List<Button> -> ViewLevels
; Sets the list of buttons 8x8 of a given ViewLevels.
(define (set-ViewLevels-levels8 levels levels8)
  (ViewLevels (ViewLevels-save levels)
              (ViewLevels-reset levels)
              (ViewLevels-levels6 levels)
              (ViewLevels-levels7 levels)
              levels8
              (ViewLevels-level_index levels)
              (ViewLevels-unlocked levels)))

; set-ViewLevels-level_index : ViewLevels Number -> ViewLevels
; Sets the level_index of a given ViewLevels.
(define (set-ViewLevels-level_index levels index)
  (ViewLevels (ViewLevels-save levels)
              (ViewLevels-reset levels)
              (ViewLevels-levels6 levels)
              (ViewLevels-levels7 levels)
              (ViewLevels-levels8 levels)
              index
              (ViewLevels-unlocked levels)))

; ViewLevels-reset-colors : ViewLevels -> ViewLevels
; Reset the background color of all buttons of a given ViewLevels.
(define (ViewLevels-reset-colors levels)
  (ViewLevels (set-Button-background (ViewLevels-save levels)
                                     BACKGROUND_COLOR)
              (set-Button-background (ViewLevels-reset levels)
                                     BACKGROUND_COLOR)
              (set-list-buttons-background (ViewLevels-levels6 levels)
                                           BACKGROUND_COLOR)
              (set-list-buttons-background (ViewLevels-levels7 levels)
                                           BACKGROUND_COLOR)
              (set-list-buttons-background (ViewLevels-levels8 levels)
                                           BACKGROUND_COLOR)
              (ViewLevels-level_index levels)
              (ViewLevels-unlocked levels)))


;---------------- PROCESS INPUT -----------------;

; process_levels_XxX : ViewLevels Number String Number
;                      (ViewLevels -> List<Button>) (ViewLevels -> ViewLevels)
;                      -> ViewLevels
; Evaluates on which button XxX (6x6, 7x7 or 8x8) the mouse is on, and changes
; its background color or computes the mouse click.
(define (process_levels_XxX levels index mouse n levelsX set-levelsX)
  (let ([button (get (levelsX levels) index)])
    (if (and (< -1 index (length (levelsX levels))) (Button-active? button))
        (cond [(string=? mouse "button-down") 
               (set-ViewLevels-level_index levels (+ index n))]
              [else
               (set-levelsX
                levels
                (set (set-list-buttons-background (levelsX levels)
                                                  BACKGROUND_COLOR)
                     index
                     (set-Button-background
                      button BACKGROUND_SELECTED)))])
        (ViewLevels-reset-colors levels))))

; process_levels_save : ViewLevels Number Number String -> ViewLevels
; Processes the save button. If button is clicked, the list of unlocked levels
; is saved on "save.txt" file.
(define (process_levels_save levels x y mouse)
  (if (string=? mouse "button-down")
      (let ([s (save "save.txt" (ViewLevels-unlocked levels))])
        (set-ViewLevels-save levels
                             (mouse_on_button (ViewLevels-save levels) x y)))
      (set-ViewLevels-save levels
                           (mouse_on_button (ViewLevels-save levels) x y))))

; process_levels_reset : ViewLevels Number Number String -> ViewLevels
; Processes the reset button. If button is clicked, the list of unlocked levels
; is reset.
(define (process_levels_reset levels x y mouse)
  (if (string=? mouse "button-down")
      (let ([r (reset "save.txt")])
        (set-ViewLevels-reset (make-ViewLevels (read_unlocked "save.txt"))
                              (mouse_on_button (ViewLevels-reset levels) x y)))
      (set-ViewLevels-reset levels
                              (mouse_on_button (ViewLevels-reset levels) x y))))

; process_levels : ViewLevels Number Number String -> ViewLevels
; Evaluates on which button the mouse is.
(define (process_levels levels x y mouse)
  (local [(define (in_area? l w t h) (and (< l x (+ l w)) (< t y (+ t h))))
          (define (conv X Y) (convert (floor (/ (- x X) LEVELS_BUTTON))
                                      (floor (/ (- y Y) LEVELS_BUTTON))
                                      LEVELS_ROW))]
    (cond [(in_area? LEVELS_6x6_X LEVELS_W LEVELS_6x6_Y LEVELS_H)
           (process_levels_XxX levels (conv LEVELS_6x6_X LEVELS_6x6_Y) mouse 0
                               ViewLevels-levels6 set-ViewLevels-levels6)]
          [(in_area? LEVELS_7x7_X LEVELS_W LEVELS_7x7_Y LEVELS_H)
           (process_levels_XxX levels (conv LEVELS_7x7_X LEVELS_7x7_Y) mouse 10
                               ViewLevels-levels7 set-ViewLevels-levels7)]
          [(in_area? LEVELS_8x8_X LEVELS_W LEVELS_8x8_Y LEVELS_H)
           (process_levels_XxX levels (conv LEVELS_8x8_X LEVELS_8x8_Y) mouse 20
                               ViewLevels-levels8 set-ViewLevels-levels8)]
          [(is_on_button? (ViewLevels-save levels) x y)
           (process_levels_save levels x y mouse)]
          [(is_on_button? (ViewLevels-reset levels) x y)
           (process_levels_reset levels x y mouse)]
          [else (ViewLevels-reset-colors levels)])))


;--------------------- DRAW ---------------------;

; draw_levels : ViewLevels -> Image
; Draws an Image of a given ViewLevels.
(define (draw_levels levels)
  (place-images (list (circle 25 "solid" "Aqua")
                      (text/font "FLOW" (+ 69 20) BACKGROUND_SELECTED
                                 "Helvetica Neue" 'default 'normal 'normal #f)
                      (circle 25 "solid" "Aqua")
                      (rectangle 350 20 "solid" "Aqua")
                      (text/font "Levels 6x6" TEXT_SIZE "aqua" #f 'default
                                 'normal 'normal #f)
                      (draw_buttons (ViewLevels-levels6 levels))
                      (text/font "Levels 7x7" TEXT_SIZE "aqua" #f 'default
                                 'normal 'normal #f)
                      (draw_buttons (ViewLevels-levels7 levels))
                      (text/font "Levels 8x8" TEXT_SIZE "aqua" #f 'default
                                 'normal 'normal #f)
                      (draw_buttons (ViewLevels-levels8 levels))
                      (draw_button (ViewLevels-save levels))
                      (draw_button (ViewLevels-reset levels)))
                (list (make-posn (- CENTER 180) LEVELS_TITLE_Y)
                      (make-posn CENTER LEVELS_TITLE_Y)
                      (make-posn (+ CENTER 180) LEVELS_TITLE_Y)
                      (make-posn CENTER LEVELS_TITLE_Y)
                      (make-posn CENTER LEVELS_6x6_TITLE_Y)
                      (make-posn CENTER (+ LEVELS_6x6_Y (/ LEVELS_H 2)))
                      (make-posn CENTER LEVELS_7x7_TITLE_Y)
                      (make-posn CENTER (+ LEVELS_7x7_Y (/ LEVELS_H 2)))
                      (make-posn CENTER LEVELS_8x8_TITLE_Y)
                      (make-posn CENTER (+ LEVELS_8x8_Y (/ LEVELS_H 2)))
                      (make-posn (+ LEVELS_SAVE_X (/ LEVELS_SAVE_W 2))
                                 (+ LEVELS_SAVE_Y (/ LEVELS_SAVE_H 2)))
                      (make-posn (+ LEVELS_RESET_X (/ LEVELS_RESET_W 2))
                                 (+ LEVELS_RESET_Y (/ LEVELS_RESET_H 2))))
                SCREEN))


;----- PROVIDE -----;
(provide (all-defined-out))