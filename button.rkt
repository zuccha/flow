#lang racket

(require "utilities.rkt")
(require 2htdp/image)
(require lang/posn)

; Struct Button : Number Number Number Number String Color Color Color Boolean 
(struct Button (x y w h text color1 color2 background active?))
; Example:
(define EMPTY_BUTTON (Button 0 0 0 0 "" "" "" "" #false))

; make_menu_button: String Number Boolean -> Button
; Return a Menu_Button
(define (make_menu_button text y active?)
  (Button MENU_B_X
          (* y MENU_B_Y)
          MENU_BUTTON_W
          MENU_BUTTON_H
          text
          TEXT_COLOR
          "gray"
          BACKGROUND_COLOR
          active?))

; make_game_button: Number Number Number Number Stirng Boolean -> Button
; Return a Game_Button
(define (make_game_button x y w h text active?)
  (Button x
          y
          w
          h
          text
          TEXT_COLOR
          "gray"
          BACKGROUND_COLOR
          active?))

; make_level_button: Number Number Number Number Stirng Boolean -> Button
; Return a Level_Button
(define (make_level_button x y w h text active?)
  (Button x
          y
          w
          h
          text
          TEXT_COLOR
          "gray"
          BACKGROUND_COLOR
          active?))

; make_button_XxX : Boolean Number Number -> Button
; Return a XxX_Button
(define (make_button_XxX active? index i)
  (Button (modulo i 5)
          (quotient i 5)
          LEVELS_BUTTON
          LEVELS_BUTTON
          (if (< (+ i index) 9)
              (string-append "0" (number->string (+ 1 i)))
              (number->string (+ 1 i)))
          TEXT_COLOR
          "gray"
          BACKGROUND_COLOR
          active?))

; generate_XxX : List<Boolean> Number Number --> List<Button>
; Return a list of buttons based on a list of booleans 
(define (generate_XxX unlocked index i)
  (cond [(empty? unlocked) '()]
        [(<= index i (+ index 9))
         (cons (make_button_XxX (first unlocked) index i)
               (generate_XxX (rest unlocked) index (add1 i)))]
        [else (generate_XxX (rest unlocked) index (add1 i))]))


; set-Button-background : Button Color -> Button
; Change the background of the button
(define (set-Button-background button background)
  (Button (Button-x button)
          (Button-y button)
          (Button-w button)
          (Button-h button)
          (Button-text button)
          (Button-color1 button)
          (Button-color2 button)
          background
          (Button-active? button)))

; set-list-buttons-background : List<Button> Color -> List<Button>
; Change the background of a list of buttons
(define (set-list-buttons-background buttons background)
  (cond [(empty? buttons) '()]
        [else (cons (set-Button-background (first buttons) background)
                    (set-list-buttons-background (rest buttons) background))]))
  

;--------------------- DRAW ---------------------;

; draw_arrow : Button -> Image
; Draws a particular button for arrows (different font size).
(define (draw_arrow button)
  (place-image (text (Button-text button)
                     ARROW_SIZE
                     (if (Button-active? button)
                         (Button-color1 button)
                         (Button-color2 button)))
               (/ (Button-w button) 2)
               (/ (Button-h button) 3)
               (frame (rectangle (Button-w button)
                                 (Button-h button)
                                 "solid"
                                 (Button-background button)))))

; draw_button : Button -> Image
; Draws a button.
(define (draw_button button)
  (place-image (text (Button-text button)
                     TEXT_SIZE
                     (if (Button-active? button)
                         (Button-color1 button)
                         (Button-color2 button)))
               (/ (Button-w button) 2)
               (/ (Button-h button) 2)
               (frame (rectangle (Button-w button)
                                 (Button-h button)
                                 "solid"
                                 (Button-background button)))))

; draw_buttons : List<Image> -> Image
; Draws a list of buttons.
(define (draw_buttons buttons)
      (draw_buttons_it buttons 0 LEVELS_W LEVELS_H LEVELS_BUTTON LEVELS_ROW))

; draw_buttons_it : List<Button> -> List<Image>
; Makes a list of images from a list of buttons.
(define (draw_buttons_it buttons i w h s r)
  (cond [(empty? buttons) (empty-scene w h)]
        [else (place-image (draw_button (first buttons))
                           (+ (* s (modulo i r)) (/ s 2))
                           (+ (* s (quotient i r)) (/ s 2))
                           (draw_buttons_it (rest buttons) (+ i 1) w h s r))]))

; is_on_button? : Button Number Number -> Boolean
; Return true if the mouse is on a button.
(define (is_on_button? button x y)
  (cond [(and (Button-active? button)
              (>= x (Button-x button)) (<= x (+ (Button-x button)
                                                (Button-w button)))
              (>= y (Button-y button)) (<= y (+ (Button-y button)
                                                (Button-h button))))
         #true]
         [else #false]))

; is_on_list_buttons : List<Button> Number Number -> Boolean
; Return true if the mouse is on a button of the list.
(define (is_on_list_buttons buttons x y)
  (cond [(empty? buttons) #false]
        [(or (is_on_button? (first buttons) x y)
             (is_on_list_buttons (rest buttons) x y))]))

; mouse_on_button : Button Number Number -> Button
; If the mouse is on a button, it colors the button.
(define (mouse_on_button button x y)
  (cond [(and (>= x (Button-x button)) (<= x (+ (Button-x button)
                                                (Button-w button)))
              (>= y (Button-y button)) (<= y (+ (Button-y button)
                                                (Button-h button))))
         (Button (Button-x button)
                 (Button-y button)
                 (Button-w button)
                 (Button-h button)
                 (Button-text button)
                 TEXT_COLOR
                 "gray"
                 BACKGROUND_SELECTED
                 (Button-active? button))]
         [else button]))

; mouse_on_button : List<Button> Number Number -> List<Button> 
; If the mouse is on a button of the list, it colors the button.
(define (mouse_on_buttons buttons x y)
  (cond [(empty? buttons) '()]
        [(mouse_on_button (first buttons) x y)
         (mouse_on_buttons (rest buttons) x y)]))


;----- PROVIDE -----;

(provide (all-defined-out))