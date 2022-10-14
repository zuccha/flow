#lang racket

(require "button.rkt")
(require "cell.rkt")
(require "color.rkt")
(require "io.rkt")
(require "level.rkt")
(require "view_game.rkt")
(require "view_levels.rkt")
(require "view_menu.rkt")
(require "utilities.rkt")

(require 2htdp/image)
(require 2htdp/universe)

; A View is one of:
; - ViewMenu
; - ViewGame
; - ViewLevels
; It represents the current menu on which the player is.

; Struct State : List<Level> Number View List<Boolean>
; A state with a list of playable levels, the index of the current level, a view
; and a list of unlocked levels.
(struct State (levels level_index view unlocked))

; initialize : List<Level> -> State
; Makes a State from a list of levels. The list of unlocked levels is made from
; a save file if it exists, or from the default values of the levels otherwise.
(define (initialize levels)
  (let ([unlocked (if (file-exists? "save.txt")
                      (read_unlocked "save.txt")
                      (make_unlocked levels))])
    (State levels 0 (make-ViewLevels unlocked) unlocked)))


;------------------ METHODS ---------------------;

; set-State-level_index : State Number -> State
; Sets the level index of a given state.
(define (set-State-level_index state level_index)
  (State (State-levels state)
         level_index
         (State-view state)
         (State-unlocked state)))

; set-State-view : State List<Level> -> State
; Sets the view of a given state.
(define (set-State-view state view)
  (State (State-levels state)
         (State-level_index state)
         view
         (State-unlocked state)))


;-------------- PROCESS INPUT -------------------;                  

; process_input : State Number Number String -> State
; Evaluates on which view a given state is, and process the input.
(define (process_input state x y mouse)
  (cond [(ViewMenu? (State-view state)) 
         (set-State-view state (process_menu (State-view state) x y mouse))]
        [(ViewLevels? (State-view state))
         (set-State-view state (process_levels (State-view state) x y mouse))]
        [(ViewGame? (State-view state))
         (let ([new_game (process_game (State-view state) x y mouse)])
           (State (State-levels state)
                  (State-level_index state)
                  new_game
                  (ViewGame-unlocked new_game)))]))


;-------------------- UPDATE --------------------;

; update_game : State -> State
; Updates the state if the view is ViewGame. If the game is completed it
; switches to ViewMenu, if the arrow buttons have been clicked it switches
; levels, if  the levels button is clicked it moves to ViewLevels.
(define (update_game state)
  (let ([level_index (ViewGame-level_index (State-view state))])
      (if (game_completed? (State-view state))
          (set-State-view
                  (set-State-level_index state (+ level_index 1))
                  (make-ViewMenu (ViewGame-unlocked (State-view state))
                                 (+ level_index 1)))
          (cond [(not (= (State-level_index state) level_index))
                 (set-State-view
                  (set-State-level_index state level_index)
                  (make-ViewGame LEVELS
                                 (ViewGame-unlocked (State-view state))
                                 level_index))]
                [(ViewGame-change_to? (State-view state))
                 (set-State-view state (make-ViewLevels
                                        (ViewGame-unlocked
                                         (State-view state))))]
                [else state]))))

; update_levels : State -> State
; Updates a give state if it's on ViewLevels. If a level has been choosen, then
; it switches to a ViewGame with said level.
(define (update_levels state)
  (let ([level_index (ViewLevels-level_index (State-view state))])
    (cond [(number? level_index)
           (set-State-view
            (set-State-level_index state level_index)
            (make-ViewGame LEVELS
                           (ViewLevels-unlocked (State-view state))
                           level_index))]
          [else state])))

; update_menu : State -> State
; Updates a given state if it's on ViewMenu. If continue button has been clicked
; it switches to a ViewGame withe the next level, if the levels button has been
; clicked, it switches to the ViewLevels.
(define (update_menu state)
  (let ([level_index (ViewMenu-level_index (State-view state))])
    (cond [(= (ViewMenu-go_to (State-view state)) VIEW_GAME)
           (set-State-view
            (set-State-level_index state level_index)
            (make-ViewGame LEVELS
                           (ViewMenu-unlocked (State-view state))
                           level_index))]
          [(= (ViewMenu-go_to (State-view state)) VIEW_LEVELS)
           (set-State-view (set-State-level_index state level_index)
                           (make-ViewLevels
                            (ViewMenu-unlocked
                             (State-view state))))]
          [else state])))

; update : State -> State
; Updates a given state base on its current view.
(define (update state)
  (cond [(ViewGame? (State-view state))
         (update_game state)]
        [(ViewLevels? (State-view state))
         (update_levels state)]
        [(ViewMenu? (State-view state))
         (update_menu state)]
        [else state]))


;--------------------- DRAW ---------------------;               

; draw : State -> Image
; Draws a given state, based on the view it is on.
(define (draw state)
  (cond [(ViewMenu? (State-view state))
         (draw_menu (State-view state))]
        [(ViewLevels? (State-view state))
         (draw_levels (State-view state))]
        [(ViewGame? (State-view state))
         (draw_game (State-view state))]
        [else state]))


;------------------- BIG BANG -------------------;

(big-bang (initialize LEVELS)        
          [on-mouse process_input]
          [on-tick update]
          [on-draw draw])