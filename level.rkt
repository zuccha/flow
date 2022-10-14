#lang racket

(require "board.rkt")
(require "boards_list.rkt")
(require "cell.rkt")
(require "utilities.rkt")
(require 2htdp/image)

; Struct Level : Board Number Number Boolean
(struct Level (board w h current_moves record_moves locked?))
; Example: (Define 30 Levels)
(define LEVEL_1 (Level BOARD_1 6 6 0 0 #true))
(define LEVEL_2 (Level BOARD_2 6 6 0 0 #false))
(define LEVEL_3 (Level BOARD_3 6 6 0 0 #false))
(define LEVEL_4 (Level BOARD_4 6 6 0 0 #false))
(define LEVEL_5 (Level BOARD_5 6 6 0 0 #false))
(define LEVEL_6 (Level BOARD_6 6 6 0 0 #false))
(define LEVEL_7 (Level BOARD_7 6 6 0 0 #false))
(define LEVEL_8 (Level BOARD_8 6 6 0 0 #false))
(define LEVEL_9 (Level BOARD_9 6 6 0 0 #false))
(define LEVEL_10 (Level BOARD_10 6 6 0 0 #false))
(define LEVEL_11 (Level BOARD_11 7 7 0 0 #false))
(define LEVEL_12 (Level BOARD_12 7 7 0 0 #false))
(define LEVEL_13 (Level BOARD_13 7 7 0 0 #false))
(define LEVEL_14 (Level BOARD_14 7 7 0 0 #false))
(define LEVEL_15 (Level BOARD_15 7 7 0 0 #false))
(define LEVEL_16 (Level BOARD_16 7 7 0 0 #false))
(define LEVEL_17 (Level BOARD_17 7 7 0 0 #false))
(define LEVEL_18 (Level BOARD_18 7 7 0 0 #false))
(define LEVEL_19 (Level BOARD_19 7 7 0 0 #false))
(define LEVEL_20 (Level BOARD_20 7 7 0 0 #false))
(define LEVEL_21 (Level BOARD_21 8 8 0 0 #false))
(define LEVEL_22 (Level BOARD_22 8 8 0 0 #false))
(define LEVEL_23 (Level BOARD_23 8 8 0 0 #false))
(define LEVEL_24 (Level BOARD_24 8 8 0 0 #false))
(define LEVEL_25 (Level BOARD_25 8 8 0 0 #false))
(define LEVEL_26 (Level BOARD_26 8 8 0 0 #false))
(define LEVEL_27 (Level BOARD_27 8 8 0 0 #false))
(define LEVEL_28 (Level BOARD_28 8 8 0 0 #false))
(define LEVEL_29 (Level BOARD_29 8 8 0 0 #false))
(define LEVEL_30 (Level BOARD_30 8 8 0 0 #false))

; List of all levels
(define LEVELS (list LEVEL_1 LEVEL_2 LEVEL_3 LEVEL_4 LEVEL_5 LEVEL_6 LEVEL_7
                     LEVEL_8 LEVEL_9 LEVEL_10 LEVEL_11 LEVEL_12 LEVEL_13
                     LEVEL_14 LEVEL_15 LEVEL_16 LEVEL_17 LEVEL_18 LEVEL_19
                     LEVEL_20 LEVEL_21 LEVEL_22 LEVEL_23 LEVEL_24 LEVEL_25
                     LEVEL_26 LEVEL_27 LEVEL_28 LEVEL_29 LEVEL_30))

; make_unlocked : List<levels> -> List<levels>
; Return a new list of unloacked levels
(define (make_unlocked levels)
  (cond [(empty? levels) '()]
        [else (cons (Level-locked? (first levels))
                    (make_unlocked (rest levels)))]))


;------------------ METHODS ---------------------;

; set-Level-board : Level Board -> Level
; Set the board of a level.
(define (set-Level-board level board)
  (Level board
         (Level-w level)
         (Level-h level)
         (Level-current_moves level)
         (Level-record_moves level)
         (Level-locked? level)))

; level_completed? : Level -> Boolean
; Returns #true if the board of a given level is completed.
(define (level_completed? level)
  (board_completed? (Level-board level)))

; reset_level : Level -> Level
; Return the level to the initial state.
(define (reset_level level)
  (set-Level-board level (reset_board (Level-board level))))


;-------------- PROCESS INPUT -------------------;

; process_level : Level Number Number String -> Level
; Processes a given level, based on the process of the board of the level.
(define (process_level level x y mouse)
  (set-Level-board level (process_board (Level-board level)
                                        x y
                                        (Level-w level)
                                        (Level-h level)
                                        mouse)))


;--------------------- DRAW ---------------------;

; draw_level : Level Image -> Image
; Draw the level means drawing the board of a corresponding level.
(define (draw_level level scene invisible?)
  (draw_board (Level-board level)
              scene
              (Level-w level)
              (cell_size (Level-w level))
              invisible?))


;----- PROVIDE -----;

(provide (all-defined-out))