#lang racket

(require "cell.rkt")
(require "color.rkt")
(require "utilities.rkt")
(require 2htdp/image)

; Board : List<Cell> Cell
; A Board made of a list of cells, and a cell used to connect two cell. 
(struct Board (cells next_cell))

; make_board : List<Cell> Number Number Number Number List<Color> -> List<Cell>
; Makes a board given an height, a width and a list of color. When using this
; functions, "cells" should be '(), "x" and "y" should be 0.
(define (make_board cells w h x y colors)
  (cond [(< y h)
         (cond [(< x w) (make_board (cons (Cell x y -1 -1 N (first colors))
                                          cells)
                                    w h (+ x 1) y (rest colors))]
               [else (make_board cells w h 0 (+ y 1) colors)])]
        [else (Board (reverse cells) EMPTY_CELL)]))


;------------------ METHODS ---------------------;

; reset_board : Board -> Board
; For each cell int a given board, it sets nx and ny to -1, and the cell color
; to N.
(define (reset_board board)
  (Board (map (lambda (cell)
                (Cell (Cell-x cell) (Cell-y cell) -1 -1 N (Cell-dot cell)))
              (Board-cells board))
         EMPTY_CELL))
  
; reset_cells : List<Cell> Color -> List<Cell>
; For each cell int a given board that has a given color, it sets nx and ny to
; -1, and the cell color to N.
(define (reset_cells cells color)
  (map (lambda (cell)
         (if (is_color? cell color)
             (Cell (Cell-x cell) (Cell-y cell) -1 -1 N (Cell-dot cell))
             cell))
       cells))

; remove_colored_cell : List<Cell> Cell Number -> List<Cell>
; It removes color from a given cell in a given list of cells. "w" is the number
; of cells in a row of the board.
(define (remove_colored_cell cells cell w)
  (let ([index (+ (Cell-x cell) (* (Cell-y cell) w))])
    (let ([new_cell (get cells index)])
      (set cells index
           (Cell (Cell-x new_cell) (Cell-y new_cell)
                 -1 -1
                 N (Cell-dot new_cell))))))


;------------------ TESTS ---------------------;

; mouse_on_board? : Board Number Number -> Boolean
; Checks if the mouse,at (x,y), is on the board.
(define (mouse_on_board? x y)
  (if (and (and (<= 0 x) (< x FIELD_WIDTH))
           (and (<= 0 y) (< y FIELD_HEIGHT)))
      #true
      #false))

; cells_completed? : List<Cell> -> Boolean
;If all cells in a list of cells are colored it returns #true, #false otherwise.
(define (cells_completed? cells)
  (cond [(empty? cells) #true]
              [(not (has_color? (first cells))) #false]
        [else (cells_completed? (rest cells))]))

; board_completed? : Board -> Boolean
; If all cells in a board are colored it returns #true, #false otherwise.
(define (board_completed? board)
  (cells_completed? (Board-cells board)))

; cell_exists? : Number Number Number Number -> Boolean
; Returns #true if the position of a cell is in a valid range, #false otherwise.
(define (cell_exists? x y w h)
  (and (<= 0 x) (< x w)
       (<= 0 y) (< y h)))

; is_head? : Cell List<Cell> -> Boolean
; Returns #true if a given cell is the head of the line in a given list of
; cells, #false otherwise.
(define (is_head? cell cells)
  (cond [(empty? cells) #true]
        [(same_pos_next? cell (first cells)) #false]
        [else (is_head? cell (rest cells))]))

; near_to? : Cell Cell -> Boolean
; Given two cells, it check if the first is adjacent to the second. 
(define (near_to? cell next)
  (local [(define (near? cell next x y)
            (and (= (+ (Cell-x cell) x) (Cell-x next))
                 (= (+ (Cell-y cell) y) (Cell-y next))))]
    (or (near? cell next 1 0) (near? cell next -1 0)
        (near? cell next 0 1) (near? cell next 0 -1))))


;-------------- PROCESS INPUT -------------------;

; board_button_down : Board Number -> Board
; Processes the move on a given board when the mouse is clicked. "cell_index"
; is the index, in the list of cells of the board, of the cell on which the
; mouse is.
(define (board_button_down board cell_index)
  (let [(cell (get (Board-cells board) cell_index))]
    (cond [(has_dot? cell)
           (Board (set (reset_cells (Board-cells board) (Cell-dot cell))
                       cell_index
                       (set-Cell-color (set-Cell-next cell EMPTY_CELL)
                                       (Cell-dot cell)))
                  (set-Cell-color cell (Cell-dot cell)))]
          [(and (not (has_dot? cell)) (is_head? cell (Board-cells board)))
           (Board (Board-cells board)
                  cell)]
          [else board])))

; board_button_up : Board Number -> Board
; Processes the move on a given board when the mouse is released. "cell_index"
; is the index, in the list of cells of the board, of the cell on which the
; mouse is.
(define (board_button_up board cell_index)
  (Board (Board-cells board)
         EMPTY_CELL))
                  

; board_drag : Board Number Number Number -> Board
; Processed the move on a given board when the mouse is dragged. "w" and "h" are
; the width and height of the board in cells. "cell_index" is the index, in the
; list of cells of the board, of the cell on which the mouse is.
(define (board_drag board cell_index w h)
  (let [(cell (get (Board-cells board) cell_index))
        (next (Board-next_cell board))]
    (cond [(and (has_color? next)
                (not (same_pos? cell next))
                (near_to? cell next))
           (cond [(and (not (has_color? cell)) (not (has_dot? cell)))
                  (Board (set (Board-cells board)
                              cell_index
                              (set-Cell-next cell next))
                         (set-Cell-next cell next))]
                 [(and (not (has_color? cell)) (is_dot? cell (Cell-color next)))
                  (Board (set (Board-cells board)
                              cell_index
                              (set-Cell-next cell next))
                         EMPTY_CELL)]
                 [(and (has_color? cell)
                       (is_color? cell (Cell-color cell))
                       (same_pos_next? cell (Board-next_cell board)))
                  (Board (remove_colored_cell (Board-cells board) next w)
                         cell)]
                 [else (Board (Board-cells board) EMPTY_CELL)])]
          [else board])))

; process_board : Board Number String -> Board
; Processes a given board, based on his size (w, h), the position of the mouse
; (x, y) and the type of its event (mouse).
(define (process_board board x y w h mouse)
  (local [(define (convert/p x y w)
            (convert (quotient x (cell_size w))
                     (quotient y (cell_size w))
                     w))]
    (if (mouse_on_board? x y)
        (cond [(string=? mouse "button-down")
               (board_button_down board (convert/p x y w))]
              [(string=? mouse "button-up")
               (board_button_up board (convert/p x y w))]
              [(string=? mouse "drag")
               (board_drag board (convert/p x y w) w h)]
              [else board])
        (Board (Board-cells board)
               EMPTY_CELL))))


;--------------------- DRAW ---------------------;               

; draw_cells : List<Cell> Image Number Number Number -> Image
; Draws the a list of cells, given a base image (scene), the width in cells of
; the board (w) and the width of a cell (size). "cell_index" should always be 0.
(define (draw_cells cells scene cell_index w size)
  (if (empty? cells)
      scene
      (place-image (draw_cell (first cells) size)
                   (+ (* (modulo cell_index w) size) (/ size 2))
                   (+ (* (quotient cell_index w) size) (/ size 2))
                   (draw_cells (rest cells) scene (+ cell_index 1) w size))))

; draw_lines : List<Cell> Number Image -> Image
; Draws the lines between cells of a list of cells on a given image (scene),
; given the size of a cell (size). 
(define (draw_lines cells size scene)
  (cond [(empty? cells) scene]
        [(and (has_color? (first cells)) (has_next? (first cells)))
         (let [(cell (first cells))]
           (add-line (draw_lines (rest cells) size scene)
                     (+ (* (Cell-x cell) size) (/ size 2))
                     (+ (* (Cell-y cell) size) (/ size 2))
                     (+ (* (Cell-nx cell) size) (/ size 2))
                     (+ (* (Cell-ny cell) size) (/ size 2))
                     (make-pen (Cell-color cell)                       
                               (quotient size 4)
                               "solid"
                               "round" "round")))]
        [else (draw_lines (rest cells) size scene)]))

; draw_board : Board Image Number Number
; Draws a given board on a given image (scene), given the size of the board in
; cells (w), the size of a single cell (size) and telling if the mode is
; invisible or not (invisible?).
(define (draw_board board scene w size invisible?)
  (if invisible?
      (draw_cells (Board-cells board) scene 0 w size)
      (draw_lines (Board-cells board)
                  size
                  (draw_cells (Board-cells board) scene 0 w size))))


;----- PROVIDE -----;

(provide (all-defined-out))