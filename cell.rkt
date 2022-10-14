#lang racket

(require "color.rkt")
(require "utilities.rkt")
(require 2htdp/image)
(require lang/posn)

; Struct Cell : Number Number Number Number Color Color
(struct Cell (x y nx ny color dot))
; Example:
(define EMPTY_CELL (Cell -1 -1 -1 -1 N N))

; set-Cell-color : Cell Color -> Cell
; Sets the color of the cell.
(define (set-Cell-color cell color)
  (Cell (Cell-x cell)
        (Cell-y cell)
        (Cell-nx cell)
        (Cell-ny cell)
        color
        (Cell-dot cell)))

; set-Cell-dot : Cell Color -> Cell
; Sets the dot-color of the cell.
(define (set-Cell-dot cell dot)
  (Cell (Cell-x cell)
        (Cell-y cell)
        (Cell-nx cell)
        (Cell-ny cell)
        (Cell-color cell)
        dot))

; set-Cell-next : Cell Cell -> Cell
(define (set-Cell-next cell next)
  (Cell (Cell-x cell)
        (Cell-y cell)
        (Cell-x next)
        (Cell-y next)
        (Cell-color next)
        (Cell-dot cell)))

; cell_empty? : Cell -> Boolean
(define (cell_empty? cell)
  (if (or (= (Cell-nx cell) -1) (= (Cell-ny cell) -1))
      #true
      #false))

; dot_size : Number -> Number
(define (dot_size size)
  (quotient size 3))

; cell_size : Number -> Number
(define (cell_size cells)
  (ceiling (/ FIELD_WIDTH cells)))

; cell_index : Number Number Number -> Number
(define (cell_index x y size)
  (+ (modulo x (cell_size size)) (modulo y (cell_size size))))

; has_next? : Cell -> Boolean
(define (has_next? cell)
  (if (or (= (Cell-nx cell) -1)
          (= (Cell-ny cell) -1))
      #false
      #true))

; has_dot? : Cell -> Boolean
(define (has_dot? cell)
  (if (string=? (Cell-dot cell) N) #false #true))

; has_color? : Cell -> Boolean
(define (has_color? cell)
  (if (string=? (Cell-color cell) N) #false #true))

; is_dot? : Cell Color -> Boolean
(define (is_dot? cell color)
  (if (string=? (Cell-dot cell) color) #true #false))

; is_color? : Cell Color -> Boolean
(define (is_color? cell color)
  (if (string=? (Cell-color cell) color) #true #false))

; same_pos? Cell Cell -> Boolean
(define (same_pos? c1 c2)
  (if (and (= (Cell-x c1) (Cell-x c2))
           (= (Cell-y c1) (Cell-y c2)))
      #true
      #false))

; same_pos_next? Cell Cell -> Boolean
(define (same_pos_next? c1 c2)
  (if (and (= (Cell-x c1) (Cell-nx c2))
           (= (Cell-y c1) (Cell-ny c2)))
      #true
      #false))

;--------------------- DRAW ---------------------;

; draw_dot : Cell Number -> Image
(define (draw_dot cell size)
  (if (has_dot? cell)
;      (if (has_color? cell)
          (circle size "solid" (Cell-dot cell))
;          (circle size "outline" (Cell-dot cell)))
      (circle size "solid" (color 0 0 0 0))))
      
; draw_color : Cell Number -> Image
(define (draw_color cell size)
  (if (has_color? cell)
      (rectangle (/ size 3) (/ size 3) "solid" (Cell-color cell))
      (rectangle size size "solid" (color 0 0 0 0))))

; draw_cell : Cell Number -> Image
(define (draw_cell cell size)
  (place-images (list (draw_dot cell (dot_size size)))
                      ;(draw_color cell size))
                (list (make-posn (/ size 2) (/ size 2)))
                      ;(make-posn (/ size 2) (/ size 2)))
                (rectangle size size "outline" TEXT_COLOR)))


;----- PROVIDE -----;

(provide (all-defined-out))