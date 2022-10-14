#lang racket

(require "board.rkt")
(require "cell.rkt")
(require "color.rkt")
(require "utilities.rkt")
(require 2htdp/image)

; Definition of all boards of all levels
(define BOARD_1 (make_board '() 6 6 0 0 (list N N N N N N
                                              N B O N O R
                                              N N N N N B
                                              N N N N R G
                                              G Y N N Y N
                                              N N N N N N)))

(define BOARD_2 (make_board '() 6 6 0 0 (list N G Y N N N
                                              N B R N N N
                                              N N N B N N
                                              N N N G N N
                                              N N N N N N
                                              R Y N N N N)))

(define BOARD_3 (make_board '() 6 6 0 0 (list N N B N N N
                                              N G R N Y N
                                              B N N N N R
                                              N N N N N N
                                              Y O N N G N
                                              O N N N N N)))

(define BOARD_4 (make_board '() 6 6 0 0 (list N N N N O N
                                              N N G N B N
                                              N N N N N N
                                              N N N R N N
                                              N N N N N O
                                              Y G Y R N B)))

(define BOARD_5 (make_board '() 6 6 0 0 (list Y N N N N N
                                              B N N N N N
                                              R B N G A N
                                              N N N N N N
                                              N N N O G N
                                              R A O Y N N)))

(define BOARD_6 (make_board '() 6 6 0 0 (list N N A B N N
                                              A N N N Y N
                                              N N N G O N
                                              N G N R N N
                                              Y R N N O N
                                              B N N N N N)))

(define BOARD_7 (make_board '() 6 6 0 0 (list N Y N N N R
                                              N R N B N N
                                              N A N G O N
                                              N N A N N N
                                              G N Y N O N
                                              N N N N B N)))

(define BOARD_8 (make_board '() 6 6 0 0 (list N N N N N O
                                              N G N N N Y
                                              N R N R N N
                                              N N N N N N
                                              N N B N B N
                                              O N G Y N N)))

(define BOARD_9 (make_board '() 6 6 0 0 (list G O N N N B
                                              N N N N N R
                                              N N B R G N
                                              N N N N O N
                                              N Y N N Y N
                                              N N N N N N)))

(define BOARD_10 (make_board '() 6 6 0 0 (list B N N N N N
                                               N N N N N N
                                               N N N N Y N
                                               N N Y G R N
                                               N G N R N N
                                               N N N B N N)))

(define BOARD_11 (make_board '() 7 7 0 0 (list N N N N N N N
                                               B P A N N R N
                                               N N N B N N N
                                               N O N A Y N N
                                               P N N N N N N
                                               Y G N G O R N
                                               N N N N N N N)))

(define BOARD_12 (make_board '() 7 7 0 0 (list N N N N N N N
                                               N N N N N R N
                                               G R O G N N N
                                               A P N N N N Y
                                               N N N Y N N N
                                               N N N N N B N
                                               A P B N N N O)))

(define BOARD_13 (make_board '() 7 7 0 0 (list N N N N N Y O
                                               N R B N B R N
                                               N N N N A N N
                                               N N N A N N O
                                               G N N N N N Y
                                               N N N N N N G
                                               N N N N N N N)))

(define BOARD_14 (make_board '() 7 7 0 0 (list N N N N N N N
                                               N O M R A P N
                                               N N G N N N R
                                               N O B N N N N
                                               N B N G N Y N
                                               N M N N N N N
                                               A N N Y N N P)))

(define BOARD_15 (make_board '() 7 7 0 0 (list N O B N N N N
                                               O R G N R Y N
                                               N N N N N N N
                                               N B N G N N N
                                               N N N N N N N
                                               N Y N N N N N
                                               N N N N N N N)))

(define BOARD_16 (make_board '() 7 7 0 0 (list N N N N N N N
                                               N N N N N N N
                                               N N N N N N N
                                               N N O B N N N
                                               N N N N N B G
                                               N G R N N O R
                                               Y N Y N N N N)))

(define BOARD_17 (make_board '() 7 7 0 0 (list N N N N N N R
                                               N P N N N N N
                                               N B N N P N N
                                               N O Y N B O N
                                               N A N N N R N
                                               N G N N N Y G
                                               N N A N N N N)))

(define BOARD_18 (make_board '() 7 7 0 0 (list N N N N N N N
                                               N G N N N Y N
                                               N N Y N N B R
                                               N N N N N N P
                                               N N N N N O A
                                               P R G B N N N
                                               N N N N N O A)))

(define BOARD_19 (make_board '() 7 7 0 0 (list N N N Y N N N
                                               N N N N N A N
                                               N N N N N N N
                                               N N N N N G R
                                               N N N G N A B
                                               N N N N N O N
                                               Y R O N N N B)))

(define BOARD_20 (make_board '() 7 7 0 0 (list G N G R N N N
                                               N N R O N N N
                                               N B N N Y N N
                                               N Y N B N N N
                                               N O N N N N N
                                               N N N N N N N
                                               N N N N N N N)))

(define BOARD_21 (make_board '() 8 8 0 0 (list N N N N N G P N
                                               N N N N N N A N
                                               N N N N G N N N
                                               N N N Y N N A N
                                               N Y N O N N N N
                                               N N N R N N O N
                                               N N N N N N R N
                                               P B N B N N N N)))

(define BOARD_22 (make_board '() 8 8 0 0 (list N N N N O N B N
                                               N N A N N O R N
                                               N N B G N N N N
                                               N N N N R N N N
                                               N N N N N Y N N
                                               N N N N N N N N
                                               N A N N N N G N
                                               N Y N N N N N N)))

(define BOARD_23 (make_board '() 8 8 0 0 (list G N N N N N N A
                                               N N B O A P N N
                                               N N N N N N B N
                                               N N N N N N N N
                                               N Y N N N N N N
                                               N N N N R N N N
                                               N R N N G Y O N
                                               N N N P N N N N)))

(define BOARD_24 (make_board '() 8 8 0 0 (list N G O N N N N N
                                               N O A N N N N N
                                               N N N A N R N N
                                               N Y N P N N N N
                                               N N N N N R B N
                                               N N N N N B P N
                                               N N N N N N N N
                                               G Y N N N N N N)))

(define BOARD_25 (make_board '() 8 8 0 0 (list P N N A N N N N
                                               N Y N R B N N N
                                               N P N N N N N N
                                               N O N N N N N N
                                               N N N Y N N N N
                                               N N N G N N N N
                                               G N N N N N B N
                                               O N R N N N A N)))

(define BOARD_26 (make_board '() 8 8 0 0 (list B N N N N N N N
                                               Y A N O N N N N
                                               N N B N N M N N
                                               N A G N N N N N
                                               N N P N M N N N
                                               R N Y N P G R N
                                               N N N N N N N N
                                               O N N N N N N N)))

(define BOARD_27 (make_board '() 8 8 0 0 (list N N N N N N N N
                                               N P N N N N N O
                                               N N N N N N N A
                                               N N N N G B N N
                                               N N N N N N N N
                                               N N G N N Y N N
                                               N N B P Y A N R
                                               N N N O R N N N)))

(define BOARD_28 (make_board '() 8 8 0 0 (list N N N N N N N N
                                               N N P O N N N B
                                               N N N N N Y N N
                                               N P N G N N A N
                                               N R N N N N N N
                                               N G N N R N N N
                                               N Y N N N N N N
                                               N N N B A N N O)))

(define BOARD_29 (make_board '() 8 8 0 0 (list N N N B N N N N
                                               N G N N N N A N
                                               N R N O N N N N
                                               N Y N N N G N B
                                               N N N N N N N N
                                               N N R N N N N N
                                               N N N N Y O A N
                                               N N N N N N N N)))

(define BOARD_30 (make_board '() 8 8 0 0 (list N N N N O P O N
                                               N N N N R N A N
                                               N N G N N N N N
                                               N N N N N P A N
                                               N N G B N N N N
                                               N N N N N Y N N
                                               N B Y R N N N N
                                               N N N N N N N N)))

;----- PROVIDE -----;

(provide (all-defined-out))