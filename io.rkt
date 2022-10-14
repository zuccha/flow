#lang racket

(require "utilities.rkt")
(require 2htdp/batch-io)

; save_string : List<Boolean> Number -> String
; It returns a string composed of the level indexes for the values, of the given
; list of boolean , corresponding to #true (the levels that are unlocked).
(define (save_string unlocked i)
  (cond [(empty? unlocked) ""]
        [(first unlocked) (string-append (number->string i) " "
                                         (save_string (rest unlocked) (+ i 1)))]
        [else (save_string (rest unlocked) (+ i 1))]))

; save : String List<Boolean> -> String
; Saves the levels unlocked on a given file.
(define (save file unlocked)
  (write-file "save.txt" (save_string unlocked 0)))

; reset : String -> String
; Resets the unlocked levels on the given save file.
(define (reset file)
  (write-file "save.txt" ""))

; set_unlocked : List<Boolean> List<Number> -> List<Boolean>
; It makes a list of unlocked level from a base list of unlocked levels and a
; list of unlocked levels from a save file.
(define (set_unlocked unlocked bs)
  (cond [(empty? bs) unlocked]
        [(<= 0 (first bs) (length unlocked))
         (set_unlocked (set unlocked (first bs) #true) (rest bs))]
        [else (set_unlocked unlocked (rest bs))]))

; read_unlocked : String -> List<Boolean>
; It makes a list of unlocked levels from a save file.
(define (read_unlocked file)
    (set_unlocked BASE_UNLOCKED (map string->number (read-words file))))

; PROVIDE
(provide (all-defined-out))