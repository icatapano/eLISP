; Ian Catapano
; C311
; Homework 8

; Ex. 1
(defun random-select (L)
  "Randomly select an element of the list"
  (while (and L (cdr L) (not (= (random 2) 0)))
    (setq L (cdr L)))
  (car L))
random-select

; (random-select '(3 8 7 0 1 2))
; 8

; (random-select '(a b c d e))
; c

; Ex. 2 a
(defun count-val (L val c)
  "Count the number of times a given value appears in a list, using tail recursion"
  (cond ((not L) c)
        ((equal (car L) val)
         (count-val (cdr L) val (+ 1 c)))
        (t (count-val (cdr L) val c))))
count-val

; (count-val '(1 9 4 3 4 3 4) 4 0)
; 3

; (count-val '(2 3 8 4 3) 3 0)
; 2

; (count-val '(2 4 3 2) 3 0)
; 1

;Ex. 2 b
(defun c-val (L val c)
 "Count the number of times a given value appears in a list, itteratively"
  (while L
    (if (equal (car L) val)
        (setq c (+ 1 c)))
    (setq L (cdr L)))
  c)
c-val

; (c-val '(2 1 3 1 8 1) 1 0)
; 3

; (c-val '(2 8 4 8) 8 0)
; 2

; Ex. 3
(defun sum-numbers (&rest L)
  "Take a list of any size and sum the numberical values only"
  (let ((s 0))
  (while L
    (if (numberp (car L))
        (setq s (+ s (car L))))
    (setq L (cdr L)))
  s))
sum-numbers

; (sum-numbers '3 '4 'a 'p '9)
; 16

; (sum-numbers 'a 'b)
; 0

; (sum-numbers '1 'd '3 'h '8 '9 'j)
; 21


