; Ian Catapano
; C311
; HW2

; EX. 1 Variables in Lisp

(setq var1 3 var2 7)
7

(defvar var3 -5 "This is a negative number variable")
var3

(expt var1 var3)
0.00411522633744856

;Function definition for Random
;(random &optional LIMIT)

;Return a pseudo-random number.
;All integers representable in Lisp, i.e. between `most-negative-fixnum'
;and `most-positive-fixnum', inclusive, are equally likely. 
(random var2)
1

(abs var3)
5

(cos var2)
0.7539022543433046

(truncate var2 var1)
2

; Ex. 2 Function Definition

(defun prod-list (L)
  "Returns the product of all elements in a list using recusion"
  (if (not L)
      0
    (if (not (cdr L))
        (* (car L))
      (* (car L)
      (prod-list (cdr L))))))
prod-list
   
(defun is-in-list (L V)
  "The first variable should be the list to check, the second is the value to look for. If the value is found true is returned"
  (if (not L)
      0
    (if (equal (car L) V)
        t
      (if (not (cdr L))
          nil
        (is-in-list (cdr L) V)))))
is-in-list

(defun median (x y z)
  "Find the median value of three numbers"
  (cond
    ((and (>= x z) (<= x y)) x)
    ((and (>= x y) (<= x z)) x)
    ((and (>= y z) (<= y x)) y)
    ((and (>= y x) (<= y z)) y)
    ((and (>= z y) (<= z x)) z)
    ((and (>= z x) (<= z y)) z)))
median

(prod-list '())
;0

(prod-list '(4 5 2))
;40

(prod-list '(2 5 9 1 6))
;540

(is-in-list '() 'k)
;0

(is-in-list '(a b c d) 'a)
;t

(is-in-list '(a b c d) 'd)
;t

(is-in-list '(a b c d) 'z)
;nil

(median 3 4 5)
;4

(median 5 1 3)
;3

(median 7 10 3)
;7


