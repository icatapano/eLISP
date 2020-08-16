; Ian Catapano
; C311
; Homework 5

(defun gcd (n m)
  "Function will return the greatest common divisor of two numbers."
  (while (not (= m 0))
    (setq temp m)
    (setq m (% n m))
    (setq n temp))
    n)
gcd

(defun replace (L z y)
  "Function will take a list and two values, for every instance of the first value it finds, it will replace that value with the second value."
  (mapcar (lambda (x) (if (equal x z) y x)) L))
replace

(defun print-list (L)
  "Function prints all elements of a list with a space between them."
  (mapc (lambda (x) (princ x) (princ " ")) L))
print-list

(defun make-multiples (n m)
  "Make a list of m number of multiples of n."
  (setq L '())
  (setq x 1)
  (dotimes (i m L)
    (setq x (* x n))
    (push x L))
  (setq F '())
  (dolist (i L F)
    (push i F)))
make-multiples

(defun is-multiple (L)
  "Checks a list to see if the list is multiples, returns true if it is."
  (setq m t)
  (setq z (car L))
  (setq B (cdr L))
  (dolist (i B m)
    (if (not (= (% i z) 0))
      (setq m nil))))
is-multiple

(gcd 5 10)
;5

(gcd 10 5)
;5

(gcd 1 7)
;1

(gcd 55 99)
;11

(replace '(3 1 5 6 3 2 3) 3 9)
;(9 1 5 6 9 2 9)

(replace '(4 8 2 5 6 9 1) 2 10)
;(4 8 10 5 6 9 1)

(setq R '(k o m a e i))
;(k o m a e i)

(print-list R)
;k o m a e i (k o m a e i)

(apply 'print-list (list R))
;k o m a e i (k o m a e i)

(funcall 'print-list R)
;k o m a e i (k o m a e i)

(make-multiples 2 6)
;(2 4 8 16 32 64)

(make-multiples 5 10)
;(5 25 125 625 3125 15625 78125 390625 1953125 9765625)

(is-multiple '(2 4 8))
;t

(is-multiple '(4 2 8))
;nil

(is-multiple (make-multiples 2 3))
;t
