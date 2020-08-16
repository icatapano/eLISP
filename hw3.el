; Ian Catapano
; C311
; HW3

(defun is-const (L)
  "Function checks a list to see if all values in the list are constent or not."
  (let ((m t))
    (if (or (not L) (not (cdr L)))
        m
      (setq x (car L))
      (setq L (cdr L))
      (dolist (i L m)
        (if (not (equal x (car L)))
            (setq m nil)
          (setq x (car L))
          (setq L (cdr L)))
       m))))
is-const

(defun element-i (L V)
  "Function checks list for the value in that element and returns it, if nothing in that element, will return nil."
    (let ((m nil))
    (setq x (+ V 1))
    (dotimes (i x m)
      (setq m (car L))
      (pop L))))
element-i

(defun is-sorted (L)
  "Function checks if the list is sorted or not."
    (let ((x nil))
    (setq m t)
    (if (or (not L) (not (cdr L)))
        m
      (setq x (car L))
      (setq L (cdr L))
      (dolist (i L m)
        (if (> x (car L))
            (setq m nil)
          (setq x (car L))
          (setq L (cdr L)))))))
is-sorted

(defun reverse (L)
  "Function takes a list and reverse the contents of that list."
  (if (not L)
      nil
    (setq O nil)
    (dolist (i L O)
      (push (car L) O)
      (setq L (cdr L)))))
reverse

(is-const '(a b c))
;nil

(is-const '())
;t

(is-const '(g))
;t

(is-const '(z z z z))
;t

(element-i '(a b c d) 2)
;c

(element-i '() 1)
;nil

(element-i '(d f) 4)
;nil

(element-i '(2 4 6 8) 1)
;4

(is-sorted '())
;t

(is-sorted '(3))
;t

(is-sorted '(4 4 4))
;t

(is-sorted '(1 2 3 4))
;t

(is-sorted '(4 5 8 3))
;nil

(reverse '(a b c d e))
;(e d c b a)

(reverse '())
;nil

(reverse '(1 2 3 4 5))
;(5 4 3 2 1)
