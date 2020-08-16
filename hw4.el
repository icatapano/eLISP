; Ian Catapano
; C311
; Homework 4

(defun is-prime (N)
  "Function checkes the number entered to see if it is a prime number or not."
  (let ((m t)) 
  (if (not N)
      nil
    (if (or (= N 0) (= N 1))
      nil
    (dotimes (i (/ N 2) m)
      (setq f (+ i 2))
      (setq k (% N f))
      (if (= k 0)
          (setq m nil)))))))
is-prime

(defun prev-to-last (L)
  "Function returns second to last element in a list."
  (cond ((not L) (cdr L) nil)
        ((not (cdr L)) nil)
        ((not (cdr (cdr L))) (car L))
        (t (prev-to-last (cdr L)))))
prev-to-last

(defun print-list (L)
  "Funcition prints out a list with spaces between each list item."
  (setq m nil)
  (dolist (i L m)
    (princ i)
    (princ " ")))
print-list

(is-prime 1)
;nil

(is-prime 17)
;t

(is-prime 4)
;nil

(prev-to-last '(a b c))
;b

(prev-to-last '(1 2 3 4 5 6))
;5

(prev-to-last '())
;nil

(prev-to-last '(a))
;nil

(print-list '(a b c d))
;a b c d nil

(print-list '(1 2 3 4 5 6))
;1 2 3 4 5 6 nil




