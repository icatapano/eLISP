; Ian Catapano
; C311
; Homework 6

; Ex. 1

(defun next-day (s)
  "Function takes a symbol argument that is the name of a day of the week and returns the next day."
  (cond
    ((eq s 'sunday) 'monday)
    ((eq s 'monday) 'tuesday)
    ((eq s 'tuesday) 'wednesday)
    ((eq s 'wednesday) 'thursday)
    ((eq s 'thursday) 'friday)
    ((eq s 'friday) 'saturday)
    ((eq s 'saturday) 'sunday)))
next-day

(defvar days '(sunday monday tuesday wednesday thursday friday saturday) "Global variable containing list of days of the week.")
days

(defun random-day ()
  "Function will pick a random day of the week from the week list and return that along with the next day."
  (setq d (random 7))
  (setq m (elt days d))
  (setq n (next-day m))
  (mapc 'princ (list "Today is " (capitalize (symbol-name m)) " and tomorrow will be " (capitalize (symbol-name n)) "."))
  (t))
random-day

(defun first-fit (L n)
  "Take a list of numbers and a number, returns the first element in the list that is equal to or larger than the value."
  (setq m nil)
  (dolist (i L m)
    (if (and (>= i n) (eq m nil))
        (setq m i))))
first-fit

(defun best-fit (L n)
  "Take a list of numbers and a number, returns the smallest element in the list that is larger or equal to the value."
  (setq m nil)
  (dolist (i L m)
    (if (and (>= i n) (eq m nil))
        (setq m i)
      (if (and (>= i n) (<= i m))
          (setq m i)))))
best-fit
   
(defun calender (d n)
   "Take the day of the week the month starts on in a number starting at 0 for Sunday, and the number of days in the month."
  (dotimes (i 7 t)
    (mapc 'princ (list (capitalize (substring (symbol-name (elt days i)) 0 1)) "  ")))
  (princ "\n")
  (dotimes (i d "   ")
    (princ "   "))
  (dotimes (i n "")
    (if (= (% (+ i d) 7) 0)
      (princ "\n"))
    (princ (+ 1 i))
    (if (<= i 8)
        (princ "  ")
      (princ " ")))
  (t))
calender

(next-day 'sunday)
;monday

(next-day 'wednesday)
;thursday

(next-day 'saturday)
;sunday

(random-day)
;Today is Tuesday and tomorrow will be Wednesday.
;Today is Saturday and tomorrow will be Sunday.

(first-fit '(3 9 1 4 8) 4)
;9

(first-fit '(1 2 3 4 5 6) 5)
;5

(first-fit '(1 2 3 4) 7)
;nil

(best-fit '(2 8 4 3 7 5 6 9 1) 4)
;4

(best-fit '(2 3 1) 5)
;nil

(best-fit '(1 2 4 6 8) 5)
;6

(calender 3 29)
; S  M  T  W  T  F  S  
;          1  2  3  4  
; 5  6  7  8  9  10 11 
; 12 13 14 15 16 17 18 
; 19 20 21 22 23 24 25 
; 26 27 28 29 

(calender 5 31)
; S  M  T  W  T  F  S  
;                1  2  
; 3  4  5  6  7  8  9  
; 10 11 12 13 14 15 16 
; 17 18 19 20 21 22 23 
; 24 25 26 27 28 29 30 
; 31 
