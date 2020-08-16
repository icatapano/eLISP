; Ian Catapano
; C311
; Homework 9

(defun recus-search (L v)
  "Function will be called by the driver function in the case of a nested list."
  (dolist (i L nil)
    (if (listp i)
         (recus-search i v)
       (if (equal i v)
           (throw 'found t)))))
recus-search

(defun deep-search (L v)
  "This is the driver function that will call the recursive function will searching a possibly nested list for a value."
  (if (not L)
      nil
    (catch 'found
     (recus-search L v))))
deep-search

(deep-search '() 8)
; nil
 
(deep-search '(2 7 5 9) 7)
; t

(deep-search '(4 8 '(9 5 3) 6 1) 5)
; t

(setq count 0)
; 0
              
(defun comb (n m)
  (setq count (+ 1 count))
  (cond
   ((= n m) 1)
   ((= m 0) 1)
   ((= m 1) n)
   (t (+ (comb (- n 1) m)
         (comb (- n 1) (- m 1))))))
comb

(comb 6 2)
; 15
count
; 9

(comb 5 2)
; 10
count
; 7

(comb 7 4)
; 35
count
; 39

; Improved version with DP:

(defun el10 (n m)
  (+ (* n 10) m))
el10

(setq C (make-vector 100 nil))
[nil nil nil nil nil nil nil nil nil nil nil nil ...]

(defun comb1 (n m)
  (setq count (+ 1 count))
  (let ((res 0))
    (if (setq res (elt C (el10 n m))) res
      (setq res
            (cond
             ((= n m) 1)
             ((= m 0) 0)
             ((= m 1) n)
             (t (+ (comb1 (- n 1) m)
                   (comb1 (- n 1) 
                          (- m 1))))))
      (aset C (el10 n m) res))))
comb1

(comb1 6 2)
; 15
count
; 9

(comb1 5 2)
; 10
count
; 1

(comb1 7 4)
; 35
count
; 13

; In most cases there is a significant reduction in function calls, which reduces run time. In the case of combining
; 6 and 2, both functions gave the same number of function calls. In the other two cases the dynamic function has
; fewer function calls.

(defun comp (n)
  "This function will compute a number where 0 = 0, 1 or 2 = 1, and any higher number will be the result of (n -1) + (n -3)"
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    ((= n 2) 1)
    (t (+ (comp (- n 1))
          (comp (- n 3))))))
comp

(comp 4)
; 2

(comp 1)
; 1

(defun elm10 (n)
  (* n 10))
elm10

(setq V (make-vector 200 nil))
[nil nil nil nil nil nil nil nil nil nil nil nil ...]

(defun comp1 (n)
  "Improved version of the compute function with dynamic programming."
  (let ((res 0))
    (if (setq res (elt V (elm10 n))) res
      (setq res
            (cond
             ((= n 0) 0)
             ((or (= n 1) (= n 2)) 1)
             (t (+ (comp1 (- n 1))
                   (comp1 (- n 3))))))
      (aset V (elm10 n) res))))
comp1

(comp1 1)
; 1

(comp1 4)
; 2

(comp1 15)
; 129


