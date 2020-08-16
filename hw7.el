; Ian Catapano
; C311
; Homework 7

(defun bubSort (A)
  "Function implements the bubble sort on an array, then returns the sorted array."
  (cond
    ((not (arrayp A)) A)
    (t 
      (setq k (length A))
      (setq j (- (length A) 1))
      (dotimes (i k A)
        (dotimes (i j t)
          (if (> (elt A i) (elt A (+ i 1)))
            (progn
              (setq s (elt A i))
              (aset A i (elt A (+ i 1)))
              (aset A (+ 1 i) s))))))))
bubSort

(setq P '[55 20 60 3 30 66 1])
; [55 20 60 3 30 66 1]


(setq N '(a d l o))
; (a d l o)

(bubSort P)
; [1 3 20 30 55 60 66]

(bubSort N)
; (a d l o)
