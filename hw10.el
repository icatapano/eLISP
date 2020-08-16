; Ian Catapano
; C311
; Homework 10

; Ex 1a
(defun my-print (&rest L)
  "Print any number of arguments"
  (mapc 'princ L) t)
my-print

(defun root (T)
  "The root of the tree"
  (if T
      (car T)))
root

(defun left-subtree (T)
  "The left subtree, also a list"
  (if (and T (cdr T))
      (car (cdr T))))
left-subtree

(defun right-subtree (T)
  "The right subtree, also a list"
  (if (cdr (cdr T))
      (car (cdr (cdr T)))))
right-subtree

(defun print-inorder (T)
  "Function will print the tree in order. Left - Root - Right"
  (if T
      (progn
        (print-inorder (left-subtree T))
        (my-print (root T) " ")
        (print-inorder (right-subtree T)))))
print-inorder

(setq S '(23 (51 (18) (33 (5))) (7 () (10))))
; (23 (51 (18) (33 (5))) (7 nil (10)))

(print-inorder S)
; 18 51 5 33 23 7 10 nil

; Ex 1b
(defun print-iorder (T)
  "Function prints the tree of a list in order without using recursion"
  (let ((stackT nil) (frame nil) (label nil))
    (push (cons T 'left) stackT)
    (while stackT
      (setq frame (pop stackT))
      (setq T (car frame) label (cdr frame))
      (if T
          (cond ((eq label 'left)
                 (push (cons T 'right) stackT)
                 (push (cons (left-subtree T) 'left) stackT))
                ((eq label 'right)
                 (my-print (root T) " ")
                 (push (cons (right-subtree T) 'left) stackT)))))))
print-iorder

(print-iorder S)
; 18 51 5 33 23 7 10 nil

; EX 2a
; This is a type 3 regular grammars.It will produce a language equivalent to a finite state machine.
; The sentences will look like: bca, bcaa, bcbca, etc...

; Ex 2b
;    S
;   /|\
;  b c U
;     /|\
;    b c U
;        /\
;       a  V
;          /\
;         a  V
;            |
;            e

; Ex 2c
; b c a b c a is not correct.
; b c b c a a a is corrct.
; a b c a is not correct.
; b c b c b c is not correct.

; Ex 2d
(defun check-S (L)
  (if (equal (car L) 'b)
      (check-T L)
    nil))
check-S

(defun check-T (L)
  (if (and 
        (equal (car L) 'b)
        (equal (car (cdr L)) 'c))
      (check-T (cdr (cdr L)))
    (if (equal (car L) 'a)
        (check-V L)
    nil)))
check-T

(defun check-V (L)
  (if (not (cdr L))
      t
    (if (equal (car L) 'a)
        (check-V (cdr L))
    nil)))
check-V

(check-S '(b c b c a a a))
; t

(check-S '(a))
; nil

(check-S '(b c))
; nil

(check-S '(b c a b c a))
; nil

(check-S '(a b c a))
; nil

(check-S '(b c b c b c))
; nil

