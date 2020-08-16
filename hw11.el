; Ian Catapano
; C311
; Homework 11

; Ex 1
; a.
;
; A => yB
; B => wC
; C => zB
; C => yD
; D => e
;
; b.
;
; This is a regualr grammar, the string must start with a y and end with a y and will have at least one w.
; If there is more than one w there will be a z between them.
;
; c.
;
; This is a DFA because there is only one possible outgoing branch for each state.
;
; d.

(defun check-A (L)
  (if (equal (car L) 'y)
      (check-B (cdr L))
    nil))
check-A

(defun check-B (L)
  (if (equal (car L) 'w)
      (check-C (cdr L))
    nil))
check-B

(defun check-C (L)
  (cond
    ((equal (car L) 'z) (check-B (cdr L)))
    ((and (equal (car L) 'y) (not (cdr L))) t)
    (t nil)))
check-C

(check-A '(y w z w y))
; t

(check-A '(y z w w))
; nil

; Ex 2
;
;Grammar
;
; S => A
; A => /B 
; B => *C
; C => charC
; C => *D
; D => charC
; D => /E
; E => e
;
; FSM
;
; Start -> A -> (use /) B -> (use *) C -> (use "any char except *") C -> (use "any char except *") D -> (use *)
; E-> (use /) -> End
;
; Table
;
;     /     *     char  else
; A   B     err   err   err
; B   err   C     err   err
; C   err   C/D   C     err
; D   E     err   C     err
; E   err   err   err   end

