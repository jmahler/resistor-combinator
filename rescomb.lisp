
; Given n resistors, finds their unique combinations
; and their equivalent resistances.

(defun series (R1 R2)
  (+ R1 R2))

(defun parallel (R1 R2)
  (/ 1 (+ (/ 1 R1) (/ 1 R2))))

(defvar *resistors*)
(setq *resistors* (list 1.2e3 1.2e3 1.2e3))

;
; Creates set combinations built on the head by using the
; source values.
;
; * (setcombs '(A) '(B C))
;
; (((C A) (B)) ((B A) (C)))
; * 
;
; Given a starting value of '(A) and using the values of '(B C)
; the combinations are: (A C) with a remainder of (B), and (B A)
; with a remainder of (C).
; Notice that every combination includes all values in the set
; which includes the head and the source/remainder.
;
; Further combinations can be built using these values as the head
; along with their remainder.
;
; Conceptually the items should be considered as "sets" and the
; order should be disregarded.
;
(defun setcombs (head src)
  "Generate set combinations on the head using the source values"
  (let ((acc nil)
		(res nil))
	(loop for i on src do
		  (setq res (cons
					  (list
						(cons (car i) head)
						(append acc (cdr i)))
					  res))
		  (setq acc (cons (car i) acc)))
	res))

