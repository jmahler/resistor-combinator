
; Given n resistors, finds their unique combinations
; and their equivalent resistances.

(defun series (R1 R2)
  (+ R1 R2))

(defun parallel (R1 R2)
  (/ 1 (+ (/ 1 R1) (/ 1 R2))))

(defvar *resistors*)
(setq *resistors* (list 1.2e3 1.2e3 1.2e3))

; {{{ (paircombs src)
; Given a set of items, a list of all the unique pairs is
; created along with the remaining elements for that particular
; combination.
;
; * (paircombs '(A B C D E F))
;
; (((A B) (C D E F)) ((A C) (B D E F)) ((A D) (C B E F)) ((A E) (D C B F))
;  ((A F) (E D C B)) ((B C) (A D E F)) ((B D) (A C E F)) ((B E) (A D C F))
;  ((B F) (A E D C)) ((C D) (B A E F)) ((C E) (B A D F)) ((C F) (B A E D))
;  ((D E) (C B A F)) ((D F) (C B A E)) ((E F) (D C B A)))
; *
;
(defun paircombs (src)
  "Build unique sets of pairs (order unimportant) including the remainder"
  (let ((acc1 nil)
		(res nil))
	(loop for i on src do
		  (let ((acc2 nil))
			(loop for j on (cdr i) do
				  (setq res (cons
							  (list
								(list (car i) (car j)) ; a combination
								(append acc1 acc2 (cdr j))) ; remainder
							  res))
				  (setq acc2 (cons (car j) acc2))))
		  (setq acc1 (cons (car i) acc1)))
	(reverse res)))
; }}}

; {{{ setcombs [DISABLED]

; create combinations of pairs and include the remainder
;(defun paircombs (rs)
;  (if (null (second rs)) ; < 2 combinations
;	nil ; error
;	(mapcan (lambda (x) (setcombs (first x) (second x)))
;		  (setcombs nil rs))))

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
;(defun setcombs (head src)
;  "Generate set combinations on the head using the source values"
;  (let ((acc nil)
;		(res nil))
;	(loop for i on src do
;		  (setq res (cons
;					  (list
;						(cons (car i) head)
;						(append acc (cdr i)))
;					  res))
;		  (setq acc (cons (car i) acc)))
;	res))
; }}}

; vim:foldmethod=marker
