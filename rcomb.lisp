
; 
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
; 

; NOTE: the description below is duplicated in README.md
;
; NAME
; ----
; 
; Resistor Combinator - Generate combinations of resistors in series and parallel.
; 
; INTRODUCTION
; ------------
; 
; A single resistor has a single combination.
; Two resistors have two combinations: one in series and one in parallel.
; Three resistors have many combinations.
; How many combinations do n resistors have?
; 
; This project aims to answer this question by generating all the possible
; combinations and calculating their equivalent resistances.
; 
; USAGE
; -----
;
;     ; An example session for the Resistor Combinator
;     
;    (defvar r)
;    (defvar s)
;     
;    ; several UNIQUE resistors
;    (setq r '(1 2 3))
;    (setq s (allcombs r))
;     
;    (uniquevals s)
;    ; (6/11 5/6 4/3 3/2 11/5 11/4 11/3 6)
;     
;    ; several IDENTICAL resistors
;    (setq r '(1 1 1))
;    (setq s (allcombs r))
;     
;    (uniquevals s)
;    ; (1/3 2/3 3/2 3)
;     
;    (dispcombvals s)
;     
;    ; symbolic display of resistor combinations
;    (setq r '(A B C))
;    (setq s (allcombs r))
;     
;    (dispcombs s)
;    ; (+ (+ A B) C) 
;    ; (|| (+ A B) C) 
;    ; (+ (|| A B) C) 
;    ; (|| (|| A B) C) 
;    ; (+ (+ A C) B) 
;    ; (|| (+ A C) B) 
;    ; (+ (|| A C) B) 
;    ; (|| (|| A C) B) 
;    ; (+ (+ B C) A) 
;    ; (|| (+ B C) A) 
;    ; (+ (|| B C) A) 
;    ; (|| (|| B C) A) 
;
 
(defun series (R1 R2)
  (+ R1 R2))

;(defun parallel (R1 R2)
(defun || (R1 R2)
  (/ 1 (+ (/ 1 R1) (/ 1 R2))))

; {{{ firstpairs
; Given a set of items, a list of all the unique pairs is
; created along with the remaining elements for that particular
; combination.
;
; * (firstpairs '(A B C D E F))
;
; (((A B) (C D E F)) ((A C) (B D E F)) ((A D) (C B E F)) ((A E) (D C B F))
;  ((A F) (E D C B)) ((B C) (A D E F)) ((B D) (A C E F)) ((B E) (A D C F))
;  ((B F) (A E D C)) ((C D) (B A E F)) ((C E) (B A D F)) ((C F) (B A E D))
;  ((D E) (C B A F)) ((D F) (C B A E)) ((E F) (D C B A)))
; *
;
(defun firstpairs (src)
  "Build the first unique sets of pairs (order unimportant) including the remainder"
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

; {{{ pairup
;
; Combine the first pair in to two single combination in series and parallel.
(defun pairup (src)
  "Pair up combinations of resistors in series and parallel."
  (mapcon (lambda (s)
			(list
			  (list
				;`(series
				`(+
				   ,(first (caar s))
				   ,(second (caar s)))
				(cadar s))
			  (list
				;`(parallel
				`(||
				   ,(first (caar s))
				   ,(second (caar s)))
				(cadar s))
			  ))
		  src))
; }}}

; {{{ secondpair, secondpairs
;
; Treat the first list as a single element and create the next
; set of pairs using the remaining elements.
;
; * (secondpair '((PARALLEL A B) (C D)))
;
; ((((PARALLEL A B) D) (C)) (((PARALLEL A B) C) (D)))
; * 
;
(defun secondpair (cs)
  "Build a list of pairs given an element and remaining elements"
  (let ((head (car cs))
		(tail (cadr cs))
		(res nil)
		(acc nil))
	(loop for j on tail do
		  (setq res (cons
					  (list
						(list head (car j))
						(append acc (cdr j)))
					  res))
		  (setq acc (cons (car j) acc)))
	res))

(defun secondpairs (src)
  "Find all second pairs in the list"
  (apply #'append (mapcar #'secondpair src)))
; }}}

; {{{ allcombs

; This calculation quickly becomes unwieldly.
; With 5 elements there are 43200 combinations and with 6
; elements there are too many to calculate.
;
; Currently there are many duplicates so the following
; numbers are greater than the number of unique combinations.
;
; * (length (allcombs '(A B)))
; 2
; * (length (allcombs '(A B C)))
; 12
; * (length (allcombs '(A B C D)))
; 96
; * (length (allcombs '(A B C D E)))         ; 5
; 960
; * (length (allcombs '(A B C D E F)))       ; 6
; 11520
; * (length (allcombs '(A B C D E F G)))     ; 7
; 161280
; * (length (allcombs '(A B C D E F G H)))   ; 8
; too big to calculate!
;
;
; If all the combinations were generated correctly it should
; result in a remainder of null (add this to a test script).
;

(defun _sndcombs(cs1)
  (let* ((cs2 (pairup cs1)))
	(if (null (cadar cs2))
	  cs2
	  (_sndcombs (secondpairs cs2)))))

(defun allcombs (cs0)
  (if (null (cdr cs0))  ; atleast 2 elements
	nil
	(let* ((cs1 (firstpairs cs0)))
	  (_sndcombs cs1))))

; }}}

; {{{ display utilities

(defun uniquevals (x)
  "Display all unique values in sorted order."
  (remove-duplicates (sort (mapcar (lambda (y) (eval (car y))) x) #'<)))

(defun dispvals (x)
  "Display all the values."
  (mapc (lambda (x)
		  (print (eval (car x))))
		x)
  t)

(defun dispcombvals (x)
  "Display the combinations along with their associated value.".
  (mapc (lambda (x)
		  (print (car x))
		  (print (eval (car x))))
		x)
  t)

(defun dispcombs (x)
  "Display all the combinations."
  (mapc (lambda (x)
		  (print (car x)))
		x)
  t)
; }}}

; vim:foldmethod=marker

