
NAME
----

Resistor Combinator - Generate combinations of resistors in series and parallel.

INTRODUCTION
------------

A single resistor has a single combination.
Two resistors have two combinations: one in series and one in parallel.
Three resistors have many combinations.
How many combinations do n resistors have?

This project aims to answer this question by generating all the possible
combinations and calculating their equivalent resistances.

USAGE
-----

    ; An example session for the Resistor Combinator
     
    (defvar r)
    (defvar s)
     
    ; several UNIQUE resistors
    (setq r '(1 2 3))
    (setq s (allcombs r))
     
    (uniquevals s)
    ; (6/11 5/6 4/3 3/2 11/5 11/4 11/3 6)
     
    ; several IDENTICAL resistors
    (setq r '(1 1 1))
    (setq s (allcombs r))
     
    (uniquevals s)
    ; (1/3 2/3 3/2 3)
     
    (dispcombvals s)
     
    ; symbolic display of resistor combinations
    (setq r '(A B C))
    (setq s (allcombs r))
     
    (dispcombs s)
    ; (+ (+ A B) C) 
    ; (|| (+ A B) C) 
    ; (+ (|| A B) C) 
    ; (|| (|| A B) C) 
    ; (+ (+ A C) B) 
    ; (|| (+ A C) B) 
    ; (+ (|| A C) B) 
    ; (|| (|| A C) B) 
    ; (+ (+ B C) A) 
    ; (|| (+ B C) A) 
    ; (+ (|| B C) A) 
    ; (|| (|| B C) A) 


AUTHOR
------

This project was completed as an extra credit assignment
for the class "Circuits and Devices (ENGR 17)".
The class was taught by Dr Robert White during the Spring of
2011 at [Butte College][butte].
This project is publicly hosted on [Github][gith] under the name [resistor-combinator][rcomb].

 [butte]: http://www.butte.edu
 [rcomb]: https://github.com/jmahler/resistor-combinator
 [gith]: http://github.com

Jeremiah Mahler <jmmahler@gmail.com><br>
<http://www.google.com/profiles/jmmahler#about>

COPYRIGHT
---------

Copyright &copy; 2011, Jeremiah Mahler.  All Rights Reserved.<br>
This project is free software and released under
the [GNU General Public License][gpl].

 [gpl]: http://www.gnu.org/licenses/gpl.html

