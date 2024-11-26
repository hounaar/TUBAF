/* rekursive Folge: y_x = y_(x-1) + (x-1) * 2; x > 0, x = 0; x == 0 */
/* x=0,y=0, x=1,y=0, x=2,y=2, x=3,y=6, x=4,y=12, x=5,y=20 */

folge(0,0) :- !.
folge(Zahl, Erg) :-
    Vor is Zahl -1,
    folge(Vor, VorErg),
    Erg is VorErg + Vor * 2.