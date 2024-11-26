/* Programm soll eine Folge y=2*x aller natürlichen Zahlen (inklusive 0) bis zu einer beim Start angegebenen Zahl berechnen und sie in einer Liste ablegen */
/* Die Werte in der Liste werden in absteigender Reihenfolge abgelegt! */

folge(Zahl, Folge) :- Zahl < 1, Folge=[].

folge(Zahl, [Erg, Y]) :-
    Vor = Zahl -1,
    folge(Vor, Erg),
    Y = Zahl * 2.