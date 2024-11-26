zeile(b(X,Y,Z,_,_,_,_,_,_),X,Y,Z).
zeile(b(_,_,_,X,Y,Z,_,_,_),X,Y,Z).
zeile(b(_,_,_,_,_,_,X,Y,Z),X,Y,Z).
zeile(b(X,_,_,Y,_,_,Z,_,_),X,Y,Z).
zeile(b(_,X,_,_,Y,_,_,Z,_),X,Y,Z).
zeile(b(_,_,X,_,_,Y,_,_,Z),X,Y,Z).
zeile(b(X,_,_,_,Y,_,_,_,Z),X,Y,Z).
zeile(b(_,_,X,_,Y,_,Z,_,_),X,Y,Z).

/* Gegner x ist kurz vor dem gewinnen */
bedrohung(' ', x, x).
bedrohung(x, ' ', x).
bedrohung(x, x, ' ').

/* Zug um zu verhindern das x gewinnt */
erzwungener_zug(Brett) :-
    zeile(Brett, X, Y, Z),
    bedrohung(X, Y, Z),
    !.