anzahl_der_eltern(adam, 0) :- !.
anzahl_der_eltern(eva, 0) :- !.
anzahl_der_eltern(_, 2).

anzahl_der_eltern2(adam, N) :- !, N=0.
anzahl_der_eltern2(eva, N) :- !, N=0.
anzahl_der_eltern2(_, 2).

anzahl_der_eltern3(adam, 0).
anzahl_der_eltern3(eva, 0).
anzahl_der_eltern3(X, 2) :- \+(X=adam), \+(X=eva).