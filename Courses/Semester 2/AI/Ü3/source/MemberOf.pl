/* Grenzbedingung */
member(X, [X|_]).
/* Rekursionsfall */
member(X, [_|Y]) :- member(X,Y).
