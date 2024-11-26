ersetze(you, i).
ersetze(are, [am, not]).
ersetze(X, X).

verändere([],[]).
verändere([K|R], [X|Y]) :- ersetze(K,X), verändere(R,Y).