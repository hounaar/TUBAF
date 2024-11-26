einfache_teile(rad).
einfache_teile(achse).
einfache_teile(motor).
einfache_teile(getriebe).
einfache_teile(kühler).
einfache_teile(lichtmaschine).
einfache_teile(sitz).
einfache_teile(karosse).
einfache_teile(armatur).

struktur(fahrwerk_hinten, [rad, rad, achse]).
struktur(fahrwerk_vorn, [rad, rad, achse]).
struktur(motorblock, [motor, kühler, lichtmaschine]).                  
struktur(antrieb, [motorblock, getriebe, fahrwerk_hinten, fahrwerk_vorn]).
struktur(fahrgastzelle, [sitz, sitz, sitz, sitz, armatur, karosse]).
struktur(auto, [fahrgastzelle, antrieb]).

teile_von(X, P) :- teile_acc(X, P, Loch), Loch=[].

teile_acc(X, [X|Loch], Loch) :- einfache_teile(X).

teile_acc(X, P, Loch) :-
    struktur(X, Restteile),
    teile_der_liste_acc(Restteile, P, Loch).

teile_der_liste_acc([], Loch, Loch).

teile_der_liste_acc([P|Rest], Alles, Loch) :-
    teile_acc(P, Alles, Loch1),
	teile_der_liste_acc(Rest, Loch1, Loch).
