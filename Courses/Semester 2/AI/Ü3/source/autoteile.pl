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

teile_von(X,[X]) :- einfache_teile(X).

teile_von(X, P) :-
    struktur(X, Teile),
    teile_der_liste(Teile, P).

teile_der_liste([],[]).

teile_der_liste([P|Rest], Alles) :-
    teile_von(P, Kopfteile),
    teile_der_liste(Rest, Restteile),
    append(Kopfteile, Restteile, Alles).
