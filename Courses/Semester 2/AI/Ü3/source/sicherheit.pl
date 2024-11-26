hatSicherheitsfreigabe(mulder, 'FBI').
hatSicherheitsfreigabe(mulder, 'Raucher'). /* wird nie genutzt */
hatSicherheitsfreigabe(scully, 'FBI').

/* Zugänge */
sichererZugang(area51).
sichererZugang(campDavid).
einfacherZugang(bastei).
einfacherZugang(zwinger).
erweiterterZugang(X) :- einfacherZugang(X).
erweiterterZugang(X) :- sichererZugang(X).

/* gibt die Orte 'Wo' an die Person 'Wer' betreten kann */
sicherheitsüberprüfung(Wer, Wo) :-
    hatSicherheitsfreigabe(Wer, _),
    !,
    erweiterterZugang(Wo).

sicherheitsüberprüfung(_, Wo) :-
    einfacherZugang(Wo).