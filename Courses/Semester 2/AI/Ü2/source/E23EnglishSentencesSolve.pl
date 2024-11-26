einfaches_wort(apple).
einfaches_wort(plum).
einfaches_wort(kumquad).
einfaches_wort(the).
einfaches_wort(a).
einfaches_wort(eat).
einfaches_wort(throw).
einfaches_wort(plant).
einfaches_wort(i).
einfaches_wort(you).
einfaches_wort(we).
einfaches_wort(ravenous).
einfaches_wort(lazy).

syntax(ppronomen, [i]).
syntax(ppronomen, [you]).
syntax(ppronomen, [we]).

syntax(adjektiv, [ravenous]).
syntax(adjektiv, [lazy]).

syntax(verb, [eat]).
syntax(verb, [throw]).
syntax(verb, [plant]).

syntax(substantiv, [apple]).
syntax(substantiv, [plum]).
syntax(substantiv, [kumquad]).

syntax(artikel, [the]).
syntax(artikel, [a]).

syntax(substantiv_phrase, [artikel, substantiv]).
syntax(substantiv_phrase, [substantiv]).

syntax(verb_phrase, [verb, adjektiv]).
syntax(verb_phrase, [verb]).

syntax(satz, [ppronomen, verb_phrase, substantiv_phrase]).

worte_aus(X, [X]) :-
    einfaches_wort(X).

worte_aus(X, P) :- 
    syntax(X, Worte),
    hole_alle_worte_aus(Worte, P).

hole_alle_worte_aus([], []).

hole_alle_worte_aus([H|Rest], Satz) :-
    worte_aus(H, Kopfworte),
    hole_alle_worte_aus(Rest, Restworte),
    append(Kopfworte, Restworte, Satz).
