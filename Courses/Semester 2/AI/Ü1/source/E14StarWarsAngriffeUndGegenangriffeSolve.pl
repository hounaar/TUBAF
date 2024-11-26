gestohlen(jyn, todesstern, pläne). /*wer, von was, was*/
gestohlen(han, jabba, geld).
mitglied(rebellen, jyn).
mitglied(rebellen, han).
mitglied(imperium, vader).
schiffe(han, 1).
schiffe(rebellen, 135).
schiffe(imperium, 10000).
angriff(imperium, todesstern, alderaan). /* wer, mit was, wo */
angriff(imperium, todesstern, yavin_5).
angriff(rebellen, xwing, eadu).
hauptquartier(rebellen, yavin_5). /*wer, wo*/
hauptquartier(imperium, coruscant).

gegenangriff(Y, X) :-
    mitglied(X, Wer),
    gestohlen(Wer, Y, pläne),
    schiffe(X, _),
    angriff(_, Y,HQ),
    hauptquartier(X, HQ).

