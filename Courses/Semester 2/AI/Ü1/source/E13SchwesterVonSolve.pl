männlich(anakin).
männlich(luke).
weiblich(padme).
weiblich(leia).
eltern(luke, anakin, padme).
eltern(leia, anakin, padme).
diff(X,Y) :- X\=Y.

schwester_von(X, Y) :-
    weiblich(X),
    eltern(X, M, V),
    eltern(Y, M, V),
    diff(X,Y).

