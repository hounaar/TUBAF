test(12).
test(11.0).
test(7.22).
test(3.141e2).
test(42).

test(hilfe).
test(ein).
test(atom).
test(true).
test(false).

test(resolution(hunger, essen)).
test(resolution(hunger, essen)).
test(resolution(müde, schlafen)).

vergleich(Input):- 
    test(X),
    (   
    	(   
    		number(Input), number(X),
        	(   
    			(X=:=Input, write(X), write(" Even "), write(Input));
    			(X>Input, write(X), write(" Bigger "), write(Input));
    			(X<Input, write(X), write(" Lower "), write(Input))
        	)
    	);
    	(   
    		X=Input, write(X), write(" Gleich "), write(Input)
    	)
    ).
    