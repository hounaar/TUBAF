listen_länge([], 0).
listen_länge([K|R], N) :- listen_länge(R, N1), N is N1 + 1.
