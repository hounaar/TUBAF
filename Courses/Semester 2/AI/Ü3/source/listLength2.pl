listen_länge(L, N) :- ll_acc(L, 0, N).

ll_acc([], A, A).
ll_acc([K|R], A, N) :- A1 is A + 1, ll_acc(R, A1, N).
