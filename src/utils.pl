:- module(utils, []).

take(0, _, []) :- !.
take(N, [H|T1], [H|T2]) :- 
    N > 0,
    N1 is N - 1,
    take(N1, T1, T2).

drop(0, R, R).
drop(N, [H|T], Rest) :-
    N > 0,
    N1 is N - 1,
    drop(N1, T, Rest).



