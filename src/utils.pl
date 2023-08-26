%% Module holding used utilities in the sudoku solver.
:- module(utils, [ take/3, drop/3 ]).


% take first N
take(0, _, []) :- !.
take(N, [H|T1], [H|T2]) :- 
    N > 0,
    N1 is N - 1,
    take(N1, T1, T2).

% drop first N elements,
drop(0, R, R) :- !.
drop(N, [_|T], Rest) :-
    N > 0,
    N1 is N - 1,
    drop(N1, T, Rest).

% Split list into two lists, one with N first elements and the other with Rest
split_at(0, T, [], T) :- !.
split_at(N, [H|T1], [H|T2], T3) :- 
    N >= 0,
    N1 is N - 1,
    split_at(N1, T1, T2, T3).