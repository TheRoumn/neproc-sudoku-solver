
:- use_module(problems).
:- use_module(domains).
:- use_module(kanar_sudoku).
:- use_module(library(clpfd)).

test(N) :- integer(N), problem(N, P), solve_sudoku(P), maplist(clpfd:labeling([ff]), P), maplist(portray_clause, P).

n_tests(0) :- !.
n_tests(N) :- once(test(N)), N1 is N - 1, n_tests(N1).

all_test :- problem_count(N), n_tests(N).