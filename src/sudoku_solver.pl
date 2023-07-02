
:- module(kanar_sudoku, [solve_sudoku/1]).

:- use_module(problems).
:- use_module(library(clpfd)).

% a problem is list of lists so lets work with them like so
% steps lets work only with 9x9; the NxN should be possible to be done later on


map_vars_to_domain([], _).
map_vars_to_domain(Vars, Domain) :-
    is_list(Vars),
    Vars = [H|T],
    H in Domain,
    map_vars_to_domain(T, Domain).
    

solve_sudoku(Problem) :-
    maplist(same_length(Problem), Problem),
    transpose(Problem, Transposed),
    length(Problem, N),
    length(Transposed, N),
    maplist(all_distinct, Problem).