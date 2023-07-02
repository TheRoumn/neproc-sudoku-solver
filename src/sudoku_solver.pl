
:- module(kanar_sudoku, [solve_sudoku/1]).

:- use_module(problems).
:- use_module(library(clpfd)).

% a problem is list of lists so lets work with them like so
% steps lets work only with 9x9; the NxN should be possible to be done later on


solve_sudoku(Problem) :- 
    maplist(same_length(Problem), Problem),
    transpose(Problem, Transposed), length(Problem, N), length(Transposed, N).
    
    