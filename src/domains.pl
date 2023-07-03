:- module(domains, [sudoku_domain/1, odd_domain/1]).

:- use_module(library(clpfd)).

% static 9x9 domains

sudoku_domain(Var) :- Var #> 0, Var #< 10. % 1..9

odd_domain(Var) :- Var #>= 1, Var #=< 18, Var mod 2 #= 1.

even_domain(Var) :- Var #>= 1, Var #=< 18, Var mod 2 #= 0.
% dynamic (N^2) x (N^2) domains, where N there are N^2 subblocks of size NxN

sudoku_dynamic(Var, Min, Max) :- Var = Min..Max.