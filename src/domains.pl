:- module(domains, [sudoku_domain/1, odd_domain/1]).

:- use_module(library(clpfd)).

% static 9x9 domains

sudoku_domain(X) :- X #> 0, X #< 10. % 1..9

odd_domain(X) :- X #>= 1, X #=< 18, X mod 2 #= 1.

even_domain(X) :- X #>= 1, X #=< 18, X mod 2 #= 0.
% dynamic (N^2) x (N^2) domains, where N there are N^2 subblocks of size NxN

sudoku_dynamic(X, Min, Max) :- X ins Min..Max.