%% Module holding useful domains for the sudoku solving. Probably will not work how I intend it to.
:- module(domains, [sudoku_domain/1, odd_domain/1, even_domain/1, sudoku_dynamic/3]).

:- use_module(library(clpfd)).


% static 9x9 domains
sudoku_domain(Var) :- Var #> 0, Var #< 10. % 1..9

% static 9x9 domain with only odd numbers.
odd_domain(Var) :- Var #>= 1, Var #=< 18, Var mod 2 #= 1.

% static 9x9 domain with only odd numbers.
even_domain(Var) :- Var #>= 1, Var #=< 18, Var mod 2 #= 0.

% dynamic (N^2) x (N^2) domains, where N there are N^2 subblocks of size NxN
sudoku_dynamic(Var, Min, Max) :- Var = Min..Max.