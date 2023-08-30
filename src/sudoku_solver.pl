%% Main module of this Project. Returns exports the solve_sudoku/1 and print_sudoku/1,2 predicates for the users.
:- module(sudoku_solver, [ solve_sudoku/1, print_sudoku/1, print_sudoku/2 ]).

% importing user modules
:- use_module(utils).
:- use_module(domains).
:- use_module(problems).
:- use_module(library(clpfd)).

%% True if N is squared. In other words the square root of N is an Integer.
is_squared(N) :- Root is sqrt(N), Floor is floor(Root), Floor =:= Root.

%% Returns the side length of a sudoku subblock. (9x9) -> (3x3) subblock, (16x16) -> (4x4) subblock
get_subblock_size(N, R) :- Root is sqrt(N), R is floor(Root).

%% Mapping mapping variables to a certain domain. 1..9, odd numbers in range 2..18, ...
map_vars_to_domain([], _).
map_vars_to_domain(Vars, Domain) :-
    is_list(Vars),
    Vars = [H|T],
    H ins Domain,
    map_vars_to_domain(T, Domain).

%% preduicate used to chunk Lists into subblock based on N.

% TODO: debug block constrait why it fails on 9x9 problem.
recursive_take_n(_, []).
recursive_take_n(N, Lists) :-
    take(N, Lists, Subblock),
    append(Subblock, Flat),
    all_distinct(Flat),
    drop(N, Lists, Rest),
    recursive_take_n(N, Rest).

% TODO: find better way to check for empty list of lists. 
%% Constrains the sudoku so the subblocks as all different numbers.
block_constraint(_, L) :- 
    append(L, Flat),
    length(Flat, 0). 
block_constraint(N, Problem) :-
    maplist(take(N), Problem, Heads),
    recursive_take_n(N, Heads),
    maplist(drop(N), Problem, Rests),
    block_constraint(N,Rests).

%% Predicate that applies all the constraint (row, column and subblock should not contain same number more than once) to the Problem given.
solve_sudoku(Problem) :-
    length(Problem, N),
    is_squared(N),                          % <-- sudoku only works with square numbers 4, 9, 16..
    maplist(same_length(Problem), Problem),
    transpose(Problem, Transposed),
    length(Transposed, N),
    % TODO Map all variables of problem to a certain domain
    append(Problem, Vars), Vars ins 1..N,   % <-- do mapping with the Vars
    maplist(all_distinct, Problem),         % <-- row constraint
    maplist(all_distinct, Transposed),      % <-- column constraint
    get_subblock_size(N, Ss),
    block_constraint(Ss, Problem).          % <-- subblock constraint

%% Labels and prints the sudoku.
print_sudoku(Problem) :- maplist(labeling([ff]), Problem), maplist(writeln, Problem).
print_sudoku(Problem, LabelingOptions) :- maplist(labeling(LabelingOptions), Problem), maplist(writeln, Problem).