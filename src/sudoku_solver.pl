
:- module(kanar_sudoku, [solve_sudoku/1]).

% :- [domains].
% :- [problems].
:- use_module(utils).
:- use_module(domains).
:- use_module(problems).
:- use_module(library(clpfd)).



% a problem is list of lists so lets work with them like so
% steps lets work only with 9x9; the NxN should be possible to be done later on

is_squared(X) :- A is sqrt(X), Floor is floor(A), Floor =:= A.

get_subblock_size(N, R) :- A is sqrt(N), R is floor(A).

map_vars_to_domain([], _).
map_vars_to_domain(Vars, Domain) :-
    is_list(Vars),
    Vars = [H|T],
    H ins Domain,
    map_vars_to_domain(T, Domain).

blocks_inner_basic([],[],[]).
blocks_inner_basic([A1,B1,C1|T1], [A2,B2,C2|T2], [A3,B3,C3|T3]) :-
    all_distinct([A1, A2, A3, B1, B2, B3, C1, C2, C3]),
    blocks_inner_basic(T1, T2, T3).

% TODO Rework block constrainst so it works with NxN where N = sqrt(|Problem|).
% The block constraint stating that all positions in a NxN subblock must be distinct.
blocks_constraint(N, Problem) :-
    integer(N),
    Problem = [A, B, C, D, E, F, H, I, J],
    blocks_inner_basic(A, B, C),
    blocks_inner_basic(D, E, F),
    blocks_inner_basic(H, I, J).
    

solve_sudoku(Problem) :-
    length(Problem, N),
    is_squared(N),                          % <-- sudoku only works with square numbers 4, 9, 16..
    maplist(same_length(Problem), Problem),
    transpose(Problem, Transposed),
    length(Transposed, N),
    % TODO Map all variables of problem to a certain domain
    append(Problem, Vars), Vars ins 1..N,   % <-- do mapping with the Vars
    maplist(all_distinct, Problem),
    maplist(all_distinct, Transposed),
    get_subblock_size(N, Ss),
    blocks_constraint(Ss, Problem).