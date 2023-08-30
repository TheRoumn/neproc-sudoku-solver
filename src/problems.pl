%% Module holding some prepared problems for the sudoku solver as well as some I sude for the constraint testing.
:- module(problems, [problem/2, problem_count/1, test_block_constraint_problem/2, empty_problem/2, solved_problem/2]).

%%% Problems for the sudoku solver.

%% Returns number of defined problems
problem_count(2).

%% Returns a Problem Based on the number given. The given number should be in range [1, N] where N is returned by the problem_count(N) predicate.
problem(0, Problem) :- Problem = [[_,_,_,_,_,_,_,_,_],
                                  [_,_,_,_,_,_,_,_,_],
                                  [_,_,_,_,_,_,_,_,_],
                                  [_,_,_,_,_,_,_,_,_],
                                  [_,_,_,_,_,_,_,_,_],
                                  [_,_,_,_,_,_,_,_,_],
                                  [_,_,_,_,_,_,_,_,_],
                                  [_,_,_,_,_,_,_,_,_],
                                  [_,_,_,_,_,_,_,_,_]].

problem(1, Problem) :- Problem = [[1,_,_,8,_,4,_,_,_],
                                  [_,2,_,_,_,_,4,5,6],
                                  [_,_,3,2,_,5,_,_,_],
                                  [_,_,_,4,_,_,8,_,5],
                                  [7,8,9,_,5,_,_,_,_],
                                  [_,_,_,_,_,6,2,_,3],
                                  [8,_,1,_,_,_,7,_,_],
                                  [_,_,_,1,2,3,_,8,_],
                                  [2,_,5,_,_,_,_,_,9]].

problem(2, Problem) :- Problem = [[1,_,_,8,_,4,_,_,_],
                                  [_,_,3,2,_,5,_,_,_],
                                  [_,2,_,_,_,_,4,5,6],
                                  [7,8,9,_,5,_,_,_,_],
                                  [_,_,_,_,_,6,2,_,3],
                                  [_,_,_,4,_,_,8,_,5],
                                  [_,_,_,1,2,3,_,8,_],
                                  [8,_,1,_,_,_,7,_,_],
                                  [2,_,5,_,_,_,_,_,9]].

%% Returns a solved problem that is either solved correctly or incorrectly based on the first argument.

solved_problem(correct, P) :- P =  [[1,5,6,8,9,4,3,2,7],
                                    [4,7,3,2,6,5,9,1,8],
                                    [9,2,8,7,3,1,4,5,6],
                                    [7,8,9,3,5,2,6,4,1],
                                    [5,1,4,9,8,6,2,7,3],
                                    [3,6,2,4,1,7,8,9,5],
                                    [6,9,7,1,2,3,5,8,4],
                                    [8,3,1,5,4,9,7,6,2],
                                    [2,4,5,6,7,8,1,3,9]].

solved_problem(incorrect, P) :- P =    [[1,5,6,8,9,4,3,2,7],
                                        [4,7,3,2,6,5,9,1,8],
                                        [9,2,8,7,3,1,4,5,6],
                                        [7,8,9,3,5,2,6,4,1],
                                        [5,1,4,9,8,6,2,7,3],
                                        [3,6,2,4,1,7,8,9,5],
                                        [6,9,7,1,2,3,5,8,4],
                                        [8,3,1,5,4,9,7,6,2],
                                        [9,4,5,6,7,8,1,3,2]].

%% Predicate that generates an empty problem of size N^2xN^2.

empty_problem_acc(0, _, Acc, Acc).
empty_problem_acc(N, OrigoSize, Acc, Result) :- N > 0, N1 is N - 1, length(List, OrigoSize), empty_problem_acc(N1, OrigoSize, [List | Acc], Result).

empty_problem(Size, Result) :- Size >= 0, NewSize is Size*Size, empty_problem_acc(NewSize, NewSize, [], Result).

%%% Problem for testing some constraints.

test_block_constraint_problem(2, Problem) :- Problem = [[1,_,2,_],
                                                        [_,_,_,_],
                                                        [3,_,4,_],
                                                        [_,_,_,_]].

test_block_constraint_problem(3, Problem) :- Problem = [[1,_,_,2,_,_,3,_,_],
                                                        [_,_,_,_,_,_,_,_,_],
                                                        [_,_,_,_,_,_,_,_,_],
                                                        [4,_,_,5,_,_,6,_,_],
                                                        [_,_,_,_,_,_,_,_,_],
                                                        [_,_,_,_,_,_,_,_,_],
                                                        [7,_,_,8,_,_,9,_,_],
                                                        [_,_,_,_,_,_,_,_,_],
                                                        [_,_,_,_,_,_,_,_,_]].