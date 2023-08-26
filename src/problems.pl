%% Module holding some prepared problems for the sudoku solver as well as some I sude for the constraint testing.
:- module(problems, [problem/2, problem_count/1, test_block_constraint_problem/2]).

%%% Problems for the sudoku solver.

%% Returns numbber of defined problems
problem_count(4).

%% Returns a Problem Based on the number given. The given number should be in range [1, N] where N is returned by the problem_count(N) predicate.
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
                                  [_,2,_,_,_,_,4,5,6],
                                  [_,_,_,4,_,_,8,_,5],
                                  [_,_,_,_,_,6,2,_,3],
                                  [8,_,1,_,_,_,7,_,_],
                                  [_,_,3,2,_,5,_,_,_],
                                  [7,8,9,_,5,_,_,_,_],
                                  [_,_,_,1,2,3,_,8,_],
                                  [2,_,5,_,_,_,_,_,9]].

problem(3, Problem) :- Problem = [[_,_,2,_,3,_,1,_,_],
                                  [_,4,_,_,_,_,_,3,_],
                                  [_,_,_,2,_,_,6,5,_],
                                  [1,_,5,_,_,_,_,8,2],
                                  [9,_,_,_,8,7,_,_,3],
                                  [_,9,3,1,_,_,_,6,_],
                                  [8,_,_,_,7,_,_,_,4],
                                  [_,_,_,_,4,_,_,_,_],
                                  [_,_,7,_,6,_,5,_,_]].

problem(4, Problem) :- Problem = [[1,_,_,_,_,_,_,_,_],
                                    [_,_,_,_,_,_,_,7,1],
                                    [_,_,2,7,4,_,_,_,_],
                                    [7,5,_,_,_,_,_,_,_],
                                    [_,3,_,_,_,_,_,_,_],
                                    [_,_,_,5,_,_,_,_,4],
                                    [_,4,_,_,_,6,_,_,_],
                                    [_,_,_,_,_,9,6,_,_],
                                    [_,_,_,_,_,1,_,3,_]].

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