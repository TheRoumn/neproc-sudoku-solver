:- use_module(library(clpfd)).

problem(1, P) :- P = [[1,_,_,8,_,4,_,_,_],
                      [_,2,_,_,_,_,4,5,6],
                      [_,_,3,2,_,5,_,_,_],
                      [_,_,_,4,_,_,8,_,5],
                      [7,8,9,_,5,_,_,_,_],
                      [_,_,_,_,_,6,2,_,3],
                      [8,_,1,_,_,_,7,_,_],
                      [_,_,_,1,2,3,_,8,_],
                      [2,_,5,_,_,_,_,_,9]].


problem(2, P) :- P = [[_,_,2,_,3,_,1,_,_],
                      [_,4,_,_,_,_,_,3,_],
                      [1,_,5,_,_,_,_,8,2],
                      [_,_,_,2,_,_,6,5,_],
                      [9,_,_,_,8,7,_,_,3],
                      [_,_,_,_,4,_,_,_,_],
                      [8,_,_,_,7,_,_,_,4],
                      [_,9,3,1,_,_,_,6,_],
                      [_,_,7,_,6,_,5,_,_]].

problem(3, P) :- P = [[1,_,_,_,_,_,_,_,_],
                      [_,_,2,7,4,_,_,_,_],
                      [_,_,_,5,_,_,_,_,4],
                      [_,3,_,_,_,_,_,_,_],
                      [7,5,_,_,_,_,_,_,_],
                      [_,_,_,_,_,9,6,_,_],
                      [_,4,_,_,_,6,_,_,_],
                      [_,_,_,_,_,_,_,7,1],
                      [_,_,_,_,_,1,_,3,_]].

solve_sudoku(Problem, Solution).