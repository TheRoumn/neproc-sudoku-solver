# 1 **Sudoku solver - User documentation**

## 1.1 Description

This project should be used as a helping hand in solving sudoku like problems in **SWI-Prolog**. The main module is **kanar_sudoku**. For more information about the project module please refer to section [Modules and defined predicates](#2-modules-and-defined-predicates).

## 1.2 The definition of the problem.

The problem must be defined as a list of list and have size $N^2{\times}N^2$, where $N\in\mathbb{N}$ and all the inner lists should be the same length as the outer (main) list. All positions in the problem must be either natural number smaller than or equal to the chosen $N^2$ or a variable, in which case all the variables should be all diferent ( each variable address should be used at most once) so ther are no other constraints besides the *row*, *column* and *subblock*.

### 1.2.1 Using predefined problems

If you want to use some predefined problems use the `problems` module using ```:- use_module(problems).``` import statement, then please refer to the [Modules and defined predicates](#2-modules-and-defined-predicates) for all available getter predicates for predifined problems.

Example usage:
```Prolog
:- use_module(problems).
:- use_module(sudoku_solver, [print_sudoku/1 as print])

? - problems:problem(1, P), maplist(writeln, P).

Output:
[1,_37186,_37192,8,_37204,4,_37216,_37222,_37228]
[_37240,2,_37252,_37258,_37264,_37270,4,5,6]
[_37300,_37306,3,2,_37324,5,_37336,_37342,_37348]
[_37360,_37366,_37372,4,_37384,_37390,8,_37402,5]
[7,8,9,_37438,5,_37450,_37456,_37462,_37468]
[_37480,_37486,_37492,_37498,_37504,6,2,_37522,3]
[8,_37546,1,_37558,_37564,_37570,7,_37582,_37588]
[_37600,_37606,_37612,1,2,3,_37636,8,_37648]
[2,_37666,5,_37678,_37684,_37690,_37696,_37702,9]
P = ...
```

or 

```Prolog
:- use_module(problems).
:- use_module(sudoku_solver, [print_sudoku/1 as print])

? - problems:solved_problem(correct, P), maplist(writeln, P).

Output:
[1,5,6,8,9,4,3,2,7]
[4,7,3,2,6,5,9,1,8]
[9,2,8,7,3,1,4,5,6]
[7,8,9,3,5,2,6,4,1]
[5,1,4,9,8,6,2,7,3]
[3,6,2,4,1,7,8,9,5]
[6,9,7,1,2,3,5,8,4]
[8,3,1,5,4,9,7,6,2]
[2,4,5,6,7,8,1,3,9]
P = ...
```


### 1.2.2 Defining your own problems

If you want to define your own problems please follow the [problem definition](#12-the-definition-of-the-problem).

So basiclly create a list of list where:
  - size of the outer list is a squared number ( number that has a integer square root ) so problems has size 1x1, 4x4, 9x9, 16x16 etc. 
  - all inner lists have the same length as the outer list
  - all positions in the inner lists are either:
    - number from range [1, length of the outer list]
    - variable with a unique address compared to all the other variables in the problem

Example of a **good** problems:
```Prolog
% consistant 4x4, all variables different, all numbers in range
P1 = [[_,_,_,1],
      [_,_,1,_],
      [_,1,_,_],
      [1,_,_,_]].

% consistant 9x9, all variables different, all numbers in range
P2 = [[1,_,_,_,_,_,_,_,_]
      [_,2,_,_,_,_,_,_,_]
      [_,_,3,_,_,_,_,_,_]
      [_,_,_,4,_,_,_,_,_]
      [_,_,_,_,5,_,_,_,_]
      [_,_,_,_,_,6,_,_,_]
      [_,_,_,_,_,_,7,_,_]
      [_,_,_,_,_,_,_,8,_]
      [_,_,_,_,_,_,_,_,9]].
```

Examples of **bad** problems:
```Prolog
% non-consistant shape, should be 4x4, is 4x?
P1 = [[_,_,_,_],
      [_,_],
      [_,_,_,],
      [_]].
      
% bad number range, 4x4 -> range [1, 4]
P2 = [[_,-1,_,5],
      [_,_,_,_],
      [_,_,_,6],
      [_,19,_,_]].

% binding all the free variable to the address _3000.
P3 = [[1, _3000, _3000, _3000], 
      [_3000, _3000, 1, _3000], 
      [_3000, 1, _3000, _3000], 
      [_3000, _3000, _3000, 1]].
```

## 2 Modules and defined predicates

All module are defined in corresponding `.pl` files.

 - **domains :** Module holding the predicates for binding variables  ( *+Vars* ) onto a certain domain.
   - Exports: 
     - `sudoku_domain(+Vars)` - Basic sudoku tile domain (number from 1 to 9).
     - `odd_domain(+Vars)` - Odd domain of number 2 to 18.
     - `even_domain(+Vars)` - Even domain of number 2 to 18. 
     - `sudoku_dynamic(+Vars, +Min, +Max)` - Dynamic sudoku domain of numbers from *+Min* to *+Max* and *Max - Min* should be large enough range.
 - **problems :** Module holding getter predicates for sudoku problems.
   - Exports: 
     - `problem_count(-N)` - Defined problem count getter. N is the count of defined problems.
     - `problem(+N, -P)` - Standard sudoku problem getter. When N smaller or equal to the defined problem count, then a problem is returned.
     - `test_block_constraint_problem(+N, -P)` - Getter for some less filled problems for testing purposes.
     - `empty_problem(+N, -P)` - Getter for an empty problem of size $N^2{\times}N^2$, where $N\in\mathbb{N}$. 
     - `solved_problem(+Correctness, _P)` - Returns a 9x9 solved problem *-P*, that is correctly/incorrectly solved based on *+Correctness* $\in$ \{*correct*, *incorrect*\}.
 - **sudoku_solver :** Module holding a NxN sudoku solver with a labeler/printer predicate
   - Exports:
     - `solve_sudoku(+P)` - Sudoku problem solver, that applies (Row, Column and Subblock) constraints to the *+P* problem variables.
     - `print_sudoku(+P)` - Sudoku problem *+P* labeler/printer that uses some default labeling options.
     - `print_sudoku(+P, +Ops)` - Sudoku problem *+P* labeler/printer with labeling options *+Ops* as parameter.

## 3 Usage

### 3.1 Installation and importing

Import can be done by using:

```Prolog
:- use_module(domains).
:- use_module(problems).
:- use_module(kanar_sudoku).
```

or you can import specific predicate using the use_module/2 predicate

```Prolog
:- use_module(domains, [sudoku_domain/1]).
:- use_module(problems, [problem/2 as sudoku_problem, problem_count/1]).
:- use_module(kanar_sudoku, [sudoku_solver/1 as solver, print_sudoku/1 as printer]).
```
### 3.2 Example usage

Taking that you already imported the module above the example query is then you can use the solve_sudoku/2 predicate in two ways:

- As a solver for an unsolved problem:

```Prolog
? - problem_count(C), N = 1, C >= N, problem(N, Problem), sudoku_solver(Problem), print_sudoku(Problem).
[1,5,6,8,9,4,3,2,7]
[9,2,8,7,3,1,4,5,6]
[4,7,3,2,6,5,9,1,8]
[3,6,2,4,1,7,8,9,5]
[7,8,9,3,5,2,6,4,1]
[5,1,4,9,8,6,2,7,3]
[8,3,1,5,4,9,7,6,2]
[6,9,7,1,2,3,5,8,4]
[2,4,5,6,7,8,1,3,9]
P = ...
```

- Or as a solution checker for your solution:
  - which either prints the solution and successfully exits the predicate

```Prolog
? - problems:solved_problem(correct,Solution), solve_sudoku(Solution), print_sudoku(Solution).
[1,5,6,8,9,4,3,2,7]
[9,2,8,7,3,1,4,5,6]
[4,7,3,2,6,5,9,1,8]
[3,6,2,4,1,7,8,9,5]
[7,8,9,3,5,2,6,4,1]
[5,1,4,9,8,6,2,7,3]
[8,3,1,5,4,9,7,6,2]
[6,9,7,1,2,3,5,8,4]
[2,4,5,6,7,8,1,3,9]
P = ...
```
  - Or if is unable to apply any of the constraints, fails.
```Prolog
? - problems:solved_problem(incorrect,Solution), solve_sudoku(Solution), print_sudoku(Solution).
false.
```