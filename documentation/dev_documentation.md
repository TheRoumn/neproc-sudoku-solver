# **1 Sudoku-solver - developer documentation**

- Author: Roman Kaňa
- Year: 2022/2023
- Subject: [Non-procedural Programming [NPRG005]](https://is.cuni.cz/studium/predmety/index.php?id=02934e532e6af11f37ae4446899dcfec&tid=&do=predmet&kod=NPRG005)

## **1.1 Assignment**

Create a Prolog program with predicates that solve sudoku problems of size $N^2{\times}N^2$, where $N\in\mathbb{N}$.


## **1.2 Scope**

This project is thought of as developing a simple SWI-Prolog module that helps the user to solve sudoku problems of size $N^2{\times}N^2$, where $N\in\mathbb{N}$. Please if you want to use these modules as a user you can refer to the [**user documentation**](/documentation/user_documentation.md)

## **1.3 Possible TODOs**

 - Domains:
   - Find a way to use them.
 - Problems:
   - Fix the 'Predicate ... not defined' warnings. 
   - Define a predicate for getting a random NxN sudoku problem.
   - Define a predicate for automatic generation of test_block_constraint_problem with side length as parameter.
 - Sudoku_solver:
   - Find a better way of checking if a list is empty.
   - Find a way to define problem domains instead of the static 1..$N^2$.
 - Add tests for correctness and execution time into test.pl file.
  
# **2 Description**

## **2.1 The definition of the problem.**

The problem must be defined as a list of list and have size $N^2{\times}N^2$, where $N\in\mathbb{N}$ and all the inner lists should be the same length as the outer (main) list. All positions in the problem must be either natural number smaller than or equal to the chosen $N^2$ or a variable, in which case all the variables should be all diferent ( each variable address should be used at most once) so ther are no other constraints besides the *row*, *column* and *subblock*.

For using predefined problems or defining your own problems use predicates from the [problems.pl](../src/problems.pl) file as an examples with the [problem definition](#21-the-definition-of-the-problem) above.

## **2.2 File structure**

 - ### 2.2 Source files '/src' 
   Directory holding all the .pl source files 
   - **domains.pl :** Module holding the predicates for mapping variables onto a certain domain.
     - Exports: `sudoku_domain/1`, `odd_domain/1`, `even_domain/1`, `sudoku_dynamic/3` predicates.
   - **problems.pl :** Module holding getter predicates for sudoku problems.
     - Exports: 
       - `problem/2` - Standard sudoku problem getter.
       - `problem_count/1` - Defined problem count getter.
       - `test_block_constraint_problem/2` - Getter for some less filled problems for testing purposes.
       - `empty_problem/2` - Getter for empty problem of size $N^2{\times}N^2$, where $N\in\mathbb{N}$.
       - `solved_problem/2` - Returns a 9x9 solved problem, that is correctly/incorrectly solved.
   - **sudoku.pl :** Module holding a NxN sudoku solver with a labeler/printer predicate
     - Exports:
       - `solve_sudoku/1` - Sudoku problem solver.
       - `print_sudoku/1` - Sudoku problem labeler/printer that uses some default labeling options.
       - `print_sudoku/2` - Sudoku problem labeler/printer with labeling options as parameter.
   - **test.pl :**  File that was intended to run tests when consulted and based on the result of the consultation the test were successful or failed.
   - **utils.pl :**  Module holding some utility predicates for better code clarity.
     - Exports: `take/3`, `drop/3`, `split_at/4`
  
 - ### Documentation '/documentation'
   - **dev_documentation.md :** Developer documentation markdown file.
   - **user_documentation.md :** User documentation markdown file.

# **3 Requirements**

## **3.1 Software**

The user will need to have the SWI Prolog installed.

## **3.2 Additional libraries**

As far as I am concerned, I used the CLPFD library, but that come with SWI Prolog, so there should be no problem.

# **4 Contributions**

Any contributions are welcome since the project is done and might be maintained from time to time.