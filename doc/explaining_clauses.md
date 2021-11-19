# Explaining clauses

### Useful links

[Study Prolog | aistudy.com](http://www.aistudy.com/program/prolog/prolog.htm)



## Understanding how to use clauses

All the text is taken from [aistudy.com](http://www.aistudy.com/program/prolog/visual_prolog/Repetitive%20Processes.htm), writen probably by **[Alain Colmerauer](https://en.wikipedia.org/wiki/Alain_Colmerauer)** or Phillipe Roussel (no author given in the page). Find the text [here](http://www.aistudy.com/program/prolog/visual_prolog/Repetitive%20Processes.htm).



### Repetitive Processes

Pascal, BASIC, or C programmers who start using Visual Prolog are often dismayed to find that the language has no FOR, WHILE, or REPEAT statements. There is no direct way to express iteration. Prolog allows only two kinds of repetition--backtracking, in which it searches for multiple solutions in a single query, and recursion, in which a procedure calls itself.

As it turns out, this lack doesn't restrict the power of the Prolog language. In fact, Visual Prolog recognizes a special case of recursion--called tail recursion --and compiles it into an iterative loop in machine language. This means that although the program logic is expressed recursively, the compiled code is as efficient as it would be in Pascal or BASIC.

In this section, we explore the art of writing repetitive processes in Prolog. As you'll see, recursion is--in most cases--clearer, more logical, and less error-prone than the loops that conventional languages use. Before delving into recursion, however, take another look at backtracking.

1. Backtracking Revisited

When a procedure backtracks, it looks for another solution to a goal that has already been satisfied. It does this by retreating to the most recent subgoal that has an untried alternative, using that alternative, then moving forward again. You can exploit backtracking as a way to perform repetitive processes.

Example

Program ch06e01.pro demonstrates how to use backtracking to perform repetitive processes--it prints all solutions to a query.

Program: *backtracking.pl*

```prolog
%%% Clauses:
country("England").
country("France").
country("Germany").
country("Denmark").

% The first clause for print_countries prints all the solutions; 
% its second clause simply terminates the goal successfully 
% because the first clause always fails.
print_countries :-
    country(X),
    write(X),   % write the value of X
    nl,         % newline
    % used to backtrack, ask for another solution until no more 
    % solutions are found (and so utimately fails)
    fail.   
print_countries.

%%% run main:
:- initialization(main).
main:- 
    print_countries,
    halt.
```

The predicate country simply lists the names of various countries, so that a goal such as

```prolog
country(X)
```

has multiple solutions. The predicate print_countries then prints out all of these solutions. It is defined as follows:

```prolog
print_countries :-
    country(X), write(X), nl, fail.

print_countries.
```

The first clause says:

"To print countries, find a solution to country(X), then write X and start a new line, then fail."

In this case, "fail" means:

"assume that a solution to the original goal has not been reached, so back up and look for an alternative."

The built-in predicate fail always fails, but you could equally well force backtracking by using any other goal that would always fail, such as 5=2+2 or country(shangri_la).

The first time through, X is bound to england, which is printed. Then, when it hits fail, the computer backs up. There are no alternative ways to satisfy nl or write(X), so the computer looks for a different solution to country(X).

The last time country(X) was executed, it bound a value to the previously free variable X. So, before retrying this step, the computer unbinds X (frees it). Then it can look for an alternative solution for country(X) and bind X to a different value. If it succeeds, processing goes forward again and the name of another country is printed.

Eventually, the first clause runs out of alternatives. The only hope then is to try another clause for the same predicate. Sure enough, execution falls through to the second clause, which succeeds without doing anything further. In this way the goal print_countries terminates with success. Its complete output is:

```prolog
england
france
germany
denmark
yes
```

Or in a real terminal:

```prolog
(base)  ❮ onyr ★  kenzae❯ ❮ prolog❯❯ swipl exercices/backtracking.pl 
England
France
Germany
Denmark
```

If the second clause were not there, the print_countries goal would terminate with failure, and the final message would be no. Apart from that, the output would be the same.
