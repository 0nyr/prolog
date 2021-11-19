%%%%%% Program taken from http://www.aistudy.com/program/prolog/visual_prolog/Repetitive%20Processes.htm
% this simple PROLOG program demonstrates backtracking
% I have modified the program for testing it in a single command
% run: swipl <filename.pl>

%%% Predicates:
% nondeterministic country(symbol)
% print_countries

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
