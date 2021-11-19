%%% Welcome to your first PROLOG program!
% run program: swipl filename.pl

% list of predicates:
link(paris,lyon).
link(lyon,marseille).
link(nice,marseille).
link(lyon,paris).

%%% do then print query result:
% L contains the list of cities that are solutions 
% to the query "X, link(X,marseille)"
% ?-link(X,marseille).		2 success
% ?-link(marseille,lyon).	failure
% ?-link(X,Y), lien(Y,X). 	2 success

:- initialization(main).
main:- 
bagof(X, link(X,marseille), L),
write('List of solutions ?- link(X,marseille): '),
writeln(L),
halt.


%%% notes:
% bagof: all solutions and doublons possible, if goal fails, returns nothing
% setof: like bagof, but eliminates doublons
% findall: If the goal fails, return empty list 

% read(X) == readline(X)