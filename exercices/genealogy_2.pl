%%%%% Genealogy queries with PROLOG
% @author: 0nyr 
% This file is my solution to the classical 
% genealogy problem.

%%% predicates:
:- discontiguous male/1.
:- discontiguous female/1.
:- discontiguous parent/2.

parent(olivier, florian).
parent(olivier, jean).
parent(olivier, matthieu).
parent(olivier, mathilde).

parent(veronique, florian).
parent(veronique, jean).
parent(veronique, matthieu).
parent(veronique, mathilde).

male(olivier).
male(florian).
male(jean).
male(matthieu).

female(veronique).
female(mathilde).

parent(pierre, veronique).
parent(pierre, sylvie).
parent(pierre, marie).
parent(pierre, arnaud).

parent(annie, veronique).
parent(annie, sylvie).
parent(annie, marie).
parent(annie, arnaud).

male(pierre).
male(arnaud).

female(annie).
female(sylvie).
female(marie).
female(marie).

parent(marie, clement).
parent(marie, florence).
parent(marie, helene).

male(clement).
female(florence).
female(helene).

%%% conditional predicates:
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- 
    parent(X, Someone), ancestor(Someone, Y).

recurse([H|T]) :-
	writeln(H),
	recurse(T).

list_of_ancestors(L, X) :- setof(Y, ancestor(Y, X), L).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y) :- sibling(X, Y), female(X).
ommer(X, Y) :- sibling(P, X), parent(P, Y). % ommer ... Non-standard genderqueer term for "aunt/uncle"
aunt(X, Y) :- ommer(X, Y), female(X).
uncle(X, Y) :- ommer(X, Y), male(X).

cousin(X, Y) :- 
    grandparent(Z, X), grandparent(Z, Y), 
    not(sibling(X, Y)), X \= Y.

%%% queries:
test_grandparent :-
    setof(X, grandparent(X, florian), L),
    write('grandparent(X, florian) ?- : '),
    writeln(L).

test_ancestors :- 
    setof(X, ancestor(X, florian), L),
    write('ancestor(X, florian) ?- : '),
    writeln(L).

test_recursion :-
    writeln('recurse(List) ?- : '),
    recurse([annie,olivier,pierre,veronique]).

test_list_of_ancestors_1 :-
    list_of_ancestors([annie,olivier,pierre,veronique],florian),
    % displays only if preceding clause was evaluated as true
    writeln("list_of_ancestors([annie,olivier,pierre,veronique],florian) ?- success as expected"). 

test_list_of_ancestors_2 :-
    not(list_of_ancestors([annie,olivier,veronique],florian)),
    % displays only if preceding clause was evaluated as false
    writeln("list_of_ancestors([annie,olivier,veronique],florian) ?- failure as expected").

test_siblings :- 
    setof(X, sibling(X, florian), L),
    write('sibling(X, florian) ?- : '),
    writeln(L).

test_cousins :- 
    setof(X, cousin(X, florian), L),
    write('cousin(X, florian) ?- : '),
    writeln(L).

test_brothers :- 
    setof(X, brother(X, florian), L),
    write('brother(X, florian) ?- : '),
    writeln(L).

test_sisters :- 
    setof(X, sister(X, florian), L),
    write('sister(X, florian) ?- : '),
    writeln(L).

test_ommer :- 
    setof(X, ommer(X, florian), L),
    write('ommer(X, florian) ?- : '),
    writeln(L).

test_aunts :- 
    setof(X, aunt(X, florian), L),
    write('aunt(X, florian) ?- : '),
    writeln(L).

test_uncles :- 
    setof(X, uncle(X, florian), L),
    write('uncle(X, florian) ?- : '),
    writeln(L).

%%% run:
:- initialization(main).
main:- 
    test_grandparent,
    test_ancestors,
    test_list_of_ancestors_1,
    test_list_of_ancestors_2,
    test_siblings,
    test_cousins,
    test_brothers,
    test_sisters,
    test_ommer,
    test_aunts,
    test_uncles,
    halt.
