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

%%% composed predicates:
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

:- initialization(main).
main:- 
    setof(X, grandparent(X, florian), L),
    write('List of solutions ?- : '),
    writeln(L),
    halt.

%%% test using the terminal:
% ?- grandparent(X,florian).
% X = pierre ;  % [press SPACE to continue]
% X = annie ;   % [press SPACE to continue]
% false.