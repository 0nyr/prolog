%%%%% Working with lists in PROLOG
% Tips: 
% * Don't think in terms of imperative declaration,
%   think in terms of declarative declaration.
% * Build predicates from success to other predicates.
% * Query testing only possible with success, so
%   keep in mind to negate failure when a query should fail.
% @author: 0nyr 

%%% conditional predicates:
member(X,[X|_]). 
member(X,[_|L]) :- member(X,L).

% concat if L3 is the concatenation of L1 and L2
concat([],L,L). 
concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).

has_following_element(X,L) :- member(X,L), member(Y,L), concat(_,[X,Y|_],L).
last_element(X, L) :- 
    % member of list with not following element
    member(X,L), not(has_following_element(X,L)).
has_preceding_element(X,L) :- member(X,L), member(Y,L), concat(_,[Y,X|_],L).
first_element(X, L) :-
    % member of list with no preceding element
    member(X,L), not(has_preceding_element(X,L)).

%%% queries:
test_membership :-
    member(pierre,[annie,olivier,pierre,veronique]),
    % displays only if preceding clause was evaluated as true
    writeln("member(pierre,[annie,olivier,pierre,veronique]) ?- success as expected").

test_get_all_members :-
    setof(X, member(X, [annie,olivier,pierre,veronique]), L),
    write('member(X, [annie,olivier,pierre,veronique]) ?- : '),
    writeln(L).

test_concat :-
    concat([1,2,3],[4,5,6],[1,2,3,4,5,6]),
    % displays only if preceding clause was evaluated as true
    writeln("concat([1,2,3],[4,5,6],[1,2,3,4,5,6]) ?- success as expected").

test_has_following_element_1 :-
    has_following_element(3, [1,2,3,4]),
    writeln("has_following_element(3, [1,2,3,4]) ?- success as expected").

test_has_following_element_2 :-
    not(has_following_element(3, [1,2,3])),
    writeln("has_following_element(3, [1,2,3]) ?- failure as expected").

test_last_element_1 :-
    last_element(3, [1,2,3]),
    writeln("last_element(3, [1,2,3]) ?- success as expected").

test_last_element_2 :-
    not(last_element(3, [1,2,3,4])),
    writeln("last_element(3, [1,2,3,4]) ?- failure as expected").

test_get_last_element :-
    setof(X, last_element(X, [1,2,3]), L),
    write('last_element(X, [1,2,3]) ?- : '),
    writeln(L).

test_has_preceding_element_1 :-
    has_preceding_element(3, [1,2,3]),
    writeln("has_preceding_element(3, [1,2,3]) ?- success as expected").

test_has_preceding_element_2 :-
    not(has_preceding_element(1, [1,2,3])),
    writeln("has_preceding_element(1, [1,2,3]) ?- failure as expected").

test_first_element_1 :-
    first_element(1, [1,2,3]),
    writeln("first_element(1, [1,2,3]) ?- success as expected").

test_first_element_2 :-
    not(first_element(3, [1,2,3])),
    writeln("first_element(3, [1,2,3]) ?- failure as expected").

%%% run:
:- initialization(main).
main:- 
    test_membership,
    test_get_all_members,
    test_concat,
    test_has_following_element_1,
    test_has_following_element_2,
    test_last_element_1,
    test_last_element_2,
    test_has_preceding_element_1,
    test_has_preceding_element_2,
    test_first_element_1,
    test_first_element_2,
    halt.