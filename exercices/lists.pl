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

list_deprived_of_element(X,L,RestOfL) :- 
    % RestOfL is the list L with no occurence of X
    % predicate 1: find a list L1 such that the list L is the 
    %   concatenation of L1 and L2 with an external element X
    % predicate 2: RestOfL is the concatenation of L1 and L2
    concat(L1,[X|L2],L), concat(L1,L2,RestOfL).

list_deprived_of_all_occurences_of_element(X,L,RestOfL) :-
    %not(member(X, RestOfL)), list_deprived_of_element(X,L,RestOfL)
    % goal: remove first occurence of X from L as long as 
    % it is possible to do so
    list_deprived_of_element(X,L,RestOfL),
    fail.


%%% clauses used to test the predicates (queries):
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

test_list_deprived_of_element_1 :-
    list_deprived_of_element(3, [1,2,3,4], [1,2,4]),
    writeln("list_deprived_of_element(3, [1,2,3,4], [1,2,4]) ?- success as expected").

test_list_deprived_of_element_2 :-
    not(list_deprived_of_element(3, [1,2,3], [1,2,3])),
    writeln("list_deprived_of_element(3, [1,2,3], [1,2,3]) ?- failure as expected").

test_list_deprived_of_element_3 :-
    % WARN: This test does not work as expected
    % because the predicate list_deprived_of_element
    % is only capable of removing the first occurence of X
    list_deprived_of_element(3, [1,2,3,2,3,3,1], [1,2,2,1]),
    writeln("list_deprived_of_element(3, [1,2,3,2,3,3,1], [1,2,2,1]) ?- success as expected").

test_list_deprived_of_element_4 :-
    bagof(R, list_deprived_of_element(2, [1,2,1,3], R), QueryResults),
    write('list_deprived_of_element(2, [1,2,1,3], R) ?- : '),
    writeln(QueryResults).

test_list_deprived_of_element_5 :-
    % source of ideas: http://www.aistudy.com/program/prolog/visual_prolog/Repetitive%20Processes.htm
    list_deprived_of_element(X, [1,2,1,3], R),
    write('list_deprived_of_element(X, [1,2,1,3], R) ?- : '),
    write("X = "), write(X),
    write(", R = "), write(R),
    nl, fail.
test_list_deprived_of_element_5. % need to repeat the clause

test_list_deprived_of_all_occurences_of_element_1 :-
    list_deprived_of_element(3, [1,2,3,2,3,3,1], [1,2,2,1]),
    writeln("list_deprived_of_element(3, [1,2,3,2,3,3,1], [1,2,2,1]) ?- success as expected").


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
    test_list_deprived_of_element_1,
    test_list_deprived_of_element_2,
    test_list_deprived_of_element_4,
    test_list_deprived_of_element_5,
    test_list_deprived_of_all_occurences_of_element_1,
    halt.