%%%%% Working with lists in PROLOG
% Tips: 
% * Don't think in terms of imperative code,
%   think in terms of declarative code.
% * Build predicates from success to other predicates.
% * Query testing only possible with success, so
%   keep in mind to negate failure when a query should fail.
% @author: 0nyr 

%%% conditional predicates:
/* X member of a list ?

Check: Step by step:

1. We are given member(3, [1,2,3])
Check:
    member(X,[X|_]),
    member(X,[_|L])
We have member(3,[1|...]) since _ means anything, so we 
can continue to evalute member(3,[2,3]) meaning we have
removed the first element of the list.

2. Check:
    member(X,[X|_]),
    member(X,[_|L])
We have member(3,[2|...]) since _ means anything, so we
can continue to evalute member(3,[3]).

3. Check:
    member(X,[X|_]),
    member(X,[_|L])
We have member(3,[3|...]) since _ means anything, so we
have member(X,[X|_]) which is true. Return.
*/
member(X,[X|_]). 
member(X,[_|L]) :- member(X,L).

/* concat if L3 is the concatenation of L1 and L2

Example: Step by step:

1. We are given concat([1,2], [3,4], [1,2,3,4]).
Check:
    concat([],L,L),
    concat([X|L1],L2,[X|L3])
We have concat([1|...], [...], [1|...]), so we have
concat([X|L1],L2,[X|L3]). We need to continue to evaluate
concat(L1, L2, L3), which means we have efectively eliminated
the first element of L1. We have to continue to evaluate.

2. Check:
    concat([],L,L),
    concat([X|L1],L2,[X|L3])
We have concat([2|...], [...], [2|...]), so we have
concat([X|L1],L2,[X|L3]). We need to continue to evaluate
concat([], [3,4], [3,4]).

2. Check:
    concat([],L,L),
    concat([X|L1],L2,[X|L3])
We have concat([], [3,4], [3,4]). So we have concat([],L,L)
which is always true. Return.
*/
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

/* The following code is not correct:

list_deprived_of_all_occurences_of_element(X,L,RestOfL) :-
    %not(member(X, RestOfL)), list_deprived_of_element(X,L,RestOfL)
    % goal: remove first occurence of X from L as long as 
    % it is possible to do so
    list_deprived_of_element(X,L,RestOfL),
    fail.
*/

/* Start testing 3rd clause or 2nd clause and continue evaluating
those 2 clauses (one of the 2 must be true) until only the 1st clause 
remain (when there are only the empty list remaining). */

/* Start: PROLOG want's to verify list_deprived_of_all_occurences_of_element,
to do so, it needs to check element(X,L,RestOfL).

Remark: 
* _ is any variable,
* [] is empty list
* A :- B means If A, is B true ?, 
    if B is false, don't have A
    if B is true, A true.

Check to apply 2nd, 3rd and 4th.
1st: wrapper for element(X,L,RestOfL) clauses.

2nd: always true for empty lists as second and third param.

3rd: if second param is [X,...], continue to evaluate element(X,L,RestOfL),
which means we just removed the first occurence of X from L.

4th: if the two given lists as second and third element start
with a same element Z, Z must be different from X, then continue
to evaluate element(X, L, RestOfL), effectively removing
Z from the evaluation pipeline.

Example: Step by step evaluation:

1. We have list_deprived_of_all_occurences_of_element(3,[1,3,2,3],[1,2]),
so we need to evaluate element(3,[1,3,2,3],[1,2]) until it is true.

2. Check if any of the following clauses is true:
    element(_,[],[]), 
    element(X,[X|L],RestOfL), 
    element(X,[Z|L],[Z|RestOfL])
We have actually that element(3,[1,3,2,3],[1,2]) is
element(3,[1|...],[1...]) so we have element(X,[Z|L],[Z|RestOfL]),
and not(X==Z) is true, so continue to evaluate element(3,[3,2,3],[2]).

3. Check if any of the following clauses is true:
    element(_,[],[]), 
    element(X,[X|L],RestOfL), 
    element(X,[Z|L],[Z|RestOfL])
We have element(3,[3|...],[2]) so we have element(X,[X|L],RestOfL).
Then we need to continue to evaluate element(3,[2,3],[2]).

4. Check if any of the following clauses is true:
    element(_,[],[]), 
    element(X,[X|L],RestOfL), 
    element(X,[Z|L],[Z|RestOfL])
We have element(3,[2|...],[2|...]) and not(3==2) is true.
So we need to continue to evaluate element(3,[3],[]).

5. Check if any of the following clauses is true:
    element(_,[],[]), 
    element(X,[X|L],RestOfL), 
    element(X,[Z|L],[Z|RestOfL])
We have element(3,[3|...],[]) so we have element(X,[X|L],RestOfL).
Then we need to continue to evaluate element(3,[],[]).

6. Check if any of the following clauses is true:
    element(_,[],[]), 
    element(X,[X|L],RestOfL), 
    element(X,[Z|L],[Z|RestOfL])
Eventually we have element(3,[],[]) so we have element(_,[],[]).
There is not more things to evaluate, so we have true. Return.
*/

list_deprived_of_all_occurences_of_element(X,L,RestOfL) :- 
    element(X,L,RestOfL).

element(_,[],[]).
element(X,[X|L],RestOfL) :- element(X,L,RestOfL).
element(X,[Z|L],[Z|RestOfL]) :- not(X==Z), element(X, L, RestOfL).


/* Inversion of list 

Remark: PROLOG distinguished variables from lists.

?- concat(3,[1,2],[3,1,2]).
false.

?- concat([3],[1,2],[3,1,2]).
true.

So beware, here, don't do concat(L2,X,ConcatResultL),
do concat(L2,[X],ConcatResultL).

Example of broken code:

inv([],[]).
inv([X|L1],[L2|X]) :- inv(L1,L2).

This won't works since [L2|X] means L2 first element of
the list, and X is all the rest... this is not what we want.
We wanted L2 all the rest, X last element ! This can help how the 
[A|B] works in prolog.
*/

/*inv([], L).
inv([X|L1],L2) :- 
    concat(L2,[X],ConcatResultL),
    inv(L1,ConcatResultL).*/
    
/*inv([],[]).
inv([X|L1], L2) :- inv(L1, InvL1), concat(InvL1,[X], L2).
*/

inv([],[]).
inv([X|L1], L2) :- inv(L1, InvL1), concat(InvL1,[X], L2).


component_worker(Index,Index,X,[X|_]).

component_worker(Index,Counter,X,[_|L]) :- 
    IncrementedCounter is Counter+1,
    component_worker(Index,IncrementedCounter,X,L).

% This clause is creating an alias without Counter
component(Index,X,L) :- component_worker(Index,1,X,L).


/* Wrong code:
% subsAll if L2 is L1 with all occurences of X replaced by Y
subsAll(X,Y,[],L2).
subsAll(X,Y,[X|L1],L2) :- 
    concat(L2,[Y],ConcatL2), 
    subsAll(X,Y,L1,ConcatL2).


subsOnce(X,Y,L1,L2) :- subsOnce_worker(X,Y,0,L1,L2).
subsOnce_worker(X,Y,_,[],L2).
subsOnce_worker(X,Y,IsAlreadySub,[Z|L1],L2) :-
    not(X==Z), 
    not(IsAlreadySub == 1), 
    concat(L2,[Z],ConcatL2),
    subsOnce_worker(X,Y,0,L1,ConcatL2).
subsOnce_worker(X,Y,IsAlreadySub,[X|L1],L2) :- 
    not(IsAlreadySub == 1),
    concat(L2,[Y],ConcatL2),
    subsOnce_worker(X,Y,1,L1,ConcatL2).
*/

/* How to find clause:
1. What do I want the clause to return ?
2. Base case
3. Stop case.
4. Special case with empty values or lists.
5. Do we need an intermediate clause ?

Understand that PROLOG goes on until it finds a true clause.
Then it backtracks and returns the result:

?- trace(subsAll).
%         subsAll/4: [all]
true.

?- subsAll(a,x,[a,b,c,d,a,a,d], L).
 T [10] Call: subsAll(a, x, [a, b, c, d, a, a, d], _28756)
 T [19] Call: subsAll(a, x, [b, c, d, a, a, d], _30062)
 T [28] Call: subsAll(a, x, [c, d, a, a, d], _30958)
 T [37] Call: subsAll(a, x, [d, a, a, d], _31866)
 T [46] Call: subsAll(a, x, [a, a, d], _722)
 T [55] Call: subsAll(a, x, [a, d], _1630)
 T [64] Call: subsAll(a, x, [d], _2526)
 T [73] Call: subsAll(a, x, [], _3422)
 T [73] Exit: subsAll(a, x, [], [])
 T [64] Exit: subsAll(a, x, [d], [d])
 T [55] Exit: subsAll(a, x, [a, d], [x, d])
 T [46] Exit: subsAll(a, x, [a, a, d], [x, x, d])
 T [37] Exit: subsAll(a, x, [d, a, a, d], [d, x, x, d])
 T [28] Exit: subsAll(a, x, [c, d, a, a, d], [c, d, x, x, d])
 T [19] Exit: subsAll(a, x, [b, c, d, a, a, d], [b, c, d, x, x, d])
 T [10] Exit: subsAll(a, x, [a, b, c, d, a, a, d], [x, b, c, d, x, x, d])
L = [x, b, c, d, x, x, d] .
*/
subsAll(_,_,[],[]).
subsAll(X,Y,[X|L1],[Y|L2]) :- 
    subsAll(X,Y,L1,L2).
subsAll(X,Y,[Z|L1],[Z|L2]) :- 
    not(X==Z),
    subsAll(X,Y,L1,L2).

subsOnce(_,_,[],[]).
subsOnce(X,Y,[X|L1],[Y|L1]). % WARN: No L2 here !
subsOnce(X,Y,[Z|L1],[Z|L2]) :- 
    not(X==Z), subsOnce(X,Y,L1,L2).

% L3, union of L1 and L2
/*
ens_union([],[],_).
ens_union([X|L1],[X|L2],L3) :-
    member(X,L3),
    ens_union(L1,L2,L3).
ens_union([X|L1],[Y|L2],L3]) :-
    not(X==Y),
    member(X,L3),
    member(Y,L3),
    ens_union(L1,L2,L3). 
ens_union([],[Y|L2],L3) :-
    membre(Y,L3),
    ens_union(L1,L2,L3).
ens_union([X|L1],[],L3) :-
    membre(X,L3),
    ens_union(L1,L2,L3).


list2ens([],[]).
list2ens([],_). % WRONG !!!
list2ens([X|L1],[X|L2]) :- 
    not(member(X,L2)),
    list2ens(L1,L2).
list2ens([X|L1],L2) :- 
    member(X,L2),
    list2ens(L1,L2).




*/

% returns a set out of a list (remove diplicates)
list_as_set([],_).
list_as_set([X|L1],SetLWithoutX) :- 
    not(member(X,SetLWithoutX)),
    concat(SetLWithoutX,[X],SetL),
    list_as_set(L1,SetL).
list_as_set([X|L1],SetLWithX) :- 
    member(X,SetLWithX),
    list_as_set(L1,SetLWithX).

list_as_set([],_) :-



/*
ens_intersect([],[],_).
ens_intersect([],[Y|L2],L3) :- member(Y,L3), ens_intersect([],L2,L3).
ens_intersect([X|L1],L2,L3]) :- member(X,L2), ens_intersect(L1,L2,L3).*/


/*inv_accumulateur(L1,R):- inv_accumulateur_worker(L1, [], R). % Lancement du worker avec un accumulateur vide
inv_accumulateur_worker([], R, R).              % Si la liste est vide, le résultat est l'accumulateur
inv_accumulateur_worker([H|L1], Acc, R):-       % Sinon
    inv_accumulateur_worker(L1, [H|Acc], R).    % On prepend la tête dans l'accumulateur

list2ens(L1, R):-list2ens_worker(L1, [], R). % Initialisation du worker.

list2ens_worker([], R, R2):- inv_accumulateur(R,R2).    % La liste est vide on a terminé. Comme on utilise un accumulateur le résultat est "à l'envers" donc il faut le renverser.
list2ens_worker([H|L1], Acc, R):- not(member(H, Acc)), list2ens_worker(L1, [H|Acc], R). % Si H n'est pas membre de l'accumulateur alors on le rajoute dans l'accumulateur et on passe à la suite.
list2ens_worker([H|L1], Acc, R):- member(H,Acc), list2ens_worker(L1, Acc, R). % Sinon on passe juste à la suite.
*/


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

/* The following code is equivalent to 

test_list_deprived_of_element_5 :-
    % source of ideas: http://www.aistudy.com/program/prolog/visual_prolog/Repetitive%20Processes.htm
    list_deprived_of_element(X, [1,2,1,3], R),
    write('list_deprived_of_element(X, [1,2,1,3], R) ?- : '),
    write("X = "), write(X),
    write(", R = "), writeln(R). 

WARN: putting ln instead of write(R), ln. will return false.
*/
test_list_deprived_of_element_5 :-
    % source of ideas: http://www.aistudy.com/program/prolog/visual_prolog/Repetitive%20Processes.htm
    list_deprived_of_element(X, [1,2,1,3], R),
    write('list_deprived_of_element(X, [1,2,1,3], R) ?- : '),
    write("X = "), write(X),
    write(", R = "), write(R),
    nl, fail.
test_list_deprived_of_element_5. % need to repeat the clause

test_list_deprived_of_all_occurences_of_element :-
    list_deprived_of_all_occurences_of_element(3, [1,2,3,2,3,3,1], [1,2,2,1]),
    writeln("list_deprived_of_element(3, [1,2,3,2,3,3,1], [1,2,2,1]) ?- success as expected").

test_list_inversion_1 :-
    inv([1,2], [2,1]),
    writeln("inv([1,2], [2,1]) ?- success as expected").

test_list_inversion_2 :-
    inv([1,2,1,3,2,4],[4,2,3,1,2,1]),
    writeln("inv([1,2,1,3,2,4],[4,2,3,1,2,1]) ?- success as expected").

test_list_inversion_3 :-
    inv([1,2,3,4], L),
    write("inv([1,2,3,4], L) ?- "),
    write("L = "), writeln(L).

/* The following doesn"t works */
test_list_inversion_4 :-
    inv(L, [4,3,2,1]),
    write("inv(L, [4,3,2,1]) ?- "),
    write("L = "), writeln(L). 

test_list_inversion_5 :-
    inv(L, [4,3,X,1]),
    write("inv(L, [4,3,X,1]) ?- "),
    write("L = "), writeln(L).

test_element_at_index_in_list_1 :-
    component(3,X,[a,b,c,d]),
    write("component(2,X,[a,b,c,d]) ?- "),
    write("X = "), writeln(X).

test_substitution_once_1 :-
    subsOnce(a,x,[a,b,c,d], L),
    write("subsOnce(a,x,[a,b,c,d], L) ?- "),
    write("L = "), writeln(L).

test_substitution_all_1 :-
    subsAll(a,x,[a,b,c,d,a,a,d], L),
    write("subsAll(a,x,[a,b,c,d,a,a,d], L) ?- "),
    write("L = "), writeln(L).

test_subslist_contained_in_list_1 :-
    list2ens([a,b,c,b,a],[a,b,c]),
    writeln("list2ens([a,b,c,b,a],[a,b,c]) ?- success as expected").

test_subslist_contained_in_list_2 :-
    list2ens([a,b,c,b,a],L),
    write("list2ens([a,b,c,b,a],L) ?- "),
    write("L = "), writeln(L).

test_list_as_set_1 :-
    list_as_set([a,b,c,a,b,d],L),
    write("list_as_set([a,b,c,a,b,d],L) ?- "),
    write("L = "), writeln(L).

test_list_as_set_2 :-
    list_as_set([a,b,c,a,b,d],L),
    writeln("list_as_set([a,b,c,c,a],[a,b,c]) ?- success as expected").

test_union_of_sets_as_lists_1 :- 
    ens_union([a,b,c,d],[d,e,f],L),
    write("list2ens([a,b,c,d],[d,e,f],L) ?- "),
    write("L = "), writeln(L).

test_intersection_of_sets_as_lists_1 :-
    ens_intersect([a,b,c,d],[b,d,e,f],L),
    write("ens_intersect([a,b,c,d],[b,d,e,f],L) ?- "),
    write("L = "), writeln(L).


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
    test_list_deprived_of_all_occurences_of_element,
    test_list_inversion_1,
    test_list_inversion_2,
    test_list_inversion_3,
    test_list_inversion_4,
    test_list_inversion_5,
    test_element_at_index_in_list_1,
    test_substitution_once_1,
    test_substitution_all_1,
    test_subslist_contained_in_list_1,
    test_subslist_contained_in_list_2,
    test_list_as_set_1,
    test_list_as_set_2,
    test_union_of_sets_as_lists_1,
    test_intersection_of_sets_as_lists_1, 
    halt.