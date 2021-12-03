# PROLOG





#### Prolog distinguishes variables from lists

```shell
?- concat(3,[1,2],[3,1,2]).
false.

?- concat([3],[1,2],[3,1,2]).
true.
```



#### clause evaluation must be true to be evaluated and kept.

To be evaluated as true, a clause can just be defined as true:

```shell
test_list_deprived_of_element_5 :-
    % source of ideas: http://www.aistudy.com/program/prolog/visual_prolog/Repetitive%20Processes.htm
    list_deprived_of_element(X, [1,2,1,3], R),
    write('list_deprived_of_element(X, [1,2,1,3], R) ?- : '),
    write("X = "), write(X),
    write(", R = "), write(R),
    nl, fail.
test_list_deprived_of_element_5. % need to repeat the clause
```

Which is equivalent to:

```shell
test_list_deprived_of_element_5 :-
    % source of ideas: http://www.aistudy.com/program/prolog/visual_prolog/Repetitive%20Processes.htm
    list_deprived_of_element(X, [1,2,1,3], R),
    write('list_deprived_of_element(X, [1,2,1,3], R) ?- : '),
    write("X = "), write(X),
    write(", R = "), writeln(R).
```

> This means that ending a clause with the commande newline: `ln` is evaluating to false if nothing is fiven after.
