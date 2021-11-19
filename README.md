# Prolog

### Useful links

##### docu

[Learn Prolog Now! guide](https://lpn.swi-prolog.org/lpnpage.php?pageid=top)

##### tools and online compilers

[Online code IDE for prolog | SWISH](https://swish.swi-prolog.org/)

[Online Prolog IDE | JDOODLE](https://www.jdoodle.com/execute-prolog-online/)

##### examples

[GitHub repo with lot of PROLOG examples](https://github.com/Anniepoo/prolog-examples)

## Install PROLOG

The following describes the installation process of PROLOG for Ubuntu:

#### Method 1: Using the official PPA

The full installation processes can be found [on the official website](https://www.swi-prolog.org/download/stable).

Installing the stable release using debian-style packages:

```shell
sudo apt-get install software-properties-common
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt-get update
sudo apt-get install swi-prolog
```

#### Method 2: Installing through `snap`:

Keep in mind `snap` is different from working with debian packages.

```shell
snap install swi-prolog		# install stable release
```

## My own prolog query system

The usual process to work with PROLOG is described [here | StackOverflow](https://stackoverflow.com/questions/19399022/how-to-run-prolog-code). This process involves writing predicates in a file, then loading the file while running the PROLOG interpreter to then be able to evaluate predicates. It means going through 5 steps each time you want to modify the base file predicates...

To simplify this process to running a single command to the terminal, the idea is to build query testing predicates that I have called `test_<name of tested predicate>`. This contains predicates being evaluated as well as `writeln` statements to keep track of what happens and what results are expected. Example:

```prolog
test_ancestors :- 
    setof(X, ancestor(X, florian), L),
    write('ancestor(X, florian) ?- : '),
    writeln(L).
```

> NOTE: Keep in mind that those query can only work by evaluation their predicates successfully from first to last one. So convert expected fails to not(fail) == success for the program to be executed normally. Example:

```prolog
test_has_following_element_2 :-
    not(has_following_element(3, [1,2,3])),
    writeln("has_following_element(3, [1,2,3]) ?- failure as expected").
```

> WARN: Keep in mind to call the query in the main query being executed at file interpretation:

```
%%% run:
:- initialization(main).
main:- 
    test_grandparent,
    test_ancestors,
    test_list_of_ancestors_1,
    test_list_of_ancestors_2,
    halt.
```

Run and execute directly in one command `swipl <filepath.pl>` in a terminal:

```shell
(base)  ❮ onyr ★  kenzae❯ ❮ alia❯❯ swipl exercices/lists.pl 
member(pierre,[annie,olivier,pierre,veronique]) ?- success as expected
member(X, [annie,olivier,pierre,veronique]) ?- : [annie,olivier,pierre,veronique]
concat([1,2,3],[4,5,6],[1,2,3,4,5,6]) ?- success as expected
has_following_element(3, [1,2,3,4]) ?- success as expected
has_following_element(3, [1,2,3]) ?- failure as expected
last_element(3, [1,2,3]) ?- success as expected
last_element(3, [1,2,3,4]) ?- failure as expected
has_preceding_element(3, [1,2,3]) ?- success as expected
has_preceding_element(1, [1,2,3]) ?- failure as expected
first_element(1, [1,2,3]) ?- success as expected
first_element(3, [1,2,3]) ?- failure as expected
```
