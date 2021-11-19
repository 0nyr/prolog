% this program just displays hello world then exits
% run: swipl hello_world.pl

:- initialization(main).
main :- write('Hello world \n'), 
write('___________\n'),
halt. % halt the program
