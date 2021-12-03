/*
@author: Alexis STRAPPAZZON
*/

membre(X, [X|_]).
membre(X, [_|L]):-membre(X, L).

concat([], L2, L2).
concat([H|L1], L2, [H|L3]):-concat(L1, L2, L3).

dernier(X, [X]).
dernier(X, [_|L]):-dernier(X,L).

element(_, [], []).                     % cas de base, qu'importe ce qu'on veut supprimer, s'il n'y a rien c'est une liste vide
element(X, [X|L], R):- element(X,L,R).  % cas ou la tête de liste correspond à X, dans ce cas on continue sans la tête
element(X, [Y|L], [Y|R]):-              % cas ou la tête de liste ne correspond pas à X, on continue en prepend la tête de liste
    X \== Y,
    element(X, L, R).

inv_naif([], []).           % inversion d'une liste vide = vide
inv_naif([X|L1], L2):-      % sinon
    inv(L1, L3),            % on inverse le reste de la liste (sans la tête)
    concat(L3, [X], L2).    % et on concatène à cette liste inversée, la tête

inv_accumulateur(L1,R):- inv_accumulateur_worker(L1, [], R). % Lancement du worker avec un accumulateur vide
inv_accumulateur_worker([], R, R).              % Si la liste est vide, le résultat est l'accumulateur
inv_accumulateur_worker([H|L1], Acc, R):-       % Sinon
    inv_accumulateur_worker(L1, [H|Acc], R).    % On prepend la tête dans l'accumulateur

composante(I, X, L):-
    composante_worker(I, X, L, 1).      % Lancement du worker avec I = 1.

composante_worker(I, X, [X|_], I).      % Cas où on atteint le I-ème élément.
composante_worker(I, X, [_|L], N):- N1 is N+1, composante_worker(I, X, L, N1). % Sinon on augmente le compteur et on passe au suivant.

subsAll(_,_,[],[]).     % Si la liste est vide il n'y a pas/plus de changement à faire, donc on renvoie la liste vide.
subsAll(X,Y,[X|L1], [Y|L2]):-subsAll(X,Y,L1,L2). % Si la head correspond à X alors on remplace par Y.
subsAll(X,Y,[Z|L1], [Z|L2]):- X \== Z, subsAll(X,Y,L1, L2). % Sinon on vérifie que X est différent de Z, et on prepend par Z, puis on passe à la suite.

% Même qu'en haut, sauf qu'on fait un seul remplacement à chaque fois.
subsAll_2(_,_,[],[]).
subsAll_2(X,Y,[X|L1], [Y|L1]).
subsAll_2(X,Y,[Z|L1], [Z|L2]):- X \== Z, subsAll_2(X,Y,L1, L2).

list2ens(L1, R):-list2ens_worker(L1, [], R). % Initialisation du worker.

list2ens_worker([], R, R2):- inv_accumulateur(R,R2).    % La liste est vide on a terminé. Comme on utilise un accumulateur le résultat est "à l'envers" donc il faut le renverser.
list2ens_worker([H|L1], Acc, R):- not(member(H, Acc)), list2ens_worker(L1, [H|Acc], R). % Si H n'est pas membre de l'accumulateur alors on le rajoute dans l'accumulateur et on passe à la suite.
list2ens_worker([H|L1], Acc, R):- member(H,Acc), list2ens_worker(L1, Acc, R). % Sinon on passe juste à la suite.

ens_union([], E2, E2).  % Si E1 est vide alors l'union vaut uniquement E2.
ens_union([H|T], E2, [H|E3]):-not(member(H, E2)), ens_union(T, E2, E3). % Si H n'est pas membre de E2 on l'ajoute dans E3 et on passe à la suite.
ens_union([H|T], E2, E3):-member(H,E2), ens_union(T,E2,E3). % Si H est membre de E2 on l'ajoute pas dans E3 car il sera ajouté avec la première clause.

ens_difference([],_,[]). % Si E1 est vide, alors E3 est vide aussi.
ens_difference([H|T], E2, [H|E3]) :- not(member(H, E2)), ens_difference(T, E2, E3). % Si H n'est pas membre de E2 on l'ajoute et on passe à la suite.
ens_difference([H|T], E2, E3) :- member(H, E2), ens_difference(T, E2, E3). % Si H est membre de E2 on l'ajoute pas et on passe à la suite.

ens_intersect([], _, []). % Si E1 est vide, alors E3 est vide aussi.
ens_intersect([H|T], E2, E3):-not(member(H,E2)), ens_intersect(T, E2, E3). % Si H n'est pas membre de E2 on l'ajoute pas.
ens_intersect([H|T], E2, [H|E3]):- member(H, E2), ens_intersect(T, E2, E3).% Si H est membre de E2 on l'ajoute.

ens_equals(L1, L2):-ens_equals_worker(L1,L2,L1,L2). % Lancement du worker.

ens_equals_worker([], [], _, _). % Si les deux listes sont vides elles sont égales.
ens_equals_worker([X|L1], [Y|L2], C1, C2):- member(X, C2), member(Y, C1), ens_equals_worker(L1, L2, C1, C2). % On traverse les listes, et on regarde que chaque item est présent dans l'autre liste C1 correspond à L1 de base, de même pour C2.