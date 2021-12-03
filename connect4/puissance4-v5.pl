/*
Novembre - Décembre 2021 - INSA Lyon, 4IF

Projet réalisé par l'Hexanôme 4121 : 
	Amina BEN-SALEM, 
	Romain GALLE, 
	Thibaud PIRES, 
	Florian RASCOUSSIER, 
	Alexis STRAPPAZZON, 
	Florie VILLEGAS, 
	Salomé VOLTZ

Ce programme permet de faire jouer une AI contre une autre IA avec différentes heuristiques.
*/

% Permet de trouver le maximum (3eme parametre: valeur de retour)
max(X,Y,Y) :- Y>X, !.
max(X,_,X).

% Permet d'ajouter un élément en fin de liste (3eme parametre :valeur de retour)
ajoutFin(X,[],[X]).
ajoutFin(X,[Y|L1],[Y|L2]):- ajoutFin(X,L1,L2).


% Renvoie le dernier élément de la liste (2eme paramètre : valeur de retour)
finListe([], _).
finListe(List, Elem):- last(List,Elem).


%renvoie une sous-liste à partir d'une liste L
/* Paramètres : S sous-liste, L liste */
prefix(P,L):-append(P,_,L).

sublist(S,L):-prefix(S,L).
sublist(S,[_|T]):-sublist(S,T).


% Fonction qui retourne la longueur d'une liste
/* Paramètres : L liste, N longueur de la liste */
longueur([],0).
longueur([_|L],N):- longueur(L,N1), N is N1+1.


% Fonction qui renvoie le nième élément d'une liste
/* Paramètres : I index de l'élément qu'on veut récupérer, L liste, X élément retourné */
nthElem(I, L, []):- longueur(L, I1), I1 < I.
nthElem(I, L, X):- nth1(I, L, X).


% Fonction qui enregistre un coup joué dans la grille
/* Paramètres : N numéro de la colonne dans laquelle Joueur joue, G grille, Joueur joueur, G' nouvelle grille 
enregistrerCoup(colonne, grille, joueur, nouvelle grille, grille) 
TODO : virer le deuxième paramètre grille qui sert à rien*/
enregistrerCoup(1, [L|_], x, _, I):- longueur(L,N), N >= 6, write('Coup Invalide\n'), jouerCoupX(I).
enregistrerCoup(1, [L|_], o, _, I):- longueur(L,N), N >= 6, write('Coup Invalide\n'), jouerCoupO(I).
enregistrerCoup(1, [L|G], Joueur, F,_):- longueur(L,N), N < 6, ajoutFin(Joueur,L,M), F=[M|G].
enregistrerCoup(N, _, x, _, I):- N > 7, write('Coup Invalide\n'), jouerCoupX(I).
enregistrerCoup(N, _, o, _, I):- N > 7, write('Coup Invalide\n'), jouerCoupO(I).
enregistrerCoup(N, [T|X], Joueur, [T|G], I):-	N > 0,
										N1 is N-1,
										enregistrerCoup(N1, X, Joueur, G, I).

/*enregistrerCoupJoueur(colonne, grille, joueur, )*/
enregistrerCoupJoueur(1, [L|_], x, _, I):- longueur(L,N), N >= 6, write('Coup Invalide\n'), jouerCoupJoueur(I).
enregistrerCoupJoueur(N, _, x, _, I):- N > 7, write('Coup Invalide\n'), jouerCoupJoueur(I).
enregistrerCoupJoueur(N, [T|X], Joueur, [T|G], I):-	N > 0, N1 is N-1, enregistrerCoupJoueur(N1, X, Joueur, G, I).
enregistrerCoupJoueur(1, [L|G], Joueur, F,o, _):- longueur(L,N), N < 6, ajoutFin(Joueur,L,M), F=[M|G].

/* enregistrerCoupIA(colonne, Grille, joueur, nouvelle grille) */
enregistrerCoupIA(1, [L|G], Joueur, F):- longueur(L,N), N < 6, ajoutFin(Joueur,L,M), F=[M|G].
enregistrerCoupIA(N, [T|X], Joueur, [T|G]):- N > 0, N1 is N-1, enregistrerCoupIA(N1, X, Joueur, G).

% Condition de victoire verticale : 4 jetons les uns après les autres sur une même colonne
/* Paramètres : G grille, Joueur joueur */
finJeuVert([L|_],Joueur):- sublist([Joueur,Joueur,Joueur,Joueur], L),!.
finJeuVert([_|G],Joueur):- finJeuVert(G,Joueur).

% Condition de victoire horizontale : 4 jetons les uns après les autres sur une même ligne
/* Paramètres : N numéro de la ligne à partir duquel on traite, G grille, J joueur */
finJeuHor(N, G, Joueur):- maplist(nthElem(N), G, L),
					 sublist([Joueur,Joueur,Joueur,Joueur],L),!.
finJeuHor(N, G, Joueur):- N > 0,
					 N1 is N-1,
					 finJeuHor(N1, G, Joueur).

finJeuHor(G,Joueur):- finJeuHor(6, G, Joueur).

uneFinDiag(_,D,Joueur,0):- sublist([Joueur,Joueur,Joueur,Joueur],D).
uneFinDiag(G,D,Joueur,N):- N > 0,
					  maplist(nthElem(N), G, L),
					  nthElem(N,L,E),
					  N1 is N-1,
					  uneFinDiag(G,[E|D],Joueur,N1).

uneFinDiag(G,Joueur):- uneFinDiag(G,[],Joueur,6).

autreFinDiag(_,D,Joueur,0):- sublist([Joueur,Joueur,Joueur,Joueur],D).
autreFinDiag(G,D,Joueur,N):- N > 0,
					    maplist(nthElem(N), G, L),
						N2 is 7-N,
						nthElem(N2,L,E),
					    N1 is N-1,
					    autreFinDiag(G,[E|D],Joueur,N1).

autreFinDiag(G,Joueur):- autreFinDiag(G,[],Joueur,6).


finJeuDiag(_,_,X,Joueur):- autreFinDiag(X,Joueur),!.
finJeuDiag(_,_,X,Joueur):- uneFinDiag(X,Joueur),!.
finJeuDiag(G,N,X,Joueur):- N < 7,
					  maplist(nthElem(N), G, L),
					  N1 is N+1,
					  finJeuDiag(G,N1,[L|X],Joueur).

finJeuDiag(G,Joueur):- finJeuDiag(G,1,[],Joueur).


% Diagonale vers la droite, en haut
checkVictoireDiagForwardUp(Grille, Joueur, NumColonne, NumLigne, 1, TotalLigne) :- 
	NumColonne =< 7, NumLigne =< 6,
	nthElem(NumColonne, Grille, Colonne), 
    nthElem(NumLigne, Colonne, Case),
	Case = Joueur, TotalLigne is 1.

checkVictoireDiagForwardUp(Grille, Joueur, NumColonne, NumLigne, 1, TotalLigne) :- 
    TotalLigne is 0.  %  cas où on sort de la zone de jeu

checkVictoireDiagForwardUp(Grille, Joueur, NumColonne, NumLigne, LongueurRestante, TotalLigne) :- 
	NumColonne =< 7, NumLigne =< 6,
	nthElem(NumColonne, Grille, Colonne), nthElem(NumLigne, Colonne, Case), 
	Case = Joueur, LongueurRestante2 is LongueurRestante - 1, 
	NumCol2 is NumColonne + 1, NumLigne2 is NumLigne + 1,
	checkVictoireDiagForwardUp(Grille, Joueur, NumCol2, NumLigne2, LongueurRestante2, TotalLigne2),
    TotalLigne is TotalLigne2 + 1.


% Diagonale vers la droite, en bas
checkVictoireDiagForwardDown(Grille, Joueur, NumColonne, NumLigne, 1, TotalLigne) :- 
	NumColonne =< 7, NumLigne =< 6,
	nthElem(NumColonne, Grille, Colonne), nthElem(NumLigne, Colonne, Case), 
	Case = Joueur, TotalLigne is 1.

checkVictoireDiagForwardDown(Grille, Joueur, NumColonne, NumLigne, 1, TotalLigne) :- 
    TotalLigne is 0.  % cas où on sort de la zone de jeu

checkVictoireDiagForwardDown(Grille, Joueur, NumColonne, NumLigne, LongueurRestante, TotalLigne) :- 
	NumColonne =< 7, NumLigne =< 6,
	nthElem(NumColonne, Grille, Colonne), nthElem(NumLigne, Colonne, Case), 
	Case = Joueur, LongueurRestante2 is LongueurRestante - 1, 
	NumCol2 is NumColonne + 1, NumLigne2 is NumLigne - 1,
	checkVictoireDiagForwardDown(Grille, Joueur, NumCol2, NumLigne2, LongueurRestante2, TotalLigne2),
    TotalLigne is TotalLigne2 + 1.

% pas besoin des versions à gauche, comme on va parcourir tout l'espace donc il y aura un overlap entre les deux versions

% cas où on a atteint le bout de la ligne (NumLigne = 7) => colonne suivante, retour à la ligne
checkVictoireJoueur(Grille, Joueur, Ligne, NumColonne) :- 
    Ligne >= 6, NumCol2 is NumColonne + 1,
	checkVictoireJoueur(Grille, Joueur, 1, NumCol2).
	

checkVictoireJoueur(Grille, Joueur, NumLigne, NumColonne) :- 
    % write("col="), write(NumColonne), write(", ln="), write(NumLigne), write("\n"),
    NumLigne < 7,
	NumColonne =< 7,  % condition d'arrêt
    (
      % ou bien on trouve une victoire en diagonale en haut à droite
      ( checkVictoireDiagForwardUp(Grille, Joueur, NumColonne, NumLigne, 4, Ligne), Ligne >= 4 ) ;
    
      % ou bien on trouve une victoire en diagonale en bas à droite
	  ( checkVictoireDiagForwardDown(Grille, Joueur, NumColonne, NumLigne, 4, Ligne), Ligne >= 4 ) ;
    
      % ou bien on continue d'explorer
      ( NumLigne2 is NumLigne + 1, checkVictoireJoueur(Grille, Joueur, NumLigne2, NumColonne) )
    ).


% Permet de savoir s'il reste de la place en hauteur sur une colonne N,
% avec une grille G, L une colonne, E l'espace restant
%espaceRestant(N : numéro colonne,[L|X] : Grille, E : espace restant dans la colonne, L : colonne n°N)

espaceRestant(1, [L|_], E, L):- longueur(L,N2), N3 is 6-N2, E=N3.
espaceRestant(N, [_|X], E, L):- N > 0, N1 is N-1, espaceRestant(N1, X, E, L).

sommeEspaceRestant([], 0).
sommeEspaceRestant([Colonne|GrilleReste], Somme) :- longueur(Colonne, Long), sommeEspaceRestant(GrilleReste, Somme2), Somme is (Somme2 + 6 - Long).

sommeEspaceRempli([], 0).
sommeEspaceRempli([Col|Reste], Espace) :- longueur(Col, Longueur), sommeEspaceRempli(Reste, Somme), Espace is Longueur + Somme.

tableauRempli(Grille) :- sommeEspaceRestant(Grille, 0).

% Définition et test des conditions de fin de jeu
/* Paramètres : G grille, Joueur joueur */
victoire(G, x):- finJeuVert(G,x).
victoire(G, o):- finJeuVert(G,o).
victoire(G, x):- finJeuHor(G,x).
victoire(G, o):- finJeuHor(G,o).
victoire(Grille, Joueur) :- checkVictoireJoueur(Grille, Joueur, 1, 1).


egalite(G) :- tableauRempli(G), 
		      not(victoire(G, x)), not(victoire(G, o)),
			  write('Égalité !\n').


finJeu(G) :- victoire(G,_),!.
finJeu(G) :- egalite(G),!.

% finJeu(Grille, _) :- sommeEspaceRestant(Grille, Somme), write(Somme), write('\n'), Somme=0. %, finJeu(GrilleReste, Joueur).
% finJeu([Col|Greste], Joueur) :- longueur(Col, 6), finJeu(Greste, Joueur).

% Affichage du gagnant
/* Paramètres : J joueur */
printGagnant(J):-write('Le Joueur '), write(J), write(' a gagne !\n'),!.


/* Paramètres : G grille*/
jouerCoupX(G):- victoire(G,J), printGagnant(J),!.
jouerCoupX(G):- write('Joueur x, entrez un numero de colonne : '),
				read(N), enregistrerCoup(N,G, x, X, G),
				afficherGrille(X),
				write('\n'),
				jouerCoupO(X).

jouerCoupO(G):- victoire(G,J), printGagnant(J),!.
jouerCoupO(G):- write('Joueur o, entrez un numero de colonne : '),
				read(N), enregistrerCoup(N,G, o, X, G),
				afficherGrille(X),
				write('\n'),
				jouerCoupX(X).

% Lancement du jeu : grille de départ de 6x7 (vide). C'est le joueur 'o' qui commence, suivi par x, jusqu'à ce que l'un des deux gagne [ou GRILLE PLEINE]
jouer:- jouerCoupO([[],[],[],[],[],[],[]]).

%Un coup gagant est un coup qui mene à un état de jeu ou le joueur est vainqueur
coupGagnant(1,G,J):- enregistrerCoupIA(1,G,J,N), victoire(N,J).
coupGagnant(2,G,J):- enregistrerCoupIA(2,G,J,N), victoire(N,J).
coupGagnant(3,G,J):- enregistrerCoupIA(3,G,J,N), victoire(N,J).
coupGagnant(4,G,J):- enregistrerCoupIA(4,G,J,N), victoire(N,J).
coupGagnant(5,G,J):- enregistrerCoupIA(5,G,J,N), victoire(N,J).
coupGagnant(6,G,J):- enregistrerCoupIA(6,G,J,N), victoire(N,J).
coupGagnant(7,G,J):- enregistrerCoupIA(7,G,J,N), victoire(N,J).

%Un coup perdant est un coup qui permet à l'adversaire de gagner
coupPerdantIA(1,G, Joueur):- autreJoueur(Joueur, Joueur2), enregistrerCoupIA(1,G,Joueur,N), coupGagnant(_,N,Joueur2).
coupPerdantIA(2,G, Joueur):- autreJoueur(Joueur, Joueur2), enregistrerCoupIA(2,G,Joueur,N), coupGagnant(_,N,Joueur2).
coupPerdantIA(3,G, Joueur):- autreJoueur(Joueur, Joueur2), enregistrerCoupIA(3,G,Joueur,N), coupGagnant(_,N,Joueur2).
coupPerdantIA(4,G, Joueur):- autreJoueur(Joueur, Joueur2), enregistrerCoupIA(4,G,Joueur,N), coupGagnant(_,N,Joueur2).
coupPerdantIA(5,G, Joueur):- autreJoueur(Joueur, Joueur2), enregistrerCoupIA(5,G,Joueur,N), coupGagnant(_,N,Joueur2).
coupPerdantIA(6,G, Joueur):- autreJoueur(Joueur, Joueur2), enregistrerCoupIA(6,G,Joueur,N), coupGagnant(_,N,Joueur2).
coupPerdantIA(7,G, Joueur):- autreJoueur(Joueur, Joueur2), enregistrerCoupIA(7,G,Joueur,N), coupGagnant(_,N,Joueur2).


jouerCoupJoueur(G) :- victoire(G,J), printGagnant(J),!.

%Si un coup permet a l'adversaire de gagner on se défend(coup défensif).

autreJoueur(x, o).
autreJoueur(o, x).

% jouerIA(Grille, _) :- victoire(Grille, _),!.
jouerIA(Grille, o) :- victoire(Grille,o),!, printGagnant(o), b_getval(nbVicO, TmpO), TmpO2 is TmpO + 1, b_setval(nbVicO, TmpO2),afficherNbCoups,!.
jouerIA(Grille, x) :- victoire(Grille,x),!, printGagnant(x), b_getval(nbVicX, TmpX), TmpX2 is TmpX + 1, b_setval(nbVicX, TmpX2),afficherNbCoups,!.
jouerIA(Grille, _) :- egalite(Grille),!, b_getval(egalites, Eg), Eg2 is Eg + 1, b_setval(egalites, Eg2),!.


%Si un coup permet de gagner il faut le jouer.
jouerIA(Grille, Joueur):- autreJoueur(Joueur, J2), coupGagnant(Coup,Grille,Joueur), enregistrerCoupIA(Coup,Grille,Joueur,NouvelleGrille),
			   afficherGrille(NouvelleGrille),
			   write('joueur : '),
 			   write(Joueur),
			   write('\n'),!,
				augmenterNbCoups,
			   jouerIA(NouvelleGrille, J2),!.




%Si un coup permet à l'autre joueur de gagner il faut l'empêcher (= le jouer).
jouerIA(Grille, Joueur):- autreJoueur(Joueur, J2), coupGagnant(Coup,Grille,J2), enregistrerCoupIA(Coup,Grille,Joueur,NouvelleGrille),
			   afficherGrille(NouvelleGrille),
			   write('joueur : ' ),
			   write(Joueur),
			   write('\n'),!,
				augmenterNbCoups,
			   jouerIA(NouvelleGrille, J2),!.



/*jouerIA(C, G, Joueur):- enregistrerCoupIA(C,G,o,X,G),
			    afficherGrille(X),
			    write('\n'),
			    jouerCoupJoueur(X).
*/

/*

		CODE DES HEURISTIQUES EN DESSOUS

*/


/* Ces prédicats permettent de réaliser l'heuristique "JOUER À DROITE" */
/*
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 1), espaceRestant(7,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(7,Grille, Joueur)), jouerIA(7,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 2), espaceRestant(6,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(6,Grille, Joueur)), jouerIA(6,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 3), espaceRestant(5,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(5,Grille, Joueur)), jouerIA(5,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 4), espaceRestant(4,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(4,Grille, Joueur)), jouerIA(4,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 5), espaceRestant(3,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(3,Grille, Joueur)), jouerIA(3,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 6), espaceRestant(2,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(2,Grille, Joueur)), jouerIA(2,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 7), espaceRestant(1,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(1,Grille, Joueur)), jouerIA(1,Grille, Joueur),!.

%Sinon jouer au plus près du centre quand même.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 1), jouerIA(7,Grille, Joueur),not(coupPerdantIA(7,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 2), jouerIA(6,Grille, Joueur),not(coupPerdantIA(6,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 3), jouerIA(5,Grille, Joueur),not(coupPerdantIA(5,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 4), jouerIA(4,Grille, Joueur),not(coupPerdantIA(4,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 5), jouerIA(3,Grille, Joueur),not(coupPerdantIA(3,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 6), jouerIA(2,Grille, Joueur),not(coupPerdantIA(2,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=o, random(1, 7, 7), jouerIA(1,Grille, Joueur),not(coupPerdantIA(1,Grille, Joueur)),!.

%Déblocage de situation
jouerIA(Grille, Joueur):- Joueur=o, jouerIA(7,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, jouerIA(6,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, jouerIA(5,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, jouerIA(4,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, jouerIA(3,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, jouerIA(2,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, jouerIA(1,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=o, jouerIA(0,Grille, Joueur),!.
*/
/* Fin des prédicats permettant de réaliser l'heuristique "JOUER À DROITE" */


/* Ces prédicats permettent de réaliser l'heuristique "JOUER À GAUCHE" */
jouerIA(Grille, x):- 
	jouerIA_minmax(Grille, x, 1).

jouerIA(Grille, o):- 
	jouerIA_minmax(Grille, o, 3).

jouerIA_minmax(_, _, 0) :- fail.

jouerIA_minmax(Grille, Player, Depth):-
	minmax(Grille, Depth, Player, Score, Move),
	write("Score "), write(Player), write(" : "),
	write(Score),
	nl,
	Move \== -1,
	!,
	jouerIA(Move, Grille, Player).

jouerIA_minmax(Grille, Player, Depth) :-
	NextDepth is Depth -1,
	jouerIA_minmax(Grille, Player, NextDepth).

/*
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), espaceRestant(1,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(1,Grille, Joueur)), jouerIA(1,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), espaceRestant(2,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(2,Grille, Joueur)), jouerIA(2,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), espaceRestant(3,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(3,Grille, Joueur)), jouerIA(3,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), espaceRestant(4,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(4,Grille, Joueur)), jouerIA(4,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), espaceRestant(5,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(5,Grille, Joueur)), jouerIA(5,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), espaceRestant(6,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(6,Grille, Joueur)), jouerIA(6,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), espaceRestant(7,Grille,E,L), finListe(L,Joueur), E > 3, not(coupPerdantIA(7,Grille, Joueur)), jouerIA(7,Grille, Joueur),!.

%Sinon jouer au plus près du centre quand même.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), jouerIA(1,Grille, Joueur),not(coupPerdantIA(1,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), jouerIA(2,Grille, Joueur),not(coupPerdantIA(2,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), jouerIA(3,Grille, Joueur),not(coupPerdantIA(3,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), jouerIA(4,Grille, Joueur),not(coupPerdantIA(4,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), jouerIA(5,Grille, Joueur),not(coupPerdantIA(5,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), jouerIA(6,Grille, Joueur),not(coupPerdantIA(6,Grille, Joueur)),!.
jouerIA(Grille, Joueur):- Joueur=x, random(1, 7, 1), jouerIA(7,Grille, Joueur),not(coupPerdantIA(7,Grille, Joueur)),!.

%Déblocage de situation
jouerIA(Grille, Joueur):- Joueur=x, jouerIA(1,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, jouerIA(2,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, jouerIA(3,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, jouerIA(4,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, jouerIA(5,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, jouerIA(6,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, jouerIA(7,Grille, Joueur),!.
jouerIA(Grille, Joueur):- Joueur=x, jouerIA(0,Grille, Joueur),!.
*/
/* Fin des prédicats permettant de réaliser l'heuristique "JOUER À GAUCHE" */

/* -----------------*/
/* MINMAX ALGORITHM */
/* -----------------*/

/* STATIC EVALUATION */
% init
boardHeuristic(Grille, R) :- boardHeuristic_worker_outer(Grille, 1, 0, R).

% Stopping condition
boardHeuristic_worker_outer([], _, R, R).

boardHeuristic_worker_outer([Colonne|ResteGrille], X, Acc, R2) :-
	NextX is X+1,
	boardHeuristic_worker_inner(Colonne, X, 1, Acc, R),
	boardHeuristic_worker_outer(ResteGrille, NextX, R, R2).

boardHeuristic_worker_inner([], _, _, R, R).
boardHeuristic_worker_inner([x|ResteColonne], X, Y, Acc, R) :-
	NextY is Y + 1,
	positionScore(X, Y, Score),
	Acc1 is Acc + Score,
	boardHeuristic_worker_inner(ResteColonne, X, NextY, Acc1, R).

boardHeuristic_worker_inner([o|ResteColonne], X, Y, Acc, R) :-
	NextY is Y + 1,
	positionScore(X, Y, Score),
	Acc1 is Acc - Score,
	boardHeuristic_worker_inner(ResteColonne, X, NextY, Acc1, R).

/* Position Score Function */
% Colonne, Ligne, Score
positionScore(1,1,3).
positionScore(2,1,4).
positionScore(3,1,5).
positionScore(4,1,7).
positionScore(5,1,5).
positionScore(6,1,4).
positionScore(7,1,3).

positionScore(1,2,4).
positionScore(2,2,6).
positionScore(3,2,8).
positionScore(4,2,10).
positionScore(5,2,8).
positionScore(6,2,6).
positionScore(7,2,4).

positionScore(1,3,5).
positionScore(2,3,8).
positionScore(3,3,11).
positionScore(4,3,13).
positionScore(5,3,11).
positionScore(6,3,8).
positionScore(7,3,5).

positionScore(1,4,5).
positionScore(2,4,8).
positionScore(3,4,11).
positionScore(4,4,13).
positionScore(5,4,11).
positionScore(6,4,8).
positionScore(7,4,5).

positionScore(1,5,4).
positionScore(2,5,6).
positionScore(3,5,8).
positionScore(4,5,10).
positionScore(5,5,8).
positionScore(6,5,6).
positionScore(7,5,4).

positionScore(1,6,3).
positionScore(2,6,4).
positionScore(3,6,5).
positionScore(4,6,7).
positionScore(5,6,5).
positionScore(6,6,4).
positionScore(7,6,3).
/*Max*/

/* Valid Move */
validMove(Grille, Move) :-
	validMove_worker(Grille, Move, 1).

validMove_worker([Colonne|_], Move, Move) :-
	longueur(Colonne, N),
	N < 6. % si y'en a 6 on peut plus en mettre donc <

validMove_worker([_|ResteGrille], Move, X) :- 
	NextX is X + 1, 
	validMove_worker(ResteGrille, Move, NextX).

/* All Valid Move */
allValidMoves(Grille, Moves):- findall(X, validMove(Grille, X), Moves).

/* add x in column */
addElementToColumn(Grille, ColumnNumber, Element, NewGrille) :-
	addElementToColumn_worker(Grille, ColumnNumber, Element, NewGrille, 1).

addElementToColumn_worker([], _, _, _, _).

addElementToColumn_worker([Colonne|ResteColonne], ColumnNumber, Element, [Colonne2|ResteColonne], ColumnNumber) :-
	ajoutFin(Element, Colonne, Colonne2).

addElementToColumn_worker([Colonne|ResteColonne], ColumnNumber, Element, [Colonne|NewGrille], Counter) :-
	ColumnNumber \== Counter,
	NextCounter is Counter+1,
	addElementToColumn_worker(ResteColonne, ColumnNumber, Element, NewGrille, NextCounter).

/* generate next grids */
nextGridsPossibles(Grille, Player, Grilles, Moves) :-
	allValidMoves(Grille, Moves),
	nextGridsPossibles_worker(Grille, Player, Moves, [], Grilles).

nextGridsPossibles_worker(_, _, [], Grilles, Grilles).

nextGridsPossibles_worker(Grille, Player, [Move|ResteMoves], Acc, Grilles) :-
	addElementToColumn(Grille, Move, Player, NewGrille),
	ajoutFin(NewGrille, Acc, Acc2),
	nextGridsPossibles_worker(Grille, Player, ResteMoves, Acc2, Grilles).

% Maximizing Player :
% True = x : MAX
% False = o : MIN

minmax(Grille, 0, _, Score, _) :- boardHeuristic(Grille, Score),!.
minmax(Grille, _, x, 50000, _) :- finJeuHor(Grille, x), !.
minmax(Grille, _, x, 50000, _) :- finJeuVert(Grille, x), !.
minmax(Grille, _, x, 50000, _) :- checkVictoireJoueur(Grille, x, 1, 1), !.

minmax(Grille, _, o, -50000, _) :- finJeuHor(Grille, o), !.
minmax(Grille, _, o, -50000, _) :- finJeuVert(Grille, o), !.
minmax(Grille, _, o, -50000, _) :- checkVictoireJoueur(Grille, o, 1, 1), !.

minmax(Grille, Depth, x, BestScore, BestMove) :-
	nextGridsPossibles(Grille, x, NewGrilles, Moves),
	minmax_worker(NewGrilles, Moves, Depth, x, BestScore, BestMove, -9999999, -1).

minmax(Grille, Depth, o, BestScore, BestMove) :-
	nextGridsPossibles(Grille, o, NewGrilles, Moves),
	minmax_worker(NewGrilles, Moves, Depth, o, BestScore, BestMove, 9999999, -1). 

minmax_worker([], _, _, _, BestScore, BestMove, BestScore, BestMove):- !.

minmax_worker([Grille|ResteGrilles], [CurrentMove|ResteMoves], Depth, x, Score, Move, BestScore, BestMove) :-
	NextDepth is Depth - 1,
	minmax(Grille, NextDepth, o, CurrentScore, _),
	(CurrentScore > BestScore, CurrentScore \== -9999999, CurrentScore \== 9999999 ->
	minmax_worker(ResteGrilles, ResteMoves, Depth, x, Score, Move, CurrentScore, CurrentMove);
	minmax_worker(ResteGrilles, ResteMoves, Depth, x, Score, Move, BestScore, BestMove)),
	!.

minmax_worker([Grille|ResteGrilles], [CurrentMove|ResteMoves], Depth, o, Score, Move, BestScore, BestMove) :-
	NextDepth is Depth - 1,
	minmax(Grille, NextDepth, x, CurrentScore, _),
	(CurrentScore < BestScore, CurrentScore \== -9999999, CurrentScore \== 9999999 ->
	minmax_worker(ResteGrilles, ResteMoves, Depth, o, Score, Move, CurrentScore, CurrentMove);
	minmax_worker(ResteGrilles, ResteMoves, Depth, o, Score, Move, BestScore, BestMove)),
	!.

% minmax(CurrentGrid, DEPTH, Player, Score, Move) : Score 15, Move 3


/*
% Stopping condition
boardHeuristic_worker(_, _, _, 7, _, _).

% Moving row
boardHeuristic_worker(Grille, Player, 8, Y, Acc, R) :- 
	Y1 is Y+1,
	boardHeuristic_worker(Grille, Player, 1, Y1, Acc, R).

% Nominal Case
boardHeuristic_worker(Grille, Player, X, Y, Acc, R) :- 
	X1 is X+1, 
	PositionScore(X, Y, Score), 
	Acc1 is Acc+Score, 
	boardHeuristic_worker(Grille, Player, X1, Y, Acc1, R).
*/

/* MINMAX Algorithm - No Pruning */

% TBD : minmax -> which call "ScoreEvaluation"
% TBD : "ScoreEvaluation" which call minmax


/* MINMAX SKETCH
% Grille : la grille
% Player : x ou o
% Depth : la profondeur actuelle
% MaximizingPlayer : True or False
% Move : Le meilleur Move à cette hauteur (colonne entre 1 et 7)
% MoveScore : le meilleur Score
% minmax(Grille, Depth, MaximizingPlayer, Move)
% Max Depth Reached
minmax(Grille, Player, 0, _, _, R) :- boardHeuristic(Grille, Player, R).

minmax(Grille, Player, Depth, True, BestMove, BestMoveScore) :-
	minmax_worker(Grille, Player, Depth, True, BestMove, BestMoveScore, 1, -1.0Inf, 0, -1.0Inf).

minmax(Grille, Player, Depth, False, BestMove, BestMoveScore) :-
	minmax_worker(Grille, Player, Depth, False, BestMove, BestMoveScore, 1, 1.0Inf, 0, 1.0Inf).

% minmax_worker(Grille, Player, Depth, MaximizingPlayer, BestMove, BestMoveScore, CurrentMove, CurrentBestMove, CurrentBestMoveScore)
% Grille : la grille
% Player : x ou o
% Depth : la profondeur actuelle
% MaximizingPlayer : True or False
% BestMove : Meilleur Move (valeur de retour)
% MoveScore : Meilleur Score (valeur de retour)
% CurrentMove : Move actuel
% CurrentScore : Score actuel
% CurrentBestMove : Meilleur move trouvé à cette profondeur, dans les enfants
% CurrentBestScore : Meilleur score trouvé à cette profondeur, dans les enfants
minmax_worker(_, _, _, _, CurrentBestMove, CurrentBestMoveScore, 8, _, CurrentBestMove, CurrentBestMoveScore).

minmax_worker([Grille|AutresGrilles], Player, Depth, True, BestMove, BestMoveScore, CurrentMove, CurrentScore, CurrentBestMove, CurrentBestMoveScore) :-
	NextMove is CurrentMove + 1,
	NextDepth is Depth - 1,
	minmax(Grille, Player, NextDepth, False, CurrentMove,), hi can you g
	minmax_worker(AutresGrilles, Player, Depth, True, BestMove, BestMoveScore, NextMove)
*/

/*

		FIN DU CODE DES HEURISTIQUES

*/




jouerIA(0, _, Joueur):- write('Pas de coup trouve pour le joueur '), write(Joueur),!, afficherStats(), halt.

jouerIA(Coup, Grille, Joueur):- enregistrerCoupIA(Coup,Grille,Joueur,NouvelleGrille),
			    afficherGrille(NouvelleGrille),
				write('joueur : ' ),
				write(Joueur ),
			    write('\n'),!,
				augmenterNbCoups,
				autreJoueur(Joueur, J2),
			    jouerIA(NouvelleGrille, J2),!.

				
/*jouerCoupJoueur(G):- write('Joueur x, entrez un numéro de colonne : '),
				read(N), enregistrerCoupJoueur(N,G, x, X, G),
				afficherGrille(X),
				write('\n'),
				jouerIA(X, 'o').*/

augmenterNbCoups :- b_getval(nbCoups, Coups), Coups2 is Coups + 1, b_setval(nbCoups, Coups2).

afficherNbCoups :- write('Victoire en '), b_getval(nbCoups, Coups), write(Coups), write(' coups !\n').

lancerIA:- 
	b_setval(nbVicO, 0), 
	b_setval(nbVicX, 0),
	b_setval(egalites, 0),
	b_setval(nbCoups, 0),
	jouerIA([[],[],[],[],[],[],[]], o).

afficherStats() :- b_getval(nbVicO, VicO),
	b_getval(nbVicX, VicX),
	b_getval(egalites, Eg),

	write('\nVictoires O / Victoires X / Egalites \n'),
	write(VicO), write(' / '), write(VicX), write(' / '), write(Eg), write('\n').

simulation(0) :- !, afficherStats().


simulation(NbSim /*1, */ /*, NbVictoiresX, NbVictoiresO, NbEgalite*/) :- !, 
%nbVicO is 0, nbVicX is 0, egalites is 0,
	b_setval(nbCoups, 0),
	jouerIA([[],[],[],[],[],[],[]], x),
	NbSim2 is NbSim - 1,
	simulation(NbSim2).


startSims(NbSims) :- 
	b_setval(nbVicO, 0), 
	b_setval(nbVicX, 0),
	b_setval(egalites, 0),
	simulation(NbSims).


afficherGrille(_,0).
afficherGrille(Grille, N):-	 N > 0,
						N1 is N-1,
						maplist(nthElem(N), Grille, L),
						afficherListe(L),
						write('\n'),
						afficherGrille(Grille, N1).

afficherGrille(Grille):- afficherGrille(Grille,6).

afficherListe([]):- write('|').
afficherListe([E|L]):-  write('|'),
						afficherElement(E),
						afficherListe(L).
afficherElement([]):- write(' '),!.
afficherElement(E):- write(E).


















%Si on a pas de coup immédiat on fait un coup au centre ou au plus près possible pour une victoire possible en verticale.
/*jouerIA(G):- espaceRestant(4,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(4,G)), jouerIA(4,G).
jouerIA(G):- espaceRestant(5,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(5,G)), jouerIA(5,G).
jouerIA(G):- espaceRestant(3,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(3,G)), jouerIA(3,G).
jouerIA(G):- espaceRestant(6,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(6,G)), jouerIA(6,G).
jouerIA(G):- espaceRestant(2,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(2,G)), jouerIA(2,G).
jouerIA(G):- espaceRestant(7,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(7,G)), jouerIA(7,G).
jouerIA(G):- espaceRestant(1,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(1,G)), jouerIA(1,G).

%Sinon jouer au plus près du centre quand même.
jouerIA(G):- jouerIA(4,G),not(coupPerdantIA(4,G)).
jouerIA(G):- jouerIA(5,G),not(coupPerdantIA(5,G)).
jouerIA(G):- jouerIA(3,G),not(coupPerdantIA(3,G)).
jouerIA(G):- jouerIA(6,G),not(coupPerdantIA(6,G)).
jouerIA(G):- jouerIA(2,G),not(coupPerdantIA(2,G)).
jouerIA(G):- jouerIA(7,G),not(coupPerdantIA(7,G)).
jouerIA(G):- jouerIA(1,G),not(coupPerdantIA(1,G)).

%Déblocage de situation
jouerIA(G):- jouerIA(4,G).
jouerIA(G):- jouerIA(5,G).
jouerIA(G):- jouerIA(3,G).
jouerIA(G):- jouerIA(6,G).
jouerIA(G):- jouerIA(2,G).
jouerIA(G):- jouerIA(7,G).
jouerIA(G):- jouerIA(1,G).
jouerIA(G):- jouerIA(0,G).*/


/* Ces prédicats permettent de réaliser l'heuristique "JOUER À GAUCHE" *//*
jouerIA(G):- espaceRestant(1,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(1,G)), jouerIA(1,G).
jouerIA(G):- espaceRestant(2,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(2,G)), jouerIA(2,G).
jouerIA(G):- espaceRestant(3,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(3,G)), jouerIA(3,G).
jouerIA(G):- espaceRestant(4,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(4,G)), jouerIA(4,G).
jouerIA(G):- espaceRestant(5,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(5,G)), jouerIA(5,G).
jouerIA(G):- espaceRestant(6,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(6,G)), jouerIA(6,G).
jouerIA(G):- espaceRestant(7,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(7,G)), jouerIA(7,G).

%Sinon jouer au plus près du centre quand même.
jouerIA(G):- jouerIA(1,G),not(coupPerdantIA(1,G)).
jouerIA(G):- jouerIA(2,G),not(coupPerdantIA(2,G)).
jouerIA(G):- jouerIA(3,G),not(coupPerdantIA(3,G)).
jouerIA(G):- jouerIA(4,G),not(coupPerdantIA(4,G)).
jouerIA(G):- jouerIA(5,G),not(coupPerdantIA(5,G)).
jouerIA(G):- jouerIA(6,G),not(coupPerdantIA(6,G)).
jouerIA(G):- jouerIA(7,G),not(coupPerdantIA(7,G)).

%Déblocage de situation
jouerIA(G):- jouerIA(1,G).
jouerIA(G):- jouerIA(2,G).
jouerIA(G):- jouerIA(3,G).
jouerIA(G):- jouerIA(4,G).
jouerIA(G):- jouerIA(5,G).
jouerIA(G):- jouerIA(6,G).
jouerIA(G):- jouerIA(7,G).
jouerIA(G):- jouerIA(0,G).
*//* Fin des prédicats permettant de réaliser l'heuristique "JOUER À GAUCHE" */


/*CODE MORT


enregistrerCoupArbre(1, [L|G], J, [[J|L]|G]):- longueur(L,N), N < 6.
enregistrerCoupArbre(N, [T|X], J, [T|G]):-	N > 0,
										N1 is N-1,
										enregistrerCoupArbre(N1, X, J, G).




% Evaluation de la grille de jeu
% evalVert(Grille, Joueur, nb pions alignés, X) Paramètres : G grille, J joueur 
%X : valeur de retour

evalVert([], _, P, P):- write(fini).
evalVert([L|G],J, P, X):-	sublist([J,J,J,J], L),
							evalVert(G, J, P, 4, X).
evalVert([L|G],J, P, X):-	sublist([J,J,J], L),
							evalVert(G, J, P, 3, X).
evalVert([L|G],J, P, X):-	sublist([J,J], L),
							evalVert(G, J, P, 2, X).
evalVert([_|G],J, P, X):- evalVert(G, J, P, 1, X).

%evalVert(Grille, Joueur, max(Pions alignés dans les colonnes précédentes), nb Pions alignés dans cette colonne, X)
%X  valeur de retour

evalVert(G,J, P1, P2, X):-	max(P1, P2, P),
							evalVert(G, J, P, X).

%evalVert(Grille, Joueur, max(Pions du joueur alignés dans la grille))	
% X : valeur de retour						
evalVert(G, J, X):- evalVert(G,J, 0, 1, X).

% Paramètres : N numéro de la ligne à partir duquel on traite, G grille, J joueur 
evalHor(_,[],_,_):- write(fini).
evalHor(N, G, J, P):- maplist(nthElem(N), G, L),
					 sublist([J,J,J,J],L),
					 evalHor(N, G, J, P, 4).
evalHor(N, G, J, P):- maplist(nthElem(N), G, L),
					 sublist([J,J,J],L),
					 evalHor(N, G, J, P, 3).
evalHor(N, G, J, P):- maplist(nthElem(N), G, L),
					 sublist([J,J],L),
					 evalHor(N, G, J, P, 2).
evalHor(N, G, J, P):- maplist(nthElem(N), G, L),
					 sublist([J],L),
					 evalHor(N, G, J, P, 1).
evalHor(N, G, J, P1, P2):- N > 0,
					 N1 is N-1,
					 write(toto),
					 max(P1, P2, P),
					 evalHor(N1, G, J, P),
					 write(P).
evalHor(G,J,_):- evalHor(6, G, J, 0, 1).

evalGrille(G, J, X) :- evalHor(G,J,P1),
					evalVert(G, J, P2),
					max(P1,P2, X).


% Paramètres : Grille grille, Joueur joueur, P profondeur, A arbre obtenu
tracerArbre(_, _, 0, _).
tracerArbre(Grille, Joueur, P, A):- P > 0,
					      P1 is P-1,
						  tracerBranche(Grille, Joueur, P1, A, 7).

tracerBranche(_, _, _, _, 1).
tracerBranche(Grille, x, P, A, N):- N > 0,
							   N1 is N-1,
							   enregistrerCoupArbre(N, Grille, x, X),
							   tracerArbre(X, o, P, A),
							   tracerBranche(Grille, x, P, A, N1).
tracerBranche(Grille, o, P, A, N):- N > 0,
							   N1 is N-1,
							   enregistrerCoupArbre(N, Grille, o, X),
							   tracerArbre(X, x, P, A),
							   tracerBranche(Grille, o, P, A, N1).

*/
/*
%function to return score
function to insert color in top of a column
function to get best score
function to get all moves possible

% Grille: la grille
% Depth: profondeur restante
% Player: MIN ou MAX
% Score: Valeur de retour
minmax(Grille, 0, Player, Score).
minmax(Grille, Depth,Player,Score):- coupPerdant();coupGangnant(),!.


*/