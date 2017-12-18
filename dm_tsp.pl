/* Lachheb Ismaël
   Quentin Bresson

   Voyageur du commerce
    - Avec méthodes gloutonnes
    - Avec méthode de l'élastique

    ?- time(glouton(1)).
    [11,12,27,31,17,19,18,16,36,9,5,8,3,7,34,33,6,28,13,15,24,4,22,38,37,20,25,23,26,2,35,30,32,14,10,29,21,1] cout total =2154317.7644572295
    % 1,418,805 inferences, 0.135 CPU in 0.135 seconds (100% CPU, 10483736 Lips)
    true.

*/
:- use_module(library(statistics)).
/* On considère un graphe complet */

dist(a,b, 4).
dist(c,a,3).
dist(b,c,9).

dist(b,a, 4).
dist(a,c,3).
dist(c,b,9).


/* ***************** Production de la matrice à partir des données *******************
Lecture du fichier
lecture des villes dans un fichier tsplib
*/
read_villes(Str) :-
    (at_end_of_stream(Str) ->
        close(Str);

    read_string(Str, "\n", "\r", _, String),
    write('Nouvelle ville  = '),writeln(String),
    split_string(String," ", " ", L),
    maplist(number_string,LN,L),
    FV=..[ville|LN],
    assertz(FV), /* on stocke toutes les coordonnées des villes dans un prédicat dynamique ville(V,XV,YV */
    read_villes(Str)).

/* read_info_villes(+Str) ou Str est un Stream
   Lecture du fichier .tsplib
*/

read_info_villes(Str) :-
    read_string(Str, "\n", "\r", _, String),
/*     write('Chaine lue = '),writeln(String), */
    (sub_string(String,0,_,_,"NODE_COORD_SECTION") -> /*tant qu'on n'est pas sur une ligne de ville, on skippe */
        read_villes(Str);
        read_info_villes(Str)).

/* read_tsp_file(+NomFichier) avec NomFichier : chaine de caractères
   Appel read_tsp_file('dji38.tsp')
*/

read_tsp_file(NomF) :-
    open(NomF,read,Str),
    retractall(ville(_,_,_)),
    read_info_villes(Str).

/* Fonction distance */

matrice2(M,C,V1,_,F):- ville(X,Y,Z), ville(X2,Y2,Z2) ,(X2>X), (X>=V1), not(member([X,X2],C)), !, D is sqrt((Z^2 - Y^2) + (Z2^2-Y2^2)), matrice2([[X,X2,D],[X2,X,D]|M],[[X,X2],[X2,X]|C],X,X2, F).
matrice2(M,_,_,_,F):- F=M.

/* ****************** Méthode Gloutonne ***********************

   Code test :
  glouton(a,[[a,b,4], [c,a,3], [b,c,9], [b,a,4], [a,c,3], [c,b,9]],[],0).
  glouton(D):- Mat is matrice(M,_,F), glouton(D,Mat,[],0).
*/

min([[A,F]|Q],E,N):- min(Q,A,F,E,N).
min([[T,F]|Q],M,_,E,N):- T<M, min(Q,T,F,E,N),!.
min([[T,_]|Q],M,O,E,N):- T>=M, min(Q,M,O,E,N),!.
min([],M,O,M,O).


verifier(Tbis,C) :- not(member(Tbis,C)).

/* La méthode doit toujours aller dans la ville qu'il n'a pas encore visité la plus proche de lui jusq'à avoir visité toutes les villes. */

glouton(D):- matrice2([],[],1,2,F), glouton(D,F,[],0).
glouton(A,[[M1,M2,M3]|Q],L,C):- distmin(A,[[M1,M2,M3]|Q],[],E,N,L),NewC is E+C, NewL=[A|L], glouton(N,[[M1,M2,M3]|Q],NewL,NewC),!.

/*Lorsque on a fini de parcourir le graphe */
glouton(A,[[_,_,_]|_],L,C):- NewL=[A|L],write(NewL), write(" cout total ="), write( C).

distmin(A,[[M,F,D]|Q],C,E,N,L):- M=A,verifier(F,L),distmin(A,Q,[[D,F]|C],E,N,L),!.
distmin(A,[[_,_,_]|Q],R,E,N,L):- distmin(A,Q,R,E,N,L),!.

/*
distmin(a,[[M,_,D]],[C], E):- M=a, distmin(a,[],[D|C],E).
distmin(a,[[M,_,D]],[C], E):- not(M=a), distmin(a,[],[D|C],E).
*/

distmin(_,[],S,E,N,_):- min(S,E,N).

/* ****************** Algo A* ***********************
<<<<<<< HEAD

=======
>>>>>>> d28e18513249007dea3afd542223a2f0e3393a65
	Fonction heuristique 1 : 2-opt :
	On part d'une solution réalisable (facile à trouver car graphe complet), et on effectue des flips successifs afin d'approcher la solution optimale
	Un flip consiste à considérer deux arêtes (A, B) et (C, D). Si le remplacement de ses arêtes par les arêtes (A, C), (B, D) diminue le cout total, on effectue le remplacement ou flip.
	Soit v un sommet donné et t un autre sommet
	On note s(v) le sommet suivant le sommet v dans notre solution courante.
	si (v, s(v)) est supérieur au cout de (s(v), s(t)) le flip considéré est dit prometteur. On ne considère que les flips prometteurs dans un premier temps
<<<<<<< HEAD

	Généralisation à l'algo de Lin-Kernighan (k-opt) possible, mais compliqué. On ne considère plus deux arêtes mais deux chemins

	Fonction heuristique 2 : Arbre couvrant de poids minimal :
	On commence par générer un arbre couvrant de poids minimal via l'algorithme de kruskal



=======
	Généralisation à l'algo de Lin-Kernighan (k-opt) possible, mais compliqué. On ne considère plus deux arêtes mais deux chemins
	Fonction heuristique 2 : Arbre couvrant de poids minimal :
	On commence par générer un arbre couvrant de poids minimal via l'algorithme de kruskal
>>>>>>> d28e18513249007dea3afd542223a2f0e3393a65
	permutation, permute et enlever
*/
enlever( X, [X|Q], Q).
enlever( X, [Y|Q], [Y|Q1]) :- enlever( X, Q, Q1).

permutation([],[]):- !.
permutation( L, [X|Q1]) :- enlever( X, L, Q), permutation( Q, Q1).


/* permute 4 : Remplace les occurences de sommet1 par Sommet2 et vice-versa dans la liste 1 et stocke le résultat dans la liste 2*/

permute(_,_,[], []).
permute(Sommet1, Sommet2, [Sommet2 | CH], [Sommet1 | NewPath]) :- permute(Sommet1, Sommet2, CH, NewPath), !.
permute(Sommet1, Sommet2, [Sommet1 | CH], [Sommet2 | NewPath]) :- permute(Sommet1, Sommet2, CH, NewPath), !.
permute(Sommet1, Sommet2, [A |CH], [A | NewPath]) :- permute(Sommet1, Sommet2, CH, NewPath).

/* Code pour fermer une liste. Apres l'initialisation de la solution réalisable, une variable non déclarée traine a la fin*/

fermer_liste([]).
fermer_liste([_ | T]) :- fermer_liste(T).

/* Calculer le cout d'une solution */

calcul_cout_total([_ | []], 0).
calcul_cout_total([A | [B | CH]], X) :- ville(A, C, D), ville(B,E,F), distance(ville(A,C,D),ville(B,E,F), [_,_,Y]), calcul_cout_total([B | CH], Z), !, X is Y + Z.

/* On trouve une solution réalisable à notre problème (facile car graphe complet : on passe une seule fois par chaque ville) */

init_sol_realisable(_, Acc) :- not(ville(Acc,_,_)).
init_sol_realisable(CH, Acc) :- ville(Acc, _, _), not(member(Acc, CH)), NewAcc is Acc + 1, init_sol_realisable([Acc | CH], NewAcc).
init_sol_realisable(CH, Acc) :- ville(Acc, _, _), member(Acc, CH), NewAcc is Acc + 1, init_sol_realisable(CH, NewAcc), !.

init_sol_realisable(Ville_depart, [Ville_depart | Z], Acc) :- init_sol_realisable([Ville_depart | Z], Acc), fermer_liste(Z).

/* Après avoir obtenu une solution réalisable, on cherche une permutation qui diminuera le cout du voyage (2-opt) */

heuristique_1(Res_Final, Cout_final, Acc1, _, _, _) :- Verif_acc1 is Acc1 + 1,
	not(ville(Verif_acc1, _, _)),
	write("\nResultat : "), write(Res_Final), write(" \nCout :"), write(Cout_final), !.

/* Lorsqu'on a considéré tous les flips pour le sommet Acc1, on passe au suivant */

<<<<<<< HEAD
heuristique_1(CH, X, Acc1, Acc2, Res, Cout_actuel) :- 
=======
heuristique_1(CH, X, Acc1, Acc2, Res, Cout_actuel) :-
>>>>>>> d28e18513249007dea3afd542223a2f0e3393a65
	not(ville(Acc2, _, _)), NewAcc1 is Acc1 + 1,
	heuristique_1(CH, X, NewAcc1, 1, Res, Cout_actuel), !.

/* Cas où on considère un chemin absurde (de la ville A à la ville A) */

heuristique_1(CH, X, Acc1, Acc1, Res, Cout_actuel) :- NewAcc is Acc1 + 1, heuristique_1(CH, X, Acc1, NewAcc, Res, Cout_actuel), !.

/* Si le flip a amélioré notre solution, on le conserve */

<<<<<<< HEAD
heuristique_1(CH, X, Acc1, Acc2, _, _) :- 
	Follower_1 is Acc1 + 1, ville(Follower_1, _, _), 
	permute(Follower_1, Acc2, CH, NewPath), calcul_cout_total(NewPath, NewCost), NewCost =< X, NewAcc2 is Acc2 + 1, 
	heuristique_1(NewPath, NewCost, Acc1, NewAcc2, NewPath, NewCost), !.
	
/* Si le flip n'a pas amélioré notre solution, on l'ignore */

heuristique_1(CH, X, Acc1, Acc2, Res, Cout_actuel) :- 
	Follower_1 is Acc1 + 1, ville(Follower_1, _, _),
	permute(Follower_1, Acc2, CH, NewPath), calcul_cout_total(NewPath, NewCost), NewCost > X, NewAcc2 is Acc2 + 1, 
	heuristique_1(CH, X, Acc1, NewAcc2, Res, Cout_actuel), !.
	
/* Appel de l'heuristique 1

=======
heuristique_1(CH, X, Acc1, Acc2, _, _) :-
	Follower_1 is Acc1 + 1, ville(Follower_1, _, _),
	permute(Follower_1, Acc2, CH, NewPath), calcul_cout_total(NewPath, NewCost), NewCost =< X, NewAcc2 is Acc2 + 1,
	heuristique_1(NewPath, NewCost, Acc1, NewAcc2, NewPath, NewCost), !.

/* Si le flip n'a pas amélioré notre solution, on l'ignore */

heuristique_1(CH, X, Acc1, Acc2, Res, Cout_actuel) :-
	Follower_1 is Acc1 + 1, ville(Follower_1, _, _),
	permute(Follower_1, Acc2, CH, NewPath), calcul_cout_total(NewPath, NewCost), NewCost > X, NewAcc2 is Acc2 + 1,
	heuristique_1(CH, X, Acc1, NewAcc2, Res, Cout_actuel), !.

/* Appel de l'heuristique 1
ville(Acc1, Coor_1_acc1, Coor_2_acc1), ville(Follower_1, Coor_1_follower_1, Coor_2_follower_1), ville(Acc2, Coor_1_acc2, Coor_2_acc2), ville(Follower_2, Coor_1_follower_2, Coor_2_follower_2),
	distance(ville(Acc1, Coor_1_acc1, Coor_2_acc1), ville(Follower_1, Coor_1_follower_1, Coor_2_follower_1), Dist_1), distance(ville(Follower_1, Coor_1_follower_1, Coor_2_follower_1), ville(Follower_2, Coor_1_follower_2, Coor_2_follower_2), Dist_2), .
>>>>>>> d28e18513249007dea3afd542223a2f0e3393a65
	Ville_depart : numéro de la ville de départ
	CH : Liste représentant le trajet du voyageur
	X : Cout du chemin
*/

a_etoile_1(Ville_depart) :- init_sol_realisable(Ville_depart, CH, 1), calcul_cout_total(CH, X), heuristique_1(CH, X, Ville_depart, 2, CH, X), !.
