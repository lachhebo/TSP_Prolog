/* Lachheb Ismaël
   Quentin Bresson

   Voyageur du commerce
    - Avec méthodes gloutonnes
    - Avec méthode de l'élastique

    ?- time(glouton(1)).
    [11,12,27,31,17,19,18,16,36,9,5,8,3,7,34,33,6,28,13,15,24,4,22,38,37,20,25,23,26,2,35,30,32,14,10,29,21,1] cout total =2154317.7644572295
    % 1,418,805 inferences, 0.135 CPU in 0.135 seconds (100% CPU, 10483736 Lips)
    true.
	
	Test de l'algo glouton sur qa194 (Cout optimal : 9352) :
	
	?- time(glouton(1)).
	[84,100,119,122,118,160,194,176,182,183,186,187,190,192,189,191,188,193,185,171,166,162,158,159,165,168,167,170,180,178,177,181,184,175,173,174,179,172,169,164,163,161,156,130,20,89,111,104,101,99,94,90,98,86,85,65,63,36,59,62,82,87,80,76,71,23,25,7,4,2,3,5,9,10,12,15,19,50,45,57,60,69,74,72,75,78,91,103,102,109,113,114,126,125,127,132,134,137,140,145,149,146,142,138,139,141,157,154,144,150,153,152,147,151,155,143,148,136,131,129,135,133,124,123,128,120,121,117,116,115,112,110,108,107,106,105,97,93,96,95,92,88,83,81,79,77,70,64,68,73,66,67,61,34,31,30,32,35,44,42,49,55,54,43,40,38,41,46,48,52,53,56,58,51,47,39,37,27,22,29,28,33,18,21,24,26,17,11,14,13,16,8,6,1] cout total =10982.563823401693
	% 892,297,654 inferences, 83.734 CPU in 84.760 seconds (99% CPU, 10656288 Lips)
	true.

	?- time(a_etoile_1(1)).

	Resultat : [5,15,19,30,32,31,27,45,64,70,114,78,88,58,38,9,10,12,35,50,49,55,112,159,177,181,188,193,185,180,168,154,63,1,2,3,4,7,11,95,103,104,98,20,65,85,86,90,94,99,89,62,59,13,14,24,96,118,147,152,153,150,144,139,122,141,157,173,174,179,172,145,140,125,126,127,134,132,137,142,138,146,149,163,164,169,161,156,130,101,23,28,43,48,52,46,44,42,53,56,73,105,107,108,167,170,178,186,176,182,194,190,175,165,116,115,120,133,135,143,148,160,155,151,136,129,117,84,68,67,66,57,33,60,83,92,106,123,124,128,121,119,113,102,91,87,76,25,17,22,29,37,51,77,75,183,187,189,192,191,184,93,81,79,69,74,72,26,21,18,97,131,166,171,162,158,80,71,6,8,16,36,82,111,109,61,47,39,34,40,41,54,100,110]
	Cout :22298.40417448868
	% 301,294,923 inferences, 84.781 CPU in 85.162 seconds (100% CPU, 3553792 Lips)
	true.

*/
:- use_module(library(statistics)).
/* On considère un graphe complet 


 ***************** Production de la matrice à partir des données *******************
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
    assertz(FV), /* on stocke toutes les coordonnées des villes dans un prédicat dynamique ville(V,XV,YV) */
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

matrice2(M,C,V1,_,F):- ville(X,Y,Z), ville(X2,Y2,Z2) ,(X2>X), (X>=V1), not(member([X,X2],C)), !, D is sqrt(((Y-Y2)^2) + ((Z-Z2)^2)), matrice2([[X,X2,D],[X2,X,D]|M],[[X,X2],[X2,X]|C],X,X2, F).
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

	Fonction heuristique 1 : 2-opt :
	On part d'une solution réalisable (facile à trouver car graphe complet), et on effectue des flips successifs afin d'approcher la solution optimale
	Un flip consiste à considérer deux arêtes (A, B) et (C, D). Si le remplacement de ses arêtes par les arêtes (A, C), (B, D) diminue le cout total, on effectue le remplacement ou flip.
	Soit v un sommet donné et t un autre sommet
	On note s(v) le sommet suivant le sommet v dans notre solution courante.
	si (v, s(v)) est supérieur au cout de (s(v), s(t)) le flip considéré est dit prometteur. On ne considère que les flips prometteurs dans un premier temps

	Généralisation à l'algo de Lin-Kernighan (k-opt) possible, mais compliqué. On ne considère plus deux arêtes mais deux chemins

	Fonction heuristique 2 : Arbre couvrant de poids minimal :
	On commence par générer un arbre couvrant de poids minimal via l'algorithme de kruskal

	permutation, permute et enlever
*/
enlever( X, [X|Q], Q).
enlever( X, [Y|Q], [Y|Q1]) :- enlever( X, Q, Q1).

permutation([],[]):- !.
permutation( L, [X|Q1]) :- enlever( X, L, Q), permutation( Q, Q1).

/* ville(X,Y,Z), ville(X2,Y2,Z2) ,(X2>X), (X>=V1), D is sqrt(((Z-Y)^2) + ((Z2-Y2)^2)),  */

distance(ville(X1,Y1,Z1),ville(X2,Y2,Z2), D) :- M is sqrt(((Y1-Y2)^2) + ((Z1 - Z2)^2)), D = [X1, X2, M].

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

heuristique_1(Res_Final, Cout_final, Acc1, _, _, _, Cpt) :- Verif_acc1 is Acc1 + 1,
	not(ville(Verif_acc1, _, _)), Cpt >= 2,
	write("\nResultat : "), write(Res_Final), write(" \nCout :"), write(Cout_final), !.
	
heuristique_1(Res_Final, Cout_final, Acc1, _, _, _, Cpt) :- Verif_acc1 is Acc1 + 1,
	not(ville(Verif_acc1, _, _)), Cpt < 2, NewCpt is Cpt + 1, heuristique_1(Res_Final, Cout_final, 1, 2, Res_Final, Cout_final, NewCpt), !.

/* Lorsqu'on a considéré tous les flips pour le sommet Acc1, on passe au suivant */

heuristique_1(CH, X, Acc1, Acc2, Res, Cout_actuel, Cpt) :-
	not(ville(Acc2, _, _)), NewAcc1 is Acc1 + 1,
	heuristique_1(CH, X, NewAcc1, 1, Res, Cout_actuel, Cpt), !.

/* Cas où on considère un chemin absurde (de la ville A à la ville A) */

heuristique_1(CH, X, Acc1, Acc1, Res, Cout_actuel, Cpt) :- NewAcc is Acc1 + 1, heuristique_1(CH, X, Acc1, NewAcc, Res, Cout_actuel, Cpt), !.

/* Si le flip a amélioré notre solution, on le conserve */

heuristique_1(CH, X, Acc1, Acc2, _, _, Cpt) :- 
	Follower_1 is Acc1 + 1, ville(Follower_1, _, _),
	permute(Follower_1, Acc2, CH, NewPath), calcul_cout_total(NewPath, NewCost), NewCost =< X, NewAcc2 is Acc2 + 1,
	heuristique_1(NewPath, NewCost, Acc1, NewAcc2, NewPath, NewCost, Cpt), !.
	
/* Si le flip n'a pas amélioré notre solution, on l'ignore */

heuristique_1(CH, X, Acc1, Acc2, Res, Cout_actuel, Cpt) :- 
	Follower_1 is Acc1 + 1, ville(Follower_1, _, _),
	permute(Follower_1, Acc2, CH, NewPath), calcul_cout_total(NewPath, NewCost), NewCost > X, NewAcc2 is Acc2 + 1, 
	heuristique_1(CH, X, Acc1, NewAcc2, Res, Cout_actuel, Cpt), !.
	

/* Appel de l'heuristique 1

	Ville_depart : numéro de la ville de départ
	CH : Liste représentant le trajet du voyageur
	X : Cout du chemin
	random_permutation permet de mélanger notre solution réalisable. celà rend l'heuristique moins sensible aux minimas locaux. 
	Cependant la solution sera a chaque fois différente
	
	Problème des 38 villes de djiboutis (Cout optimal : 6656).
	Meilleur résultat obtenu (environ 30 essais, on part de la ville 1) :
	
	?- a_etoile_1(1).

	Resultat : [27,31,36,34,33,38,37,35,32,30,26,25,23,20,22,24,28,19,18,11,12,9,8,13,15,17,16,7,6,3,5,4,2,1,10,14,21,29]
	Cout :7032.226891035899
	true.
*/

a_etoile_1(Ville_depart) :- init_sol_realisable(Ville_depart, CH, 1), random_permutation(CH, CHbis), calcul_cout_total(CHbis, X), heuristique_1(CHbis, X, Ville_depart, 2, CHbis, X, 0), !.