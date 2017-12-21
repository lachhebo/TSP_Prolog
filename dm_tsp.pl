/*
   Lachheb Ismaël
   Quentin Bresson

  Voyageur du commerce
    - Avec la méthode gloutonne
    - Avec la méthode 2-opt
    - Avec l'algorithme A* utilisant des fonctions heuristiques gloutonnes et MST (Minimum Spanning Tree)

  Les résultats montrés ici n'affiche pas toujours la ville d'origine dans la solution finale, mais elle est prise en compte dans le calcul du temps de trajet

  Tests sur dji38 (Cout optimal : 6656) :

  Algo 2-opt :

  ?- time(heur_2opt(1, X, Y)).
  % 4,341,281 inferences, 0.860 CPU in 0.860 seconds (100% CPU, 5049747 Lips)
  X = [1, 2, 4, 3, 5, 6, 7, 8, 9|...],
  Y = 6659.431532931465.


  Algo glouton :

  ?- time(glouton(1)).

  Chemin : [38,11,12,19,18,17,16,10,14,21,29,30,32,35,37,33,34,36,31,27,28,24,22,26,25,23,20,15,13,9,8,6,7,5,3,4,2,1]
  Cout total : 7890.854092451755
  % 1,418,728 inferences, 0.105 CPU in 0.105 seconds (100% CPU, 13527899 Lips)
  true.


  Algo A* avec ACM :

  ?- time(a_etoile_acm(1,[1],X,Y)).
  % 996,157,371 inferences, 74.115 CPU in 74.115 seconds (100% CPU, 13440771 Lips)
  X = [10, 14, 21, 15, 13, 16, 17, 18, 19|...],
  Y = 7780.720412250776.


  Algo A* avec la méthode des distances :

  ?- time(a_etoile_dist(1,[1],X,Y)).
  % 3,775,682 inferences, 0.387 CPU in 0.387 seconds (100% CPU, 9749541 Lips)
  X = [38, 36, 31, 27, 34, 33, 37, 35, 32|...],
  Y = 7822.45411542367.


  Autres instances de TSP :

  Test de l'algo glouton sur qa194 (Cout optimal : 9352) :

	?- time(glouton(1)).
	[84,100,119,122,118,160,194,176,182,183,186,187,190,192,189,191,188,193,185,171,166,162,158,159,165,168,167,170,180,178,177,181,184,175,173,174,179,172,169,164,163,161,156,130,20,89,111,104,101,99,94,90,98,86,85,65,63,36,59,62,82,87,80,76,71,23,25,7,4,2,3,5,9,10,12,15,19,50,45,57,60,69,74,72,75,78,91,103,102,109,113,114,126,125,127,132,134,137,140,145,149,146,142,138,139,141,157,154,144,150,153,152,147,151,155,143,148,136,131,129,135,133,124,123,128,120,121,117,116,115,112,110,108,107,106,105,97,93,96,95,92,88,83,81,79,77,70,64,68,73,66,67,61,34,31,30,32,35,44,42,49,55,54,43,40,38,41,46,48,52,53,56,58,51,47,39,37,27,22,29,28,33,18,21,24,26,17,11,14,13,16,8,6,1]
  cout total =10982.563823401693
	% 892,297,654 inferences, 83.734 CPU in 84.760 seconds (99% CPU, 10656288 Lips)
	true.


*/
:- use_module(library(statistics)).
:- use_module(library(lists)).


/* On considère un graphe complet

 ***************** Lecture des villes dans un fichier tsplib ******************* */

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

/* ****************** Heuristiques ***********************

	Fonction heuristique 1 : 2-opt :
	On part d'une solution réalisable (facile à trouver car graphe complet), et on effectue des flips successifs afin d'approcher la solution optimale
	Un flip consiste à considérer deux arêtes (A, B) et (C, D). Si le remplacement de ses arêtes par les arêtes (A, C), (B, D) diminue le cout total, on effectue le remplacement ou flip.
	Soit v un sommet donné et t un autre sommet
	On note s(v) le sommet suivant le sommet v dans notre solution courante.
	si (v, s(v)) est supérieur au cout de (s(v), s(t)) le flip considéré est dit prometteur. On ne considère que les flips prometteurs dans un premier temps

	Généralisation à l'algo de Lin-Kernighan (k-opt) possible, mais compliqué. On ne considère plus deux arêtes mais deux chemins

	Fonction heuristique 2 : gloutonne (greedy) :
  On se place sur le noeud de départ, et on se déplace jusqu'au noeud le plus proche non visité. Lorsqu'il n'y a plus de noeuds on revient au noeud de départ
  Dans cette heuristique c'est la génération de la matrice qui va prendre le plus de temps
*/

/* ****************** Méthode Gloutonne ***********************

  Code test :
  time(glouton(1)).
*/

/* Etant donné deux villes A et B, le prédicat distance(ville(A, +Coor_A1, +Coor_A2), ville(+B, +Coor_B1, +Coor_B2), -D) calcule la distance les séparant et la stocke dans D */

distance(ville(X1,Y1,Z1),ville(X2,Y2,Z2), D) :- M is sqrt((((Y1-Y2)^2) + ((Z1 - Z2)^2))), D = [X1, X2, M].


/* Fonction de création de la matrice (liste des arêtes), utile pour l'heuristique gloutonne et la fonction heuristique acm */

comparaison1(_):- matrice([],[],1,2,_).

matrice(M,C,V1,_,F):- ville(X,Y,Z), ville(X2,Y2,Z2) ,(X2>X), (X>=V1), not(member([X,X2],C)), !, D is sqrt(((Y-Y2)^2) + ((Z-Z2)^2)), matrice([[X,X2,D],[X2,X,D]|M],[[X,X2],[X2,X]|C],X,X2, F).
matrice(M,_,_,_,F):- F=M.

/* Le prédicat min(...) permet de trouver l'arête permettant de rejoindre le noeud le plus proche du noeud courant */

min([[A,F]|Q],E,N):- min(Q,A,F,E,N).
min([[T,F]|Q],M,_,E,N):- T<M, min(Q,T,F,E,N),!.
min([[T,_]|Q],M,O,E,N):- T>=M, min(Q,M,O,E,N),!.
min([],M,O,M,O).

/* verifier(+Tbis, +C) : vérifie l'absence d'un élément dans une liste. Simplifie la lecture des autres prédicats */

verifier(Tbis,C) :- not(member(Tbis,C)).

/* La méthode doit toujours aller dans la ville qu'il n'a pas encore visité la plus proche de lui jusq'à avoir visité toutes les villes. */

glouton(D):- matrice([],[],1,2,F), glouton(D,F,[],0).
glouton(A,[[M1,M2,M3]|Q],L,C):- distmin(A,[[M1,M2,M3]|Q],[],E,N,L), NewC is E+C, NewL=[A|L], glouton(N,[[M1,M2,M3]|Q],NewL,NewC),!.

/* Calculer le cout d'une solution */
glouton(A,[[_,_,_]|_],L,C):- NewL=[A|L], write(" \nChemin : "), write(NewL), write(" \nCout total : "), write( C).

distmin(A,[[M,F,D]|Q],C,E,N,L):- M=A,verifier(F,L),distmin(A,Q,[[D,F]|C],E,N,L),!.
distmin(A,[[_,_,_]|Q],R,E,N,L):- distmin(A,Q,R,E,N,L),!.

distmin(_,[],S,E,N,_):- min(S,E,N).

/* ****************** Algorithme 2-opt *********************** */

/* permute_2opt_reverse(+Sommet, +List1, -List_Res) : Inverse les positions des éléments jusqu'au sommet Sommet dans la liste List et stocke le résultat dans la liste List_Res */

permute_2opt_reverse(Sommet, [Sommet | CH], CH) :- !.
permute_2opt_reverse(Sommet, [_ | CH], NewPath) :- permute_2opt_reverse(Sommet, CH, NewPath), !.

/* Le prédicat cut_list(Sommet, Liste, Res) Avance la lecture de la liste Liste jusqu'au sommet Sommet et renvoie le reste de la liste dans Res */

cut_list(_, [], []).
cut_list(Sommet, [Sommet | CH], CH) :- !.
cut_list(Sommet, [_ | CH], X) :- cut_list(Sommet, CH, X), !.

/* permute_2opt(+Sommet1, +Sommet2, +List1, -NewPath) : Remplace les occurences de Sommet1 par Sommet2 et vice-versa dans la liste List1 et stocke le résultat dans la liste NewPath */


permute_2opt(_,_,[],[]) :-!.
permute_2opt(Sommet1, Sommet2, [Sommet2 |CH], NewPath) :- reverse(CH, NewCH), permute_2opt_reverse(Sommet1, NewCH, ReversedList), cut_list(Sommet1, CH, New), append([[Sommet1], ReversedList, [Sommet2], New], NewPath), !.
permute_2opt(Sommet1, Sommet2, [Sommet1 |CH], NewPath) :- reverse(CH, NewCH), permute_2opt_reverse(Sommet2, NewCH, ReversedList), cut_list(Sommet2, CH, New), append([[Sommet2], ReversedList, [Sommet1], New], NewPath), !.
permute_2opt(Sommet1, Sommet2, [A |CH], [A | NewPath]) :- permute_2opt(Sommet1, Sommet2, CH, NewPath), !.

/* Calculer le cout d'une solution */
fermer_liste([]).
fermer_liste([_ | T]) :- fermer_liste(T).

/* Calculer le cout d'une solution */

calcul_cout_total([_ | []], 0).
calcul_cout_total([A | [B | CH]], X) :- ville(A, C, D), ville(B,E,F), distance(ville(A,C,D),ville(B,E,F), [_,_,Y]), calcul_cout_total([B | CH], Z), !, X is Y + Z.

/* On trouve une solution réalisable pseudo-aléatoire à notre problème (facile car graphe complet : on passe une seule fois par chaque ville) */
init_sol_realisable(_, Acc) :- not(ville(Acc,_,_)).
init_sol_realisable(CH, Acc) :- ville(Acc, _, _), not(member(Acc, CH)), NewAcc is Acc + 1, init_sol_realisable([Acc | CH], NewAcc).
init_sol_realisable(CH, Acc) :- ville(Acc, _, _), member(Acc, CH), NewAcc is Acc + 1, init_sol_realisable(CH, NewAcc), !.

init_sol_realisable(Ville_depart, [Ville_depart | Z], Acc) :- init_sol_realisable([Ville_depart | Y], Acc), fermer_liste(Z), random_permutation(Y, Z).

/* Après avoir obtenu une solution réalisable, on cherche une permutation qui diminuera le cout du voyage (2-opt) */
/* On ne considère pas de flip si on est sur la ville de départ */

heuristique_1(Ville_depart, CH, X, Ville_depart, Acc2, Res, Cout_actuel, Cpt, Path, Cost) :-
	NewAcc1 is Ville_depart + 1,
	heuristique_1(Ville_depart, CH, X, NewAcc1, Acc2, Res, Cout_actuel, Cpt, Path, Cost), !.

heuristique_1(Ville_depart, CH, X, Acc1, Ville_depart, Res, Cout_actuel, Cpt, Path, Cost) :-
  	NewAcc2 is Ville_depart + 1,
  	heuristique_1(Ville_depart, CH, X, Acc1, NewAcc2, Res, Cout_actuel, Cpt, Path, Cost), !.

/* Conditions de fin */

heuristique_1(Ville_depart, Res_Final, _, Acc1, _, _, _, Cpt, Path, Cost) :- Verif_acc1 is Acc1 + 1,
	not(ville(Verif_acc1, _, _)), Cpt >= 2,
  append(Res_Final, [Ville_depart], NewRes), calcul_cout_total(NewRes, NewCost),
	Path = NewRes, Cost = NewCost, !.

heuristique_1(Ville_depart, Res_Final, Cout_final, Acc1, _, _, _, Cpt, Path, Cost) :- Verif_acc1 is Acc1 + 1,
	not(ville(Verif_acc1, _, _)), Cpt < 2, NewCpt is Cpt + 1, heuristique_1(Ville_depart, Res_Final, Cout_final, 1, 2, Res_Final, Cout_final, NewCpt, Path, Cost), !.

/* Lorsqu'on a considéré tous les flips pour le sommet Acc1, on passe au suivant */


heuristique_1(Ville_depart, CH, X, Acc1, Acc2, Res, Cout_actuel, Cpt, Path, Cost) :- not(ville(Acc2, _, _)), NewAcc1 is Acc1 + 1,
                                                                                     heuristique_1(Ville_depart, CH, X, NewAcc1, 1, Res, Cout_actuel, Cpt, Path, Cost), !.
/* Si le flip a amélioré notre solution, on le conserve */

heuristique_1(Ville_depart, CH, X, Acc1, Acc2, _, _, _, Path, Cost) :-
	Follower_1 is Acc1 + 1, ville(Follower_1, _, _),
	permute_2opt(Follower_1, Acc2, CH, NewPath), calcul_cout_total([Ville_depart | NewPath], NewCost), NewCost < X, NewAcc2 is Acc2 + 1,
	heuristique_1(Ville_depart, NewPath, NewCost, Acc1, NewAcc2, NewPath, NewCost, 0, Path, Cost), !.

/* Si le flip n'a pas amélioré notre solution, on l'ignore */

heuristique_1(Ville_depart, CH, X, Acc1, Acc2, Res, Cout_actuel, Cpt, Path, Cost) :-
	Follower_1 is Acc1 + 1, ville(Follower_1, _, _),
	permute_2opt(Follower_1, Acc2, CH, NewPath), calcul_cout_total([Ville_depart | NewPath], NewCost), NewCost >= X, NewAcc2 is Acc2 + 1,
	heuristique_1(Ville_depart, CH, X, Acc1, NewAcc2, Res, Cout_actuel, Cpt, Path, Cost), !.

/* Appel de l'heuristique 1

	Ville_depart : numéro de la ville de départ
	Path : Liste représentant le trajet du voyageur
  Cost : Cout du chemin
	random_permutation permet de mélanger notre solution réalisable. celà rend l'heuristique moins sensible aux minimas locaux.
	Cependant la solution sera a chaque fois différente.

*/

heur_2opt(Ville_depart, Path, Cost) :- init_sol_realisable(Ville_depart, CH, 1), calcul_cout_total([Ville_depart |CH], X), heuristique_1(Ville_depart, CH, X, 1, 2, CH, X, 0, Path, Cost), !.


/* ****************** Algorithme A* ************************/


/* Construction d'une ACM */

/* Crée la matrice à partir des données fourni, trie notre liste des arêtes, puis lance la fonction ACM */
acm(R) :- matrice([],[],1,2,F), tri_insertion(F,T), acm(T,[],R),!.

/* Pour chaque arête et sa complémentaire, on vérifie que son ajout ne crée pas de cycle dans le graphe. Si non on l'ajoute dans l'ACM, on c'est que notre arbre est minimum car la liste est trié dans l'ordre croissant */
acm([],Select,Select).
acm([[A,F,D],[F,A,D]|Q],Select,R) :- cycle(A,F,Select), acm(Q,[[A,F,D],[F,A,D]|Select],R),!.
acm([[A,F,D],[F,A,D]|Q],Select,R) :- acm(Q,Select,R),!.

/* Nous sert à mesurer l'impact de la construcation de la matrice sur le temps de calcul de l'acm */
comparaison2(_):- matrice([],[],1,2,F), tri_insertion(F,_).

/* On regarde si on peut partir du point de départ de l'arête qu'on veut ajouter, si oui on commence a parcourir à partir de ses descandants, si non on ajoute le sommet */
cycle(_,_,[]).
cycle(A,F,Select):- member([A,El,_],Select), cycle([A,El],Select,[],F),!.
cycle(A,_,Select):- not(member([_,A,_],Select)).


cycle([],_,_,_).
/*On cherche tous les descandants et on les ajoute à la pile */
cycle([El|Q], Select, Parcouru,F):- member([El,Suivant,_],Select),
                                    not(member(Suivant,[El|Q])),
                                    not(member(Suivant,Parcouru)),!,
                                    NewPile = [Suivant|[El|Q]],
                                    cycle(NewPile,Select,Parcouru,F),!.

/*On vérifie si l'intersection des nouveaux noeud contient le noeud final, si oui, alors le résulat de cycle est faux (on n'ajoute pas ce sommet) */
cycle([El|Q],_,Parcouru,F):-  intersection([El|Q], Parcouru, [F]),!,
                              false.

/*On ajoute le sommet parcouru à la suite des sommets parcouru et on continue à parcourir la liste des descandants */
cycle([El|Q],Select,Parcouru,F):-  NewParcouru = [El|Parcouru],!,
                                   intersection([F], NewParcouru, []),
                                   cycle(Q,Select,NewParcouru,F),!.


/* Fonction de tri par insertion écrite peandant un TP précédent et adapté à notre problème. */

insertion(X,[],[X]) :- !.
insertion([A,B,X],[[C,D,Y]|L],[[A,B,X],[C,D,Y]|L]):- X=<Y,!.
insertion([A,B,X],[[C,D,Y]|L],[[C,D,Y]|L1]):- X>Y, insertion([A,B,X],L,L1).

tri_insertion([],[]).
tri_insertion([X|L],LT) :-  tri_insertion(L,L1), insertion(X,L1,LT).


/*Algorithme A* */

/* Version avec l'heuristique de distance */
a_etoile_dist(D,R,Res,Cout):-matrice([],[],1,2,M), tri_insertion(M,T), heuristique_dist(T,D,R,Res), calcul_cout_total(Res,Cout).

/* On choisi le noeud suivant et on continue jusqu'à que tous noeuds soit parcouru. */
heuristique_dist(M,D,R,Res):- suivant_dist(M,D,R,Suivant),
                              not(member(Suivant,R)),
                              NewR=[Suivant|R],!,
                              heuristique_dist(M,D,NewR,Res).

heuristique_dist(_,_,R, Res):- Res = R.

/* La fonction suivant nous permet de calculer le noeud suivant en calculant g(n) et h(n) pour chaque noeud suivant qui est vérifié */
suivant_dist(M,D,[Current|Q],Suivant):- findall(El,member([Current,El,_],M),Next),
                                        g1(M,[Current|Q],0,G_Init),
                                        g2(M,Current,Next,G_Init,[],G),
                                        h_distance(Next,D,[Current|Q],H),
                                        evalue(G,H,[],Evaluation),
                                        mini(Evaluation,Suivant,D).

/* Version avec l'heuristique d'acm */
a_etoile_acm(D,R,Res,Cout):- matrice([],[],1,2,M), tri_insertion(M,T), heuristique_acm(T,D,R,Res), calcul_cout_total(Res,Cout).

/* On choisi le noeud suivant et on continue jusqu'à que tous noeuds soit parcouru. */
heuristique_acm(M,D,R,Res):- suivant_acm(M,D,R,Suivant),
                             not(member(Suivant,R)),
                             NewR=[Suivant|R],!,
                             heuristique_acm(M,D,NewR,Res),!.

heuristique_acm(_,_,R, Res):- Res = R.

/* La fonction suivant nous permet de calculer le noeud suivant en calculant g(n) et h(n) pour chaque noeud suivant qui est vérifié */
suivant_acm(M,D,[Current|Q],Suivant):- findall(El,member([Current,El,_],M),Next),
                                       g1(M,[Current|Q],0,G_Init),
                                       g2(M,Current,Next,G_Init,[],G),
                                       h_acm(Next,M,[Current|Q],H),
                                       evalue(G,H,[],Evaluation),
                                       mini(Evaluation,Suivant,D).


/* Liste de fonctions utiles à l'algorithme A* */

/* La fonction G1 calcul le cout du noeud initial jusqu'au noeud courant*/
g1(M,[T1,T2|Q],G1,G):- member([T1,T2,D],M),
                       NewG is D + G1,!,
                       g1(M,[T2|Q],NewG,G).

g1(_,[_],G,Cout):- Cout = G.

/* La fonction G2 calcule la distance  entre le noeud courant et les noeud envisagé pour devenir des noeuds courants */
g2(_,_,[],_,G, G).
g2(M,Current,[T|Q],Init_G, G, Res):- member([Current,T,Dist],M),
                                     Ajout is Init_G + Dist,
                                     NewG = [[T,Ajout]|G],!,
                                     g2(M,Current,Q, Init_G, NewG, Res).

/* La fonction evalu calcule la somme de h(n) et de g(n) pour chacun des noeuds envisagé. */
evalue(_,[],Eval, Eval).
evalue(G,[[Noeud,Cout_H]|Q],Evaluation,Res):- member([Noeud,Cout_G],G),
                                              Cout_total is Cout_G + Cout_H,
                                              NewEval = [[Noeud,Cout_total]|Evaluation],!,
                                              evalue(G,Q,NewEval,Res).


/* La fonction mini sélectionne le noeud candidat qui a la plus faible somme g(n)+h(n) */
mini([],D,D).
mini([[Noeud,Cout]|Q],Suivant,_):- mini(Q,Cout, Noeud,Suivant).

mini([[Noeud,Cout]|Q],Cout_current, _, Suivant):- Cout < Cout_current,!,
                                                  mini(Q,Cout, Noeud, Suivant).
mini([[_,Cout]|Q],Cout_current, Noeud_current, Suivant):- Cout >= Cout_current,!,
                                                          mini(Q,Cout_current, Noeud_current, Suivant).
mini([],_,Noeud_current,Noeud_current).

/* Prédicats utilisés pour les fonctions heuristiques, notamment la première (ACM) */

enlever_arete([], _, []).
enlever_arete([ [A, B, Val] | List1], List2, [[ A, B, Val] | Res]) :- not(member(A, List2)), not(member(B, List2)), enlever_arete(List1, List2, Res), !.
enlever_arete([ [A, _, _] | List1], List2, Res) :- member(A, List2), enlever_arete(List1, List2, Res), !.
enlever_arete([ [_, B, _] | List1], List2, Res) :- member(B, List2), enlever_arete(List1, List2, Res), !.

/* Prédicat calculant le poids de toutes les arêtes d'un ACM */

calculer_cout_acm([], 0).
calculer_cout_acm([ [_, _, Cout_arete] | Acm], Res) :- calculer_cout_acm(Acm, Z) , Res is Z + Cout_arete, !.

/* Prédicat calculant le coût heuristique lié à un sommet */

calculer_cout_acm_heur_sommet(A, Liste_arete, DejaVu, [A, Cout_heur_sommet]) :-
    enlever_arete(Liste_arete, DejaVu, NewList),
    acm(NewList, [], MyAcm),
    calculer_cout_acm(MyAcm, Cout_heur_sommet), !.

/* Prédicat calculant les coûts heuristiques chacun lié à un sommet de la liste Liste_sommets */

calculer_cout_acm_heur_sommets([], _, _, []).
calculer_cout_acm_heur_sommets([A | Liste_sommets], Liste_arete, DejaVu, [Res_sommet | Res]) :-
    not(member(A, DejaVu)), calculer_cout_acm_heur_sommet(A, Liste_arete, [A | DejaVu], Res_sommet),
    calculer_cout_acm_heur_sommets(Liste_sommets, Liste_arete, DejaVu, Res), !.
calculer_cout_acm_heur_sommets([A | Liste_sommets], Liste_arete, DejaVu, Res) :-
    member(A, DejaVu),
    calculer_cout_acm_heur_sommets(Liste_sommets, Liste_arete, DejaVu, Res), !.

/* Première fonction heuristique : on utilise un arbre couvrant de poids minimal pour calculer un coût heuristique pour chaque sommet voisin */

h_acm(Liste_voisin, Liste_arete, DejaVu, Res) :- calculer_cout_acm_heur_sommets(Liste_voisin, Liste_arete, DejaVu, Res).

/* Seconde fonction heuristique : on évalue le coût heuristique d'un noeud grâce à la distance qui le sépare de la ville de départ (on ne considère pas les noeuds déjà visité) */

h_distance([], _, _,[]).
h_distance([A | Liste_voisin], Ville_depart, DejaVu, Res) :- member(A, DejaVu),
                                                             h_distance(Liste_voisin, Ville_depart, DejaVu, Res), !.

h_distance([A | Liste_voisin], Ville_depart, DejaVu, [[A, Cout] | Res]) :- not(member(A, DejaVu)), ville(A, CoorA1, CoorA2), ville(Ville_depart, CoorB1, CoorB2),
                                                                           distance(ville(A, CoorA1, CoorA2), ville(Ville_depart, CoorB1, CoorB2), [A, _, Cout]),
                                                                           h_distance(Liste_voisin, Ville_depart,DejaVu, Res), !.
