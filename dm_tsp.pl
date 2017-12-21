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


/* ***************** Production de la matrice à partir des données ******************* */

/* Lecture du fichier  */
/* lecture des villes dans un fichier tsplib */
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

/* read_info_villes(+Str) ou Str est un Stream */
/* Lecture du fichier .tsplib */

read_info_villes(Str) :-
    read_string(Str, "\n", "\r", _, String),
/*     write('Chaine lue = '),writeln(String), */
    (sub_string(String,0,_,_,"NODE_COORD_SECTION") -> /*tant qu'on n'est pas sur une ligne de ville, on skippe */
        read_villes(Str);
        read_info_villes(Str)).

/* read_tsp_file(+NomFichier) avec NomFichier : chaine de caractères */
/* Appel read_tsp_file('dji38.tsp') */

read_tsp_file(NomF) :-
    open(NomF,read,Str),
    retractall(ville(_,_,_)),
    read_info_villes(Str).

/* Fonction distance */

matrice(M,C,V1,_,F):- ville(X,Y,Z), ville(X2,Y2,Z2) ,(X2>X), (X>=V1), not(member([X,X2],C)), !, D is acos(sin(Y)*sin(Y2)+cos(Y)*cos(Y2)*cos(Z-Z2)), matrice([[X,X2,D],[X2,X,D]|M],[[X,X2],[X2,X]|C],X,X2, F).
matrice(M,_,_,_,F):- F=M.

/* ****************** Méthode Gloutonne *********************** */

/* Code test :
  glouton(a,[[a,b,4], [c,a,3], [b,c,9], [b,a,4], [a,c,3], [c,b,9]],[],0).
  glouton(D):- Mat is matrice(M,_,F), glouton(D,Mat,[],0).
*/

min([[A,F]|Q],E,N):- min(Q,A,F,E,N).
min([[T,F]|Q],M,_,E,N):- T<M, min(Q,T,F,E,N),!.
min([[T,_]|Q],M,O,E,N):- T>=M, min(Q,M,O,E,N),!.
min([],M,O,M,O).

verifier(Tbis,C) :- not(member(Tbis,C)).

/* La méthode doit toujours aller dans la ville qu'il n'a pas encore visité la plus proche de lui jusq'à avoir visité toutes les villes. */

glouton(D):- matrice([],[],1,2,F), glouton(D,F,[],0).
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

heuristique_1(CH, X, Acc1, Acc2, Res, Cout_actuel) :-
	not(ville(Acc2, _, _)), NewAcc1 is Acc1 + 1,
	heuristique_1(CH, X, NewAcc1, 1, Res, Cout_actuel), !.

/* Cas où on considère un chemin absurde (de la ville A à la ville A) */

heuristique_1(CH, X, Acc1, Acc1, Res, Cout_actuel) :- NewAcc is Acc1 + 1, heuristique_1(CH, X, Acc1, NewAcc, Res, Cout_actuel), !.

/* Si le flip a amélioré notre solution, on le conserve */

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
	Ville_depart : numéro de la ville de départ
	CH : Liste représentant le trajet du voyageur
	X : Cout du chemin
*/


a_etoile_1(Ville_depart) :- init_sol_realisable(Ville_depart, CH, 1), calcul_cout_total(CH, X), heuristique_1(CH, X, Ville_depart, 2, CH, X), !.



a_etoile_isma(D,R,Res):-matrice([],[],1,2,M), heuristique_isma(M,D,R,Res).
/*
a_etoile(D,[D]).
*/



acm(_) :- matrice2([],[],1,2,F), tri_insertion(F,T), acm(T,[],R), write(R),!.
acm([],Select,Select).
acm([[A,F,D],[F,A,D]|Q],Select,R) :- cycle(A,F,Select), acm(Q,[[A,F,D],[F,A,D]|Select],R),!.
acm([[A,F,D],[F,A,D]|Q],Select,R) :- acm(Q,Select,R),!.


comparaison(_):- matrice2([],[],1,2,F), tri_insertion(F,_).



cycle(_,_,[]).
cycle(A,F,Select):- member([A,El,_],Select), cycle([A,El],Select,[],F),!.
cycle(A,_,Select):- not(member([_,A,_],Select)).

cycle([],_,_,_).

cycle([El|Q], Select, Parcouru,F):-
                                    member([El,Suivant,_],Select),
                                    not(member(Suivant,[El|Q])),
                                    not(member(Suivant,Parcouru)),!,
                                    NewPile = [Suivant|[El|Q]],
                                    cycle(NewPile,Select,Parcouru,F),!.


cycle([El|Q],_,Parcouru,F):-  intersection([El|Q], Parcouru, [F]),!,
                              false.

cycle([El|Q],Select,Parcouru,F):-  NewParcouru = [El|Parcouru],
                                   !,
                                   intersection([F], NewParcouru, []),
                                   cycle(Q,Select,NewParcouru,F),!.




:- use_module(library(lists)).

insertion(X,[],[X]) :- !.
insertion([A,B,X],[[C,D,Y]|L],[[A,B,X],[C,D,Y]|L]):- X=<Y,!.
insertion([A,B,X],[[C,D,Y]|L],[[C,D,Y]|L1]):- X>Y, insertion([A,B,X],L,L1).

tri_insertion([],[]).
tri_insertion([X|L],LT) :-  tri_insertion(L,L1), insertion(X,L1,LT).

heuristique_isma(M,D,R,Res):- suivant(M,D,R,Suivant),
                              not(member(Suivant,R)),
                              NewR=[Suivant|R],!,
                              heuristique_isma(M,D,NewR,Res).

heuristique_isma(_,_,R, Res):- Res = R.

suivant(M,D,[Current|Q],Suivant):- findall(El,member([Current,El,_],M),Next),
                                   g1(M,[Current|Q],0,G_Init),
                                   g2(M,Current,Next,G_Init,[],G),
                                   h_acm(Next,M,[Current|Q],H),
                                   evalu(G,H,[],Evaluation),
                                   mini_isma(Evaluation,Suivant,D).


/*
h2()

*/

g1(M,[T1,T2|Q],G1,G):- member([T1,T2,D],M),
                       NewG is D + G1,!,
                       g1(M,[T2|Q],NewG,G).

g1(_,[_],G,Cout):- Cout = G.


g2(_,_,[],_,G, G).


g2(M,Current,[T|Q],Init_G, G, Res):- member([Current,T,Dist],M),
                                     Ajout is Init_G + Dist,
                                     NewG = [[T,Ajout]|G],!,
                                     g2(M,Current,Q, Init_G, NewG, Res).



evalu(_,[],Eval, Eval).

evalu(G,[[Noeud,Cout_H]|Q],Evaluation,Res):- member([Noeud,Cout_G],G),
                                         Cout_total is Cout_G + Cout_H,
                                         NewEval = [[Noeud,Cout_total]|Evaluation],!,
                                         evalu(G,Q,NewEval,Res).



mini_isma([],D,D).
mini_isma([[Noeud,Cout]|Q],Suivant,_):- mini_isma(Q,Cout, Noeud,Suivant).


mini_isma([[Noeud,Cout]|Q],Cout_current, _, Suivant):- Cout < Cout_current,!,
                                                       mini_isma(Q,Cout, Noeud, Suivant).


mini_isma([[_,Cout]|Q],Cout_current, Noeud_current, Suivant):- Cout >= Cout_current,!,
                                                               mini_isma(Q,Cout_current, Noeud_current, Suivant).


mini_isma([],_,Noeud_current,Noeud_current).









/************ Code de quentin *********************/



distance(ville(X1,Y1,Z1),ville(X2,Y2,Z2), D) :- M is sqrt((((Y1-Y2)^2) + ((Z1 - Z2)^2))), D = [X1, X2, M].


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
    tri_insertion(NewList, Sorted_List),
    acm(Sorted_List, [], MyAcm),
    calculer_cout_acm(MyAcm, Cout_heur_sommet), !.

/* Prédicat calculant les coûts heuristiques chacun lié à un sommet de la liste Liste_sommets */

calculer_cout_acm_heur_sommets([], _, _, []).
calculer_cout_acm_heur_sommets([A | Liste_sommets], Liste_arete, DejaVu, [Res_sommet | Res]) :-
    calculer_cout_acm_heur_sommet(A, Liste_arete, [A | DejaVu], Res_sommet),
    calculer_cout_acm_heur_sommets(Liste_sommets, Liste_arete, DejaVu, Res), !.

/* Première fonction heuristique : on utilise un arbre couvrant de poids minimal pour calculer un coût heuristique pour chaque sommet voisin */

h_acm(Liste_voisin, Liste_arete, DejaVu, Res) :- calculer_cout_acm_heur_sommets(Liste_voisin, Liste_arete, DejaVu, Res).

/* Seconde fonction heuristique : on évalue le coût heuristique d'un noeud grâce à la distance qui le sépare de la ville de départ */
/*
h_distance([], _, []).
h_distance([A | Liste_voisin], Ville_depart, [B | Res]):- h_distance(Liste_voisin, Ville_depart, Res), distance(ville(A, CoorA1, CoorA2), ville(Ville_depart, CoorB1, CoorB2), B).

*/
