/* Lachheb Ismaël
   Quentin Bresson

   Voyageur du commerce
    - Avec méthodes gloutonnes
    - Avec méthode de l'élastique

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

distance(ville(X1,Y1,Z1),ville(X2,Y2,Z2), D) :- M is acos(sin(Y1)*sin(Y2)+cos(Y1)*cos(Y2)*cos(Z1-Z2)), D=[X1,X2,M].

matrice(M,C,F):- ville(X,Y,Z), ville(X2,Y2,Z2), not(X=X2), verifier([X,X2],C), Cnew=[[X,X2],[X2,X]|C], distance(ville(X,Y,Z),ville(X2,Y2,Z2),D), D=[S1,S2,Dist], Mnew=[D,[S2,S1,Dist]|M], matrice(Mnew,Cnew,F),!.
matrice(M,_,F):- F = M.

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

glouton(D):- matrice([],[],F), glouton(D,F,[],0).
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
	si (v, s(v)) est supérieur au cout de (s(v), s(p)) le flip considéré est dit prometteur. On ne considère que les flips prometteurs dans un premier temps
	
	Généralisation à l'algo de Lin-Kernighan (k-opt) possible

	Fonction heuristique 2 : ? :
	
	
	permutation et enlever 
*/
enlever( X, [X|Q], Q).
enlever( X, [Y|Q], [Y|Q1]) :- enlever( X, Q, Q1).

permutation([],[]):- !.
permutation( L, [X|Q1]) :- enlever( X, L, Q), permutation( Q, Q1).

/* Code pour fermer une liste. Apres l'initialisation, une variable non déclarée traine a la fin*/

fermer_liste([]).
fermer_liste([_ | T]) :- fermer_liste(T).

/* Calculer le cout d'une solution */

calcul_cout_total([_ | []], 0).
calcul_cout_total([A | [B | CH]], X) :- distance(ville(A,_,_),ville(B,_,_), Y), calcul_cout_total([B | CH], Z) , X is Y + Z.

/* On trouve une solution réalisable à notre problème (facile car graphe complet : on passe une seule fois par chaque ville) */

init_sol_realisable(_, Acc) :- not(ville(Acc,_,_)).
init_sol_realisable(CH, Acc) :- ville(Acc, _, _), not(member(Acc, CH)), NewAcc is Acc + 1, init_sol_realisable([Acc | CH], NewAcc).
init_sol_realisable(CH, Acc) :- ville(Acc, _, _), member(Acc, CH), NewAcc is Acc + 1, init_sol_realisable(CH, NewAcc), !.

init_sol_realisable(Ville_depart, [Ville_depart | Z], Acc) :- init_sol_realisable([Ville_depart | Z], Acc), fermer_liste(Z).

/* Après avoir obtenu une solution réalisable, on cherche une permutation qui diminuera le cout du voyage (2-opt) */

heuristique_1(CH, X, Acc1, Acc2) :- .

/* Appel de l'heuristique 1
	Ville_depart : numéro de la ville de départ
	CH : Liste représentant le trajet du voyageur
	X : Cout du chemin
*/

a_etoile_1(Ville_depart) :- init_sol_realisable(Ville_depart, CH, 1), calcul_cout_total(CH, X), heuristique_1(CH, X, 1, 2).
