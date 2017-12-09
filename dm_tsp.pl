/* Lachheb Ismaël
   Quentin Bresson

   Voyageur du commerce
    - Avec méthodes gloutonnes
    - Avec méthode de l'élastique

*/
/* On considère un graphe complet */

dist(a,b, 4).
dist(c,a,3).
dist(b,c,9).

dist(b,a, 4).
dist(a,c,3).
dist(c,b,9).


/* ****************** Méthode Gloutonne *********************** */

/* Code test :
  glouton(a,[[a,b,4], [c,a,3], [b,c,9], [b,a,4], [a,c,3], [c,b,9]],[],0).
*/

min([[A,F]|Q],E,N):- min(Q,A,F,E,N).
min([[T,F]|Q],M,_,E,N):- T<M, min(Q,T,F,E,N),!.
min([[T,_]|Q],M,O,E,N):- T>=M, min(Q,M,O,E,N),!.
min([],M,O,M,O).

verifier(Tbis,C) :- not(member(Tbis,C)).

/* La méthode doit toujours aller dans la ville qu'il n'a pas encore visité la plus proche de lui jusq'à avoir visité toutes les villes. */

glouton(A,[[M1,M2,M3]|Q],L,C):- distmin(A,[[M1,M2,M3]|Q],[],E,N,L),NewC is E+C, NewL=[A|L], glouton(N,[[M1,M2,M3]|Q],NewL,NewC),!.

/*Lorsque on a fini de parcourir le graphe */
glouton(A,[[_,_,_]|_],L,C):- NewL=[A|L],write(NewL), write( C).

distmin(A,[[M,F,D]|Q],C,E,N,L):- M=A,verifier(F,L),distmin(A,Q,[[D,F]|C],E,N,L),!.
distmin(A,[[_,_,_]|Q],R,E,N,L):- distmin(A,Q,R,E,N,L),!.

/*
distmin(a,[[M,_,D]],[C], E):- M=a, distmin(a,[],[D|C],E).
distmin(a,[[M,_,D]],[C], E):- not(M=a), distmin(a,[],[D|C],E).
*/

distmin(_,[],S,E,N,_):- min(S,E,N).

/* ****************** Méthode A* *********************** */
