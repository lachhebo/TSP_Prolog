/*
  Ismaël Lachheb
  Quentin Bresson
  Les arêtes, de la forme dist(X, Y, Z) où :
    X : un sommet
    Y : un autre sommet
    Z : le cout associé au trajet direct de X vers Y

*/

/*
Résultat pour 10 villes :

[[6,10,1.2400804234203895],[10,6,1.2400804234203895],
[1,7,0.8923867940259624],[7,1,0.8923867940259624],
[2,5,0.7094607435438184],[5,2,0.7094607435438184],
[3,4,0.6788940750885002],[4,3,0.6788940750885002],
[1,9,0.6605370334316085],[9,1,0.6605370334316085],
[8,10,0.6552787085366004],[10,8,0.6552787085366004],
[4,6,0.645977005134641],[6,4,0.645977005134641],
[4,7,0.6052461872650327],[7,4,0.6052461872650327],
[2,8,0.18118827429745737],[8,2,0.18118827429745737]]

[[6,10,1.2400804234203895],[10,6,1.2400804234203895],
[1,7,0.8923867940259624],[7,1,0.8923867940259624],
[2,5,0.7094607435438184],[5,2,0.7094607435438184],
[3,4,0.6788940750885002],[4,3,0.6788940750885002],
[1,9,0.6605370334316085],[9,1,0.6605370334316085],
[8,10,0.6552787085366004],[10,8,0.6552787085366004],
[4,6,0.645977005134641],[6,4,0.645977005134641],
[4,7,0.6052461872650327],[7,4,0.6052461872650327],
[2,8,0.18118827429745737],[8,2,0.18118827429745737]]



*/

/*
*/

:- use_module(library(lists)).

insertion(X,[],[X]) :- !.
insertion([A,B,X],[[C,D,Y]|L],[[A,B,X],[C,D,Y]|L]):- X=<Y,!.
insertion([A,B,X],[[C,D,Y]|L],[[C,D,Y]|L1]):- X>Y, insertion([A,B,X],L,L1).

tri_insertion([],[]).
tri_insertion([X|L],LT) :-  tri_insertion(L,L1), insertion(X,L1,LT).

/* La fonction de cycle associé à l'acm */

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


/*
cycle([El|_],Select,Parcouru):- member([El,Suivant,_],Select),
                                member(Suivant,Parcouru),
                                false.
*/
/*
inter([],_, []).
inter([X1|X2], L,[X1|T]):- member(X1, L),!, inter(X2, L,T).
inter([_|X2], L,T):- inter(X2, L,T).

[[1,5,1.972676429935048],[5,1,1.972676429935048],
[2,5,1.3046667588799097],[5,2,1.3046667588799097],
[3,4,0.8715212348676055],[4,3,0.8715212348676055],
[2,4,0.5240453634122028],[4,2,0.5240453634122028]]


*/
/*
suivant(El,Select,New):- member([El,New,_],Select).
*/


matrice2(M,C,V1,_,F):- ville(X,Y,Z), ville(X2,Y2,Z2) ,(X2>X), (X>=V1), not(member([X,X2],C)), !, D is acos(sin(Y)*sin(Y2)+cos(Y)*cos(Y2)*cos(Z-Z2)), matrice2([[X,X2,D],[X2,X,D]|M],[[X,X2],[X2,X]|C],X,X2, F).
matrice2(M,_,_,_,F):- F=M.

/*
vidage([[A,B,C]|Q],I,R):- member([B,A,C],Q), enlever([B,A,C],Q,FF), vidage(FF,[[A,B,C]|I],R).
vidage([],I,R):- R=I.
*/

acm(_) :- matrice2([],[],1,2,F), tri_insertion(F,T), acm(T,[],R), write(R),!.
acm([],Select,Select).
acm([[A,F,D],[F,A,D]|Q],Select,R) :- cycle(A,F,Select), acm(Q,[[A,F,D],[F,A,D]|Select],R),!.
acm([[A,F,D],[F,A,D]|Q],Select,R) :- acm(Q,Select,R),!.


comparaison(_):- matrice2([],[],1,2,F), tri_insertion(F,_).
/*
acm([[Racine,_,_]|Ferme],L):- [Ouvert|Ferme]=[[Depart, Arrive, Cout]|Queue], not(cycle([Depart,Arrive,Cout], L)), acm(Queue, [[Depart,Arrive,Cout]|L]), !.
acm([[Racine,_,_]|Ferme],L):- [Ouvert|Ferme]=[[Depart, Arrive, Cout]|Queue], cycle([Depart,Arrive,Cout], L), acm(Queue,L), !.
*/

/*
La distance entre 


*/
