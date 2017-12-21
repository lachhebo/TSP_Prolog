# TSP_Prolog
Implémentation d'heuristique de résolution du TSP avec Prolog


On trouve dans le fichier dm_tsp.pl un ensemble d'heuristiques :

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


On trouve aussi l'Algorithme A* implementé avec un ACM et un heuristique de distance.  

Enfin les fichiers .tsp sont des données sur lesquelles peuvent être lancé les algorithmes. 
