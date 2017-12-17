# TSP_Prolog
Implémentation d'heuristique de résolution du TSP avec Prolog


Comme on peut le voir, c'est surtout la construction de la matrice qui a une complexité absolument monstrueuse, pour construire la matrice associé à 38 noeuds dans notre graphe,il a fallu 41.177 secondes et 41.307 secondes pour obtenir le résultat du glouton et construire la matrice. On peut donc déduire qu'indépendamment de la construction de la matrice, il a falu 0.130 secondes à l'algorithme glouton pour obtenir une solution. Reste à juger de la qualité de l'heuristique gloutonne, d'abord par rapport à une relaxation du problème et ensuite par rapport à l'algorithme a_star. 


?- time(glouton(1)).
  [18,31,27,38,11,16,6,28,20,34,25,26,10,33,8,2,37,17,5,30,32,21,24,13,15,3,12,4,7,14,36,19,29,9,35,23,22,1] coût total =15.862156085446006
  % 471,325,822 inferences, 41.307 CPU in 41.309 seconds (100% CPU, 11410193 Lips)
  true.

?- time(matrice([],[],F)).
  % 471,247,731 inferences, 41.176 CPU in 41.177 seconds (100% CPU, 11444655 Lips)
  F = [[38, 37, 0.7606720706033152], [38, 36, 2.5894180719712314], [38, 35, 1.5154669590986527], [38, 34, 1.7214806216957887], [38, 33, 0.8539396290416041], [38, 32, 0.6591749180553383], [38, 31|...], [38|...], [...|...]|...].
