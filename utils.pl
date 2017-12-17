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
    (sub_string(String,0,_,_,"NODE_COORD_SECTION") -> /* tant qu'on n'est pas sur une ligne de ville, on skippe */
        read_villes(Str);
        read_info_villes(Str)).

/* read_tsp_file(+NomFichier) avec NomFichier : chaine de caractères */
/* Appel read_tsp_file('dji38.tsp') */

read_tsp_file(NomF) :-
    open(NomF,read,Str),
    retractall(ville(_,_,_)),
    read_info_villes(Str).
