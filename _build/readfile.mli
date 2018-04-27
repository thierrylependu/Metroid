type t;;
type o;;

exception Pas_de_perso;;
exception Trop_de_perso;;
exception Pas_de_porte;;

val o_is_porte : o -> bool;;
val o_is_perso : o -> bool;;
val o_is_enemy : o -> bool;;
val o_is_mur : o -> bool;;
val get_list_of_o : o -> int list;;

val traiter_fichier : string -> o list;;

