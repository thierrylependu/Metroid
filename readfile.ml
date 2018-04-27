type t = Perso | Enemy | Mur | Porte ;;
type o = { typ_of_objet : t ; l : int list };;

exception Ligne_erronee;;
exception Pas_de_perso;;
exception Trop_de_perso;;
exception Pas_de_porte;;

let o_is_porte ob = (ob.typ_of_objet = Porte);;
let o_is_perso ob = (ob.typ_of_objet = Perso);;
let o_is_enemy ob = (ob.typ_of_objet = Enemy);;
let o_is_mur ob = (ob.typ_of_objet = Mur);;
let get_list_of_o ob = ob.l ;;

let separateur = ' ';;

let traiter_ligne li =
  let lst = String.split_on_char separateur li in (* on separe la ligne en mots *)
  let rec aux acc = function
    | [] -> acc
    | s::tl ->
       match s with
       | "" -> aux acc tl
       | _ -> aux (s::acc) tl
  in
  let list_sans_mot_vide = List.rev (aux [] lst) in
  let fst_wd = List.hd list_sans_mot_vide in (* on regarde le 1er mot de la ligne*)
  match fst_wd with
  | "p" ->
     let n_int = (List.length list_sans_mot_vide)-1 in
     if not(n_int=2) then raise Ligne_erronee
     else
       let new_l = [int_of_string(List.nth list_sans_mot_vide 1);
                    int_of_string(List.nth list_sans_mot_vide 2)] in
       { typ_of_objet = Perso ; l = new_l }
  | "e" ->
     let n_int = (List.length list_sans_mot_vide)-1 in
     if not(n_int=2) then raise Ligne_erronee
     else
       let new_l = [int_of_string(List.nth list_sans_mot_vide 1);
                    int_of_string(List.nth list_sans_mot_vide 2)] in
       { typ_of_objet = Enemy ; l = new_l }
  | "m" ->
     let n_int = (List.length list_sans_mot_vide)-1 in
     if not(n_int=4) then raise Ligne_erronee
     else
       let new_l = [int_of_string(List.nth list_sans_mot_vide 1);
                    int_of_string(List.nth list_sans_mot_vide 2);
                    int_of_string(List.nth list_sans_mot_vide 3);
                    int_of_string(List.nth list_sans_mot_vide 4)] in
       { typ_of_objet = Mur ; l = new_l }
  | "d" ->
     let n_int = (List.length list_sans_mot_vide)-1 in
     if not(n_int=2) then raise Ligne_erronee
     else
       let new_l = [int_of_string(List.nth list_sans_mot_vide 1);
                    int_of_string(List.nth list_sans_mot_vide 2)] in
       { typ_of_objet = Porte ; l = new_l }
  | _ -> raise Ligne_erronee
;;

let traiter_fichier filename = (* renvoie une liste d'objets avec le type et les donnees pour le creer dans la scene *)
  let chan = open_in filename in
  let perso_trouve = ref false in
  let porte_trouve = ref false in
  let rec aux c acc =
    try
      let l = input_line c in
      if (String.length l) > 0
      then
        let fst_char_line = String.get l 0 in (* on regarde le 1er char de la ligne *)
        if not(fst_char_line = 'p'
               || fst_char_line = 'e'
               || fst_char_line = 'm'
               || fst_char_line = 'd')
        then raise Ligne_erronee
        else
          let new_elt = traiter_ligne l in
          if (o_is_perso new_elt) then
            if (not !perso_trouve) then
              perso_trouve := true
            else raise Trop_de_perso
          else if (o_is_porte new_elt) then
            porte_trouve := true
          else ()
          ;
            aux c (new_elt::acc)
      else raise Ligne_erronee(* cas ou la ligne est vide *)
    with
    | End_of_file -> (* fin du fichier *)
       if (not !perso_trouve) then raise Pas_de_perso
       else if (not !porte_trouve) then raise Pas_de_porte
       else ()
      ;
        acc
    | Ligne_erronee -> aux c acc (* cas ou la ligne est syntaxiquement incorrecte, on ne fait rien *)
    | Failure _ -> aux c acc (* cas ou on a essayé de lire un entier qui n'en est pas un *)
  in
  let l_final = List.rev (aux chan []) in
  l_final
;;

(*
Les lignes du fichier prises en compte doivent un des formats suivants :
A)p x y (x,y entiers) -> renvoie un perso dont l'origine de sa hitbox (en haut a gauche) sera en x y
B)e x y (x,y entiers) -> renvoie un ennemi dont l'origine de sa hitbox (en haut a gauche) sera en x y
C)m x y l h (x,y,l,h entiers) -> renvoie un mur dont l'origine de sa hitbox (en haut a gauche) sera en x y , sa largeur sera l et sa hauteur h
D)d x y (x,y entiers) -> renvoie une porte dont l'origine de sa hitbox (en haut a gauche) sera en x y
Les lignes ne suivant pas l'un ces formats seront ignorées
De plus, le ficher sera valide si et seulement si il y a une seule et unique ligne de format A(pour le perso) ET au moins une ligne de format D(pour la porte)
*)
