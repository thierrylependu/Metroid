open Tsdl;;
type objet;;
type face;;
type o_type;;

val create_perso : int -> int -> int -> int -> objet;;
val create_mur : int -> int -> int -> int -> objet;;
val create_projectile : int -> int -> int -> face -> objet;;
val create_enemy : int -> int -> int -> int -> objet;;
val create_porte : int -> int -> int -> int -> objet;;

val is_porte : objet -> bool;;
val is_perso : objet -> bool;;
val is_proj : objet -> bool;;
val is_mur : objet -> bool;;
val is_enemy : objet -> bool;;
val is_facing_left : objet -> bool;;
val is_facing_right : objet -> bool;;
val is_none : objet -> bool;;

val get_hitbox : objet -> Sdl.rect;;
val get_facing : objet -> face;;
val get_type : objet -> o_type;;
val get_vit_v : objet -> int;;
val get_vit_h : objet -> int;;
val get_x : objet -> int;;
val get_y : objet -> int;;

val change_vit_h : objet -> int -> unit;;
val change_vit_v : objet -> int -> unit;;
val change_x_y : objet -> int -> int -> unit;;

val do_facing_r : objet -> unit;;
val do_facing_l : objet -> unit;;
val do_facing_none : objet -> unit;;

val make_face_none : objet -> objet;;
