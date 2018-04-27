open Tsdl;;

type face = Right | Left | None ;; (* permet de savoir vers où regardent les ennemis/perso *)
type o_type = Perso | Enemy | Mur | Projectile | Porte;; (* type de l'objet *)

type objet = {mutable r : Sdl.rect (* hitbox *); mutable vit_h : int ; mutable vit_v : int ; typ : o_type ; mutable facing : face}

let create_perso x y largeur hauteur = {r = Sdl.Rect.create x y largeur hauteur ; vit_h = 0 ; vit_v = 0 ; typ = Perso ; facing = Right}
let create_mur x y largeur hauteur = {r = Sdl.Rect.create x y largeur hauteur ; vit_h = 0 ; vit_v = 0 ; typ = Mur ; facing = None}
let create_projectile x y taille fa = if fa = Right then {r = Sdl.Rect.create (x+80) y taille taille ; vit_h = 5 ; vit_v = 0 ; typ = Projectile ; facing = Right} 
                                      else {r = Sdl.Rect.create (x-15) y taille taille ; vit_h = -5 ; vit_v = 0 ; typ = Projectile ; facing = Left }
let create_enemy x y largeur hauteur = {r = Sdl.Rect.create x y largeur hauteur ; vit_h = 1 ; vit_v = 0 ; typ = Enemy ; facing = Left }
let create_porte x y largeur hauteur = {r = Sdl.Rect.create x y largeur hauteur ; vit_h = 0 ; vit_v = 0 ; typ = Porte ; facing = None}

let is_porte ob = (ob.typ = Porte);;
let is_perso ob = (ob.typ = Perso);;
let is_proj ob = (ob.typ = Projectile);;
let is_mur ob = (ob.typ = Mur);;
let is_enemy ob = (ob.typ = Enemy);;
let is_facing_left ob = (ob.facing = Left);;
let is_facing_right ob = (ob.facing = Right);;
let is_none ob = (ob.facing = None);;

let get_hitbox ob = ob.r;;
let get_facing ob = ob.facing;;
let get_type ob = ob.typ;;
let get_vit_v ob = ob.vit_v;;
let get_vit_h ob = ob.vit_h;;
let get_x ob = Sdl.Rect.x (get_hitbox ob);;
let get_y ob = Sdl.Rect.y (get_hitbox ob);;

let change_vit_h ob n = if n <= (if is_perso ob then 5 else 2) && n >= (if is_perso ob then -5 else -2) then ob.vit_h <- n;;
let change_vit_v ob n = if n <= 5 && n >= -5 then ob.vit_v <- n ;;
let change_x_y ob x y =
  let new_hitbox = Sdl.Rect.create x y (Sdl.Rect.w (ob.r)) (Sdl.Rect.h (ob.r)) in
  ob.r <- new_hitbox
;;

let do_facing_r ob =  ob.facing <- Right ;;
let do_facing_l ob =  ob.facing <- Left ;;
let do_facing_none ob =  ob.facing <- None ;;

let make_face_none ob = {ob with facing = None};;

