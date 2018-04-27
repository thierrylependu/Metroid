open Tsdl;;
open Objet;;
open Readfile;;

type dir = Haut | Bas | Droite | Gauche ;;

type action_depl = Aucune | Saut_normal | Saut_vers_droite | Saut_vers_gauche | Marcher_droite | Marcher_gauche ;;

type action = action_depl * bool;;

exception Va_disparaitre;;

(* corrspond à la vitesse max du perso *)
let vit_h_lim = 3;;
let vit_v_lim = 10;;

type scene = {mutable l : objet list ; gravite : int ; perso : objet ; liste_portes : objet list};;

let detect_collision o1 o2 = (* permet de regarder si 2 objets sont en collision *)
  let r1 = Objet.get_hitbox o1 in
  let r2 = Objet.get_hitbox o2 in
  let x1 = Sdl.Rect.x r1 in (* point en haut à gauche de la hitbox de o1 *)
  let y1 = Sdl.Rect.y r1 in (* point en haut à gauche de la hitbox de o1 *)
  let x2 = Sdl.Rect.x r1 + Sdl.Rect.w r1 in
  let y2 = Sdl.Rect.y r1 + Sdl.Rect.h r1 in
  let x1' = Sdl.Rect.x r2 in (* point en haut à gauche de la hitbox de o2 *)
  let y1' = Sdl.Rect.y r2 in (* point en haut à gauche de la hitbox de o2 *)
  let x2' = Sdl.Rect.x r2 + Sdl.Rect.w r2 in
  let y2' = Sdl.Rect.y r2 + Sdl.Rect.h r2 in
  if x1 < x1' (* cas ou o1 est a gauche de o2 *)
  then
    if x2 <= x1' then false (* les 2 objets ne sont pas en collision *)
    else (* cas ou o1 et o2 sont en collision en x *)
      if y1 < y1' (* cas ou o1 est au dessus de o2 *)
      then (y2 > y1')
      else (* cas ou o1 est en dessous de o2 *) (y2' > y1)
  else (* cas ou o1 est a droite de o2 *)
    if x2' <= x1 then false
    else if y1 < y1' (* cas ou o1 est au dessus de o2 *)
    then (y2 > y1')
    else (* cas ou o1 est en dessous de o2 *) (y2' > y1)
;;

let add_object_to_scene o s = (* rajoute un objet dans la liste d'objets *)
  let new_l = o::(s.l) in
  s.l <- new_l
;;

let get_perso s = (* renvoie le perso de la scene sous forme d'objet *)
  s.perso
;;

let get_list s =(*renvoie la liste d'objets de la scene*)
  s.l
;;

let get_list_porte s = (*renvoie la liste des portes*)
  s.liste_portes
;;

let make_enemy x y = (* renvoie 1 ennemi *)
  let largeur = 40 in
  let hauteur = 60 in
  Objet.create_enemy x y largeur hauteur
;;

let make_scene filename = (* cree la scene avec le nom de fichier donne *)
  let o_list = Readfile.traiter_fichier filename in
  let largeur_perso = 50 (* val a choisir *) in
  let hauteur_perso = 30 (* val a choisir *) in
  let largeur_porte = 50 (* val a choisir *) in
  let hauteur_porte = 50 (* val a choisir *) in
  let sce = {l = [] ;
             gravite = 1 ;
             perso = (Objet.create_perso 0 0 0 0) ;
             liste_portes = []}
  in
  let rec aux acc = function
    | [] -> acc
    | ob::tl ->
       let list_arg = Readfile.get_list_of_o ob in
       if (Readfile.o_is_perso ob) then
         let x = List.nth list_arg 0 in
         let y = List.nth list_arg 1 in
         let pe = Objet.create_perso x y largeur_perso hauteur_perso in
         let new_acc = {acc with perso = pe} in
         aux new_acc tl
       else
         if (Readfile.o_is_enemy ob) then
           let x = List.nth list_arg 0 in
           let y = List.nth list_arg 1 in
           let the_enemy = make_enemy x y in
	   add_object_to_scene the_enemy acc;
           aux acc tl
         else
           if (Readfile.o_is_mur ob) then
             let x = List.nth list_arg 0 in
             let y = List.nth list_arg 1 in
             let larg = List.nth list_arg 2 in
             let haut = List.nth list_arg 3 in
             let mur = Objet.create_mur x y larg haut in
	     add_object_to_scene mur acc;
             aux acc tl
           else
             let x = List.nth list_arg 0 in
             let y = List.nth list_arg 1 in
             let la_porte = Objet.create_porte x y largeur_porte hauteur_porte in
	     let lp = acc.liste_portes in
             let new_acc = {acc with liste_portes = (la_porte::lp)} in
             aux new_acc tl
  in
  aux sce o_list
;;

let affichage s = ();;

let perso_a_terre s = (* regarde si le perso est au sol *)
  let p = s.perso in
  let r = Objet.get_hitbox p in
  let y_bottom = (Sdl.Rect.y r) + (Sdl.Rect.h r)  + 40 in (* la hauteur au niveau du bas de la hitbox du perso *)
  let rec aux = function
    | [] -> false
    | x::tl ->
       if (Objet.is_mur x) then
         let r' = Objet.get_hitbox x in
         if (Sdl.Rect.y r') = y_bottom then
           if (Sdl.Rect.x r > (Sdl.Rect.x r' + Sdl.Rect.w r'))||((Sdl.Rect.x r + Sdl.Rect.w r) < Sdl.Rect.x r') then
             aux tl
           else 
             true
         else aux tl
       else aux tl
  in
  aux (s.l)
;;


let monstre_a_terre s m = (* regarde si le monstre est au sol *)
  let r = Objet.get_hitbox m in
  let y_bottom = (Sdl.Rect.y r) + (Sdl.Rect.h r) in (* la hauteur au niveau du bas de la hitbox du perso *)
  let rec aux = function
    | [] -> false
    | x::tl ->
       if (Objet.is_mur x) then
         let r' = Objet.get_hitbox x in
         if (Sdl.Rect.y r') = y_bottom then
           if (Sdl.Rect.x r > (Sdl.Rect.x r' + Sdl.Rect.w r'))||((Sdl.Rect.x r + Sdl.Rect.w r) < Sdl.Rect.x r') then
             aux tl
           else 
             true
         else aux tl
       else aux tl
  in
  aux (s.l)
;;


let maj_mvt_perso_enemy ob s = (* modifie la position du perso ou de l'ennemi *)
  let new_supposed_ob = ob in
  Objet.change_x_y new_supposed_ob ((Objet.get_x new_supposed_ob)+(Objet.get_vit_h new_supposed_ob)) ((Objet.get_y new_supposed_ob)+(Objet.get_vit_v new_supposed_ob));
  let rec aux = function
    | [] -> []
    | o::tl ->
       if not(o=ob) then
	 if (detect_collision o new_supposed_ob) then o::(aux tl) else aux tl
       else aux tl in
  let l_colli = aux (s.l) in
  if l_colli = [] then
    Objet.change_x_y ob (Objet.get_x new_supposed_ob) (Objet.get_y new_supposed_ob)
  else
    let new_x = ref (Objet.get_x new_supposed_ob) in
    let new_y = ref (Objet.get_y new_supposed_ob) in
    let v_h = Objet.get_vit_h ob in
    let v_v = Objet.get_vit_v ob in
    let rec aux2 = function (* etudie les collisions afin de determiner la nouv hitbox *)
      | [] -> ()
      | o::tl ->
         Objet.change_x_y new_supposed_ob (!new_x) (!new_y);
         let () =
           if detect_collision o new_supposed_ob then
             if (v_h = 0 && not(v_v = 0)) then
               if v_v < 0 then (* cas ou l'objet monte *)
                 new_y := (Objet.get_y o + Sdl.Rect.h (Objet.get_hitbox o))
               else (* cas ou l'objet descend *)
                 new_y := (Objet.get_y o - Sdl.Rect.h (Objet.get_hitbox ob))
             else if (v_v = 0 && not(v_h = 0)) then
               if v_h < 0 then (* cas ou l'objet vient de la droite *)
                 new_x := (Objet.get_x o + Sdl.Rect.w (Objet.get_hitbox o))
               else (* cas ou l'objet vient de la gauche *)
                 new_x := (Objet.get_x o - Sdl.Rect.w (Objet.get_hitbox ob))
             else
	       let colli_zone_x = (* espace de collision en x *)
	         if (Objet.get_vit_h ob = 0) then 0 (* car pas de vit horizontale *)
	         else if (Objet.get_vit_h ob > 0) then (* cas ou l'objet vient de la gauche *)
		   ((!new_x) + (Sdl.Rect.w (Objet.get_hitbox ob)) - (Objet.get_x o))
	         else ((Objet.get_x o) + (Sdl.Rect.w (Objet.get_hitbox o)) - (!new_x))
	       in
	       let colli_zone_y = (* espace de collision en y *)
	         if (Objet.get_vit_v ob = 0) then 0 (* car pas de vit verticale *)
	         else if (Objet.get_vit_v ob > 0) then (* cas ou l'objet vient du haut *)
		   ((!new_y) + (Sdl.Rect.h (Objet.get_hitbox ob)) - (Objet.get_y o))
	         else ((Objet.get_y o) + (Sdl.Rect.h (Objet.get_hitbox o)) - (!new_y))
	       in
	       if colli_zone_y = colli_zone_x then () (* pas de modif *)
	       else
	         if colli_zone_y < colli_zone_x then
		   if (Objet.get_vit_v ob > 0) then
		     new_y := ((Objet.get_y o) - (Sdl.Rect.h (Objet.get_hitbox ob)))
                   else new_y := ( (Objet.get_y o) + (Sdl.Rect.h (Objet.get_hitbox o)) )
	         else
		   if (Objet.get_vit_h ob > 0) then
		     new_x := ((Objet.get_x o) - (Sdl.Rect.w (Objet.get_hitbox ob)))
		   else new_x := ((Objet.get_x o) + ((Sdl.Rect.w (Objet.get_hitbox o))))
	   else () (* pas de modif *)
         in
	 aux2 tl
    in
    aux2 l_colli;
    Objet.change_x_y ob (!new_x) (!new_y)
;;

let make_proj s = (* renvoie 1 projectile *)
  let per = s.perso in
  let fa = Objet.get_facing per in
  let taille = 5 in
  if (Objet.is_facing_left per)
  then Objet.create_projectile (Sdl.Rect.x (Objet.get_hitbox per)-taille) (Sdl.Rect.y (Objet.get_hitbox per)) taille fa
  else Objet.create_projectile (Sdl.Rect.x (Objet.get_hitbox per)+ Sdl.Rect.w (Objet.get_hitbox per)) (Sdl.Rect.y (Objet.get_hitbox per)) taille fa
;;

let maj_mvt_proj p s = (* modifie la position d'un projectile *)
  let new_supposed_proj = p in
  Objet.change_x_y new_supposed_proj (Objet.get_x new_supposed_proj + Objet.get_vit_h new_supposed_proj) (Objet.get_y new_supposed_proj);
  let rec aux = function
    | [] -> false
    | o::tl ->
       if Objet.is_mur o then
         if detect_collision new_supposed_proj o then
           true
         else aux tl
       else
         if Objet.is_enemy o then
           if detect_collision new_supposed_proj o then
             let () = Objet.do_facing_none o in (* l'ennemi devient va etre supprime *)
             true
           else aux tl
         else aux tl
  in
  let va_disparaitre = aux (s.l) in (* on regarde si le projectile rentre en collision avec un ennemi ou un mur *)
  if va_disparaitre then (Objet.do_facing_none p)
  else Objet.change_x_y p (Objet.get_x new_supposed_proj) (Objet.get_y new_supposed_proj)
;;

let update_scene s a = (* met a jour la scene *)
  let (d,va_tirer) = a in
  let perso_au_sol = perso_a_terre s in
  (* changement de la vitesse verticale du perso *)
  (* changement de la vitesse horizontale du perso *)
  let vh = Objet.get_vit_h s.perso in
  let vv = Objet.get_vit_v s.perso in
  let () =
    match d with
    | Aucune ->
       let new_vh =
	 if vh=0 then 0
	 else
           if vh<0 then vh+1
           else vh-1
       in
       let new_vv =
	 if perso_au_sol then
           0
	 else (min (vv+(s.gravite)) vit_v_lim)
       in
       Objet.change_vit_h s.perso new_vh;
       Objet.change_vit_v s.perso new_vv
    | Saut_normal ->
       let new_vh =
	 if vh=0 then 0
	 else
           if vh<0 then vh+1
           else vh-1
       in
       let new_vv =
	 if perso_au_sol then  -(vit_v_lim)
	 else (min (vv+(s.gravite)) vit_v_lim)
       in
       Objet.change_vit_h s.perso new_vh;
       Objet.change_vit_v s.perso new_vv
    | Saut_vers_droite ->
       let new_vh =
	 if vh<=0 then 1
	 else (min vit_h_lim (vh+1))
       in
       let new_vv =
	 if perso_au_sol then
           -(vit_v_lim)
	 else (min (vv+(s.gravite)) vit_v_lim)
       in
       Objet.change_vit_h s.perso new_vh;
       Objet.change_vit_v s.perso new_vv
    | Saut_vers_gauche ->
       let new_vh =
	 if vh>=0 then -1
	 else (max (-vit_h_lim) (vh-1))
       in
       let new_vv =
	 if perso_au_sol then
           -(vit_v_lim)
	 else (min (vv+(s.gravite)) vit_v_lim)
       in
       Objet.change_vit_h s.perso new_vh;
       Objet.change_vit_v s.perso new_vv
    | Marcher_droite ->
       let new_vh =
	 if vh<=0 then 1
	 else (min vit_h_lim (vh+1))
       in
       let new_vv =
	 if perso_au_sol then
           0
	 else (min (vv+(s.gravite)) vit_v_lim)
       in
       Objet.change_vit_h s.perso new_vh;
       Objet.change_vit_v s.perso new_vv
    | Marcher_gauche ->
       let new_vh =
	 if vh>=0 then -1
	 else (max (-vit_h_lim) (vh-1))
       in
       let new_vv =
	 if perso_au_sol then
           0
	 else (min (vv+(s.gravite)) vit_v_lim)
       in
       Objet.change_vit_h s.perso new_vh;
       Objet.change_vit_v s.perso new_vv
  in
  maj_mvt_perso_enemy (s.perso) s; (* on fait bouger le perso *)
  let new_objet_list =
    let rec aux acc = function
      | [] ->
         if va_tirer then
           (make_proj s)::acc
         else
           acc
      | o::tl ->
         if Objet.is_mur o then aux (o::acc) tl
         else
           if Objet.is_proj o then
	     let () = maj_mvt_proj o s in
             if Objet.is_none o then (* on regarde si le projectile doit disparaitre *)
               aux acc tl
             else aux (o::acc) tl
           else
             if Objet.is_none o then (* on regarde si l'ennemi doit disparaitre *)
               aux acc tl
             else
	       let () = maj_mvt_perso_enemy o s in
               aux (o::acc) tl
    in
    List.rev (aux [] (s.l))
  in
  s.l <- new_objet_list
;;

let clear s = (* permet de savoir si le perso est en collision avec une porte *)
  let p = s.perso in
  let lp = s.liste_portes in
  let rec aux = function
    | [] -> false
    | po::tl ->
       if (detect_collision po p) then true
       else aux tl
  in
  aux lp
;;
