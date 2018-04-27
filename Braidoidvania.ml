open Tsdl;;
open Result;;
open Scene;;
open Readfile;;
open Objet;;


match Sdl.init Sdl.Init.video with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
| Ok () ->
   
   match Sdl.create_window ~w:800 ~h:600 "Braidoidvania" Sdl.Window.opengl with
   | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
   | Ok w -> 
      
      let rend = Sdl.create_renderer w ~flags:Sdl.Renderer.(presentvsync + accelerated)  in
      match rend  with
      |Error (`Msg e) -> Sdl.log "Create render error %s" e; exit 1
      |Ok r -> 

	 Sdl.delay 1000l; (* attendre 1000ms *)
	
	 (*ferme le jeu et quitte le programme*)
	 let quit () =
	   Sdl.destroy_window w;
	   Sdl.quit ();
	   exit 0
	 in

   
	 (*supprime tout les elements du renderer*)
	 let clear_rend () =
	   match Sdl.render_clear r with
	   |Error (`Msg e) -> Sdl.log "Render clear error %s" e; exit 1
	   |Ok _ ->  Sdl.render_present r
	 in

	 
      



	 (*creer l'image d'un block en x y*)
	 let make_block tx rect l h =
		 match Sdl.render_copy ~dst:rect r tx with
		 |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
		 |Ok rcopy ->
		    rcopy;	       
	 in

	 (*affiche tout les blocs du niveau*)
	 let affiche_all_block tx scene rect =
	   let l = Scene.get_list scene in
	   let rec affiche_block objl copyl=
	     match objl with
	     |[] -> ()
	     |hd :: tl ->
		if Objet.is_mur hd then begin
		  let rect = Objet.get_hitbox hd in
		  let l = Sdl.Rect.w rect in
		  let h = Sdl.Rect.h rect in
		  let cp = make_block tx rect l h in
		  affiche_block tl (cp::copyl);
		end
		else affiche_block tl copyl
	   in
	   affiche_block l [];
	 in


	 (*Affiche la fin du niveau sous la forme d'une porte*)
	 let affiche_porte tx scene =
	   let l = Scene.get_list_porte scene in
	   let rec get_porte list =
	     match list with
	     |[] -> Printf.printf"Pas de porte dans la scene \n";quit (); Objet.create_porte 0 0 0 0
	     |hd :: tl ->
		if Objet.is_porte hd then hd
		else get_porte tl
	   in
	   let porte = get_porte l in
	   let rectS = Sdl.Rect.create 205 30 75 135 in
	   let rectD = Sdl.Rect.create (Objet.get_x porte) (Objet.get_y porte) 45 70 in
	   match Sdl.render_copy ~src:rectS  ~dst:rectD r tx with
	   |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	   |Ok rcopy ->
	      rcopy;	
	 in

	 (*Affiche un projectile à l'ecran*)
	  let make_bullet tx x y =
		 let rect = Sdl.Rect.create x y 20 20 in
		 match Sdl.render_copy ~dst:rect r tx with
		 |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
		 |Ok rcopy ->
		    rcopy;	       
	 in
	 
	 
	 (*Creer l'animation du monstre*)
	 let createAnimMonster monster tx  =
	   let x = Objet.get_x monster in
	   let y = Objet.get_y monster in
	   let rectPos = Sdl.Rect.create x y 60 60 in
	   if  Objet.is_facing_left monster then begin
	     match Sdl.render_copy ~dst:rectPos r tx  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
		rcopy
	   end
	   else begin
	     match Sdl.render_copy_ex ~dst:rectPos r tx 0.0 None Sdl.Flip.horizontal  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
		rcopy
	   end;	    
	 in

	 (*Affiche tous les monstres à l'écran*)
	 let affiche_all_monsters scene tx =
	   let l = Scene.get_list scene in
	   let rec affiche_monster l =
	     match l with
	     |[] -> ()
	     |hd :: tl ->
		if Objet.is_enemy hd then
		  createAnimMonster hd tx;	     
	       affiche_monster tl;
	   in
	   affiche_monster l;
	 in


	 (*Creer une animation lorsque le joueur ne se déplace pas*)
	 let animRepos tx scene =
	   let perso = Scene.get_perso scene in
	   let rectPos = Sdl.Rect.create (Objet.get_x (Scene.get_perso scene)) (Objet.get_y (Scene.get_perso scene)) 80 80 in
	   let rect = Sdl.Rect.create 0 40 120 130 in
	   if Objet.is_facing_right perso  then begin
	     match Sdl.render_copy ~src:rect ~dst:rectPos r tx  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
		rcopy
	   end
	   else begin
	     match Sdl.render_copy_ex ~src:rect ~dst:rectPos r tx 0.0 None Sdl.Flip.horizontal  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
		rcopy
	   end;
	 in
	 

	 (*Creer une animation de deplacement*)
	 let animMarche tx scene nb=
	   let numAnim = nb / 6 in
	   let perso = Scene.get_perso scene in
	   let x = Objet.get_x perso in
	   let y = Objet.get_y perso in
	   let rectPos = Sdl.Rect.create x y 80 80 in
	   let rect1 = Sdl.Rect.create 20 220 120 130 in
	   let rect2 = Sdl.Rect.create 155 220 120 130 in
	   let rect3 = Sdl.Rect.create 290 220 120 130 in
	   let rect4 = Sdl.Rect.create 425 220 120 130 in
	   let rect5 = Sdl.Rect.create 555 220 120 130 in
	   let rect6 = Sdl.Rect.create 670 220 120 130 in
	   let rect7 = Sdl.Rect.create 790 220 120 130 in
	   let rect8 = Sdl.Rect.create 915 220 120 130 in
	   let rect9 = Sdl.Rect.create 1030 220 120 130 in
	   let rect10 = Sdl.Rect.create 20 370 120 130 in
	   let rect11 = Sdl.Rect.create 145 370 120 130 in
	   let rect12 = Sdl.Rect.create 260 370 120 130 in
	   let rect13 = Sdl.Rect.create 380 370 120 130 in
	   let rect14 = Sdl.Rect.create 500 370 120 130 in
	   let rect15 = Sdl.Rect.create 630 370 120 130 in
	   let rect16 = Sdl.Rect.create 750 370 120 130 in
	   let rect17 = Sdl.Rect.create 875 370 120 130 in
	   let rect18 = Sdl.Rect.create 990 370 120 130 in
	   let animList = [|rect1; rect2; rect3; rect4; rect5; rect6; rect7; rect8; rect9; rect10; rect11; rect12; rect13;rect14; rect15; rect16; rect17; rect18|] in
	   if Objet.is_facing_right perso  then begin
	     match Sdl.render_copy ~src:animList.(numAnim) ~dst:rectPos r tx  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
		rcopy
	   end
	   else begin
	     match Sdl.render_copy_ex ~src:animList.(numAnim) ~dst:rectPos r tx 0.0 None Sdl.Flip.horizontal  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
		rcopy
	   end;
	 in

	 (*Creer une animation de saut*)
	 let animSaut tx scene nb =
	   let numAnim = nb/25 in
	   let perso = Scene.get_perso scene in
	   let x = Objet.get_x perso in
	   let y = Objet.get_y perso in
	   let rectPos = Sdl.Rect.create x y 80 80 in
	   let rect1 = Sdl.Rect.create 1200 540 130 150 in
	   let rect2 = Sdl.Rect.create 1330 540 130 150 in
	   let rect3 = Sdl.Rect.create 1465 540 130 150 in
	   let rect4 = Sdl.Rect.create 1600 540 130 150 in
	   let rect5 = Sdl.Rect.create 1735 540 130 150 in
	   let animList = [|rect1; rect2; rect3; rect4; rect5|] in
	   if  Objet.is_facing_right perso  then begin
	     match Sdl.render_copy ~src:animList.(numAnim) ~dst:rectPos r tx  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
	       rcopy
	   end
	   else begin
	     match Sdl.render_copy_ex ~src:animList.(numAnim) ~dst:rectPos r tx 0.0 None Sdl.Flip.horizontal  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
	       rcopy
	   end;
	 in
	 
	 (*Creer une animation de chute*)
	 let animTombe tx scene nb =
	   let numAnim = nb/8 in
	   let perso = Scene.get_perso scene in
	   let x = Objet.get_x perso in
	   let y = Objet.get_y perso in
	   let rectPos = Sdl.Rect.create x y 80 80 in
	   let rect1 = Sdl.Rect.create 45 875 130 150 in
	   let rect2 = Sdl.Rect.create 170 875 130 150 in
	   let rect3 = Sdl.Rect.create 290 875 130 150 in
	   let rect4 = Sdl.Rect.create 490 875 130 150 in
	   let rect5 = Sdl.Rect.create 600 875 130 150 in
	   let rect6 = Sdl.Rect.create 730 875 130 150 in
	   let rect7 = Sdl.Rect.create 850 875 130 150 in
	   let rect8 = Sdl.Rect.create 960 875 130 150 in
	   let rect9 = Sdl.Rect.create 1085 875 130 150 in
	   let rect10 = Sdl.Rect.create 1205 875 130 150 in
	   let rect11 = Sdl.Rect.create 1330 875 130 150 in
	   let rect12 = Sdl.Rect.create 1450 875 130 150 in
	   let animList = [|rect1; rect2; rect3; rect4; rect5; rect6; rect7; rect8; rect9; rect10; rect11; rect12|] in
	   if  Objet.is_facing_right perso then begin
	     match Sdl.render_copy ~src:animList.(numAnim/12) ~dst:rectPos r tx  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
		rcopy
	   end
	   else begin
	  match Sdl.render_copy_ex ~src:animList.(numAnim/12) ~dst:rectPos r tx 0.0 None Sdl.Flip.horizontal  with
	  |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	  |Ok rcopy ->
	     rcopy
	end;
	 in
	 
	 (*Creer une animation de tir*)
	 let animTir tx scene nb =
	   let numAnim = nb/10 in
	   let perso = Scene.get_perso scene in
	   let x = Objet.get_x perso in
	   let y = Objet.get_y perso in
	   let rectPos = Sdl.Rect.create x y 80 80 in
	   let rect1 = Sdl.Rect.create 15 1035 130 150 in
	   let rect2 = Sdl.Rect.create 130 1035 130 150 in
	   let rect3 = Sdl.Rect.create 250 1035 130 150 in
	   let rect4 = Sdl.Rect.create 430 1035 130 150 in
	   let rect5 = Sdl.Rect.create 590 1035 130 150 in
	   let rect6 = Sdl.Rect.create 740 1035 130 150 in
	   let rect7 = Sdl.Rect.create 870 1035 130 150 in
	   let rect8 = Sdl.Rect.create 1005 1035 130 150 in
	   let rect9 = Sdl.Rect.create 1180 1035 130 150 in
	   let rect10 = Sdl.Rect.create 1285 1035 130 150 in
	   let rect11 = Sdl.Rect.create 1415 1035 130 150 in
	   let animList = [|rect1; rect2; rect3; rect4; rect5; rect6; rect7; rect8; rect9; rect10; rect11|] in 
	   if Objet.is_facing_right perso  then begin
	     match Sdl.render_copy ~src:animList.(numAnim) ~dst:rectPos r tx  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
		rcopy
	   end
	   else begin
	     match Sdl.render_copy_ex ~src:animList.(numAnim) ~dst:rectPos r tx 0.0 None Sdl.Flip.horizontal  with
	     |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	     |Ok rcopy ->
		rcopy
	   end;
	 in

	 (*Creer une partie de scene affiche par camera*)
	 let create_background tx =
	   (*Creer le rectangle qu'affichera la camera*)
	   let rect = Sdl.Rect.create 200 0 800 750 in
	   (*Affiche la camera*)
	   match Sdl.render_copy ~src:rect r tx with
	   |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
	   |Ok rcopy ->
	      rcopy;
	 in
	 
	 (*Creer la camera du jeu*)
	 let create_camera scene =
	   let perso = Scene.get_perso scene in
	   let taille = Objet.get_x perso + 25 - 400 in
	   let cam = Sdl.Rect.create 0 0 800 600 in
	   let x = if taille < 0 then 0 else if taille > 1500 - 800 then 1500 - 800 else taille in
	   
	   Sdl.Rect.set_x cam (x - 25 - 400);
	   cam	     
	   in



	 
	 (*differencie les touches appuyées sur le clavier*)
	 let getEvent () =
	   
	   let quit_touch = Sdl.get_scancode_from_key Sdl.K.q in
	   let start_touch = Sdl.get_scancode_from_key Sdl.K.return in
	   let pause_touch = Sdl.get_scancode_from_key Sdl.K.escape in
	   let right_touch = Sdl.get_scancode_from_key Sdl.K.right in
	   let left_touch = Sdl.get_scancode_from_key Sdl.K.left in
	   let jump_touch = Sdl.get_scancode_from_key Sdl.K.up in
	   let bullet_touch = Sdl.get_scancode_from_key Sdl.K.space in
	   
	   Sdl.pump_events ();
	   let key_state = Sdl.get_keyboard_state() in
	   let evt = ref "none" in
	   if key_state.{quit_touch} = 1 then evt := "q"
	   else if key_state.{start_touch} = 1 then evt := "start"
	   else if key_state.{pause_touch} = 1 then evt := "esc"
	   else if key_state.{bullet_touch} = 1 then evt := "sp"
	   else if key_state.{jump_touch} = 1 && key_state.{right_touch} = 1 then evt := "jpr"
	   else if key_state.{jump_touch} = 1 && key_state.{left_touch} = 1 then evt := "jpl"
	   else if key_state.{jump_touch} = 1 then evt := "jp"
	   else if key_state.{right_touch} = 1 then evt := "r"
	   else if key_state.{left_touch} = 1 then evt := "l";
	   !evt
	 in




	 (*cree l'image du menu du jeu*)
	 let make_menu () =
	   match Sdl.load_bmp "menu.bmp"  with
	   |Error (`Msg e) -> Sdl.log "Load bmp error %s" e; exit 1
	   |Ok i ->
	      match Sdl.create_texture_from_surface r i with
	      |Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
	      |Ok tx ->
		 match Sdl.render_copy r tx with
		 |Error (`Msg e) -> Sdl.log "Render copy error %s" e; exit 1
		 |Ok rcopy ->
		    rcopy;
	 in

	 (*lance le menu du jeu*)
	 let start_menu () =
	   make_menu ();
	   Sdl.render_present r;
	   let wait_start = ref true in
	   while !wait_start do
	     let evt = getEvent () in
	     if evt = "start" then wait_start := false;
	     if evt = "q" then quit ();
	   done;
	 in


	 (*Detecte les collisions avec un mur*)
	 let collision objet scene =
	   let collisionRect r1 r2 =
	     let leftA = Sdl.Rect.x r1 in
	     let rightA = Sdl.Rect.x r1 + Sdl.Rect.w r1 in
	     let topA = Sdl.Rect.y r1 in
	     let bottomA = Sdl.Rect.y r1 + Sdl.Rect.h r1 in
	     let leftB = Sdl.Rect.x r2 in
	     let rightB = Sdl.Rect.x r2 + Sdl.Rect.w r2 in
	     let topB = Sdl.Rect.y r2 in
	     let bottomB = Sdl.Rect.y r2 + Sdl.Rect.h r2 in
	     if bottomA <= topB then false
	     else if topA >= bottomB then false
	     else if rightA <= leftB then false
	     else if leftA >= rightB then false
	     else true
	   in
	   
	   let hitboxObj = Objet.get_hitbox objet in
	   let rectObj = if Objet.is_perso objet || Objet.is_enemy objet then Sdl.Rect.create (if Objet.is_facing_right objet then Sdl.Rect.x hitboxObj + Sdl.Rect.w hitboxObj +38 else Sdl.Rect.x hitboxObj)
	     (Sdl.Rect.y hitboxObj) (2) (Sdl.Rect.h hitboxObj) else Sdl.Rect.create (if Objet.is_facing_right objet then Sdl.Rect.x hitboxObj + Sdl.Rect.w hitboxObj + 20 else Sdl.Rect.x hitboxObj)  (Sdl.Rect.y hitboxObj) (2) (Sdl.Rect.h hitboxObj) in
	   let l = Scene.get_list scene in
	   let rec collision_mur liste =
	     match liste with
	     |[] -> false
	     |hd :: tl ->
		if Objet.is_mur hd then begin
		  let rectMur = Objet.get_hitbox hd in
		  if collisionRect rectObj rectMur then true
		  else collision_mur tl
		end
		else collision_mur tl;
	   in
	   collision_mur l
	 in
        

	 (*detecte les collisions avec un monstre*)
	 let collisionMonstre perso monstre =
	   let leftA = Objet.get_x perso in
	   let rightA = Objet.get_x perso + Sdl.Rect.w (Objet.get_hitbox perso) in
	   let topA = Objet.get_y perso in
	   let bottomA = Objet.get_y perso + Sdl.Rect.h (Objet.get_hitbox perso) in
	   let leftB = Objet.get_x monstre in
	   let rightB = Objet.get_x monstre + Sdl.Rect.w (Objet.get_hitbox monstre) in
	   let topB = Objet.get_y monstre in
	   let bottomB = Objet.get_y monstre + Sdl.Rect.h (Objet.get_hitbox monstre) in
	   if bottomA <= topB then false
	   else if topA >= bottomB then false
	   else if rightA <= leftB then false
	   else if leftA >= rightB then false
	   else true
	 in

	 (*demarre le jeu*)
	 while true do
	   
	   start_menu ();
	   let play = ref true in
	   let level = ref 1 in
	   let nombreLevel = 5 in
	   
	   (*charge le niveau*)
	   while !play do
	  
	     Sdl.delay 100l;
	     
	   let pause = ref false in
	   let numAnim = ref 0 in
	   let en_saut = ref false in
	   let temps_saut = ref 0 in
	   let niveauEnCours = ref true in
	   let sautHor = ref false in
	   let animEnCours = ref "" in

	   
	   let nom_level = "level"^(string_of_int !level)^".txt" in
	   let nom_background = "background"^(string_of_int !level)^".bmp" in
	   let scene = ref (Scene.make_scene nom_level) in
	   let perso = ref(Scene.get_perso !scene) in
	   let tir = ref (Objet.create_projectile (-1000) 0 20 (Objet.get_facing !perso)) in
	   clear_rend();

	   (*Charge toutes les textures*)
	   match Sdl.load_bmp nom_background  with
	   |Error (`Msg e) -> Sdl.log "Load bmp error %s" e; exit 1
	   |Ok imageBackground ->
	      match Sdl.create_texture_from_surface r imageBackground with
	      |Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
	      |Ok txBackground ->
		 match Sdl.load_bmp "braid.bmp"  with
		 |Error (`Msg e) -> Sdl.log "Load bmp error %s" e; exit 1
		 |Ok imagePerso ->
		    match Sdl.create_texture_from_surface r imagePerso with
		    |Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
		    |Ok txPerso ->
		        match Sdl.load_bmp "block.bmp"  with
			|Error (`Msg e) -> Sdl.log "Load bmp error %s" e; exit 1
			|Ok imageBlock ->
			   match Sdl.create_texture_from_surface r imageBlock with
			   |Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
			   |Ok txBlock ->
			      match Sdl.load_bmp "door.bmp"  with
			      |Error (`Msg e) -> Sdl.log "Load bmp error %s" e; exit 1
			      |Ok imagePorte ->
				 match Sdl.create_texture_from_surface r imagePorte with
				 |Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
				 |Ok txPorte ->
				    match Sdl.load_bmp "monster.bmp"  with
				    |Error (`Msg e) -> Sdl.log "Load bmp error %s" e; exit 1
				    |Ok imageMonster ->
				       match Sdl.create_texture_from_surface r imageMonster with
				       |Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
				       |Ok txMonster ->
					  match Sdl.load_bmp "bullet.bmp"  with
					  |Error (`Msg e) -> Sdl.log "Load bmp error %s" e; exit 1
					  |Ok imageTir ->
					     match Sdl.create_texture_from_surface r imageTir with
					     |Error (`Msg e) -> Sdl.log "Create texture error %s" e; exit 1
					     |Ok txTir ->

	   (*Demarre le niveau*)
	   while !niveauEnCours do
	     
	     
	     if !pause = false then begin
	    
	       animEnCours := "";
	       let mouvementDansTour = ref false in
	       let rectCam = create_camera !scene in

	     (*Place les entites sur l'ecran*)
	     create_background txBackground;
	     affiche_all_block txBlock !scene rectCam;
	     affiche_porte txPorte !scene;
	     affiche_all_monsters !scene txMonster;
	     make_bullet txTir (Objet.get_x !tir) (Objet.get_y !tir);

	     

	    
	     if Scene.perso_a_terre !scene = false && !en_saut = false then
	       Objet.change_vit_v !perso (Objet.get_vit_v !perso + 1);
	     if Scene.perso_a_terre !scene then
	       Objet.change_vit_v !perso 0;
	     
	     (*Fonctionnement des ennemis*)
             let listeObj = Scene.get_list !scene in
	     let tailleListe = ref (List.length listeObj) in
	     while !tailleListe > 0 do
	       tailleListe := !tailleListe-1;
	       let obj = ref (List.nth listeObj !tailleListe) in
	       
	       if Objet.is_enemy !obj && Scene.monstre_a_terre !scene !obj = false then
		 Objet.change_vit_v !obj (Objet.get_vit_v !obj + 1);

	        if Objet.is_enemy !obj && Scene.monstre_a_terre !scene !obj then
		 Objet.change_vit_v !obj 0;

	       if Objet.is_enemy !obj && Scene.monstre_a_terre !scene !obj  then begin

		 if Objet.is_facing_left !obj then begin
		    Objet.change_vit_h !obj (Objet.get_vit_h !obj - 1);
		   if collision !obj !scene then
		     Objet.do_facing_r !obj;
		 end
		 else begin
		   Objet.change_vit_h !obj (Objet.get_vit_h !obj + 1);
		   if collision !obj !scene then
		     Objet.do_facing_l !obj;
		 end;

	       end;

	       if Objet.is_enemy !obj && collisionMonstre !perso !obj then begin
		 niveauEnCours := false;
		 play := false;
		 end;

	       if collisionMonstre !tir !obj then begin
		 Objet.change_x_y !tir (-100) 0;
		 Objet.change_x_y !obj (-200) 0;
	       end;

	       
	       if collision !obj !scene = false then
		 Objet.change_x_y !obj (Objet.get_x !obj + Objet.get_vit_h !obj) (Objet.get_y !obj + Objet.get_vit_v !obj)
	       else
		 Objet.change_vit_h !obj 0;
	     done;

	     (*gestion des sauts*)
	     if !temps_saut > 0 && !en_saut = true then begin
	       animEnCours := "saute";
	       Objet.change_vit_v !perso (Objet.get_vit_v !perso - 1);
	       temps_saut := !temps_saut - 1;
	       if !sautHor then begin
		 mouvementDansTour := true;
		 Objet.change_vit_h !perso (Objet.get_vit_h !perso + (if Objet.is_facing_right !perso then 1 else -1));
	       end;
	     end;

	     if !temps_saut = 0 then begin
	       en_saut := false;
	       sautHor := false;
	     end;

	     if !temps_saut = 0 && Scene.perso_a_terre !scene = false then
	       animEnCours := "tombe";
	     if !temps_saut > 0 then
	       animEnCours := "saute";

	     (*gestion des tirs*)

	     if Objet.get_x !tir >= 0 then begin

	       if Objet.is_facing_right !tir then
		 Objet.change_vit_h !tir (Objet.get_vit_h !tir + 1)
	       else
		 Objet.change_vit_h !tir (Objet.get_vit_h !tir -1);

	       if collision !tir !scene then
		  Objet.change_x_y !tir (-100) 0;

	       let tir2 = Objet.create_projectile (Objet.get_x !tir) (Objet.get_y !tir) 20 (Objet.get_facing !tir) in
	       Objet.change_x_y tir2 (Objet.get_x !tir + Objet.get_vit_h !tir) (Objet.get_y !tir);
	       if collision tir2 !scene then
		 Objet.change_x_y !tir (-100) 0
	       else
		 Objet.change_x_y !tir (Objet.get_x !tir + Objet.get_vit_h !tir) (Objet.get_y !tir);
	     end
	     else
	       Objet.change_vit_h !tir 0;

	     
	     (*gestion des touches*)
	     let evt = getEvent () in
	     if evt = "q" then quit ();
	     if evt = "esc" then
	       pause:= true;
	     if evt = "r" && Scene.perso_a_terre !scene  then begin
	       Objet.change_vit_h !perso (Objet.get_vit_h !perso + 1);
	       mouvementDansTour := true;
	       Objet.do_facing_r !perso;
	       animEnCours := "marche";
	       Sdl.delay 10l;
	     end;
	     if evt = "l" && Scene.perso_a_terre !scene then begin
	       Objet.change_vit_h !perso (Objet.get_vit_h !perso - 1);
	       mouvementDansTour := true;
	       Objet.do_facing_l !perso;
	       animEnCours := "marche";
	       Sdl.delay 10l;
	      end;
	      if evt = "jp" && Scene.perso_a_terre !scene then begin
		en_saut := true;
		if !temps_saut = 0 then temps_saut := 30;
		animEnCours := "saute";
	      end;
	      if evt = "jpr" && Scene.perso_a_terre !scene then begin
		en_saut := true;
		sautHor := true;
		if !temps_saut = 0 then temps_saut := 30;
		Objet.change_vit_h !perso (Objet.get_vit_h !perso + 1);
		Objet.do_facing_r !perso;
		animEnCours := "saute"
	      end;
	      if evt = "jpl" && Scene.perso_a_terre !scene then begin
		en_saut := true;
		sautHor := true;
		if !temps_saut = 0 then temps_saut := 30;
		Objet.change_vit_h !perso (Objet.get_vit_h !perso + 1);
		Objet.do_facing_l !perso;
		animEnCours := "saute";
	      end;
	      if evt = "sp" then begin
		tir := Objet.create_projectile (Objet.get_x !perso) (Objet.get_y !perso + 40) 20 (Objet.get_facing !perso);
		animEnCours := "tire";
		Sdl.delay 100l;
	      end;

	      (*passage au niveau suivant*)
	      if clear !scene then begin
		level := !level+1;
		niveauEnCours := false;
	      end;


	      if !mouvementDansTour = false then begin
		if Objet.get_vit_h !perso < 0 then
		  Objet.change_vit_h !perso (Objet.get_vit_h !perso + 1)
		else if Objet.get_vit_h !perso > 0 then
		  Objet.change_vit_h !perso (Objet.get_vit_h !perso - 1)
	      end;


	      
	      
	      if collision !perso !scene = false then
		Objet.change_x_y !perso (Objet.get_x !perso + Objet.get_vit_h !perso) (Objet.get_y !perso + Objet.get_vit_v !perso)
	      else begin
		Objet.change_vit_h !perso 0;
		Objet.change_x_y !perso (if Objet.is_facing_left !perso then Objet.get_x !perso + 2 else Objet.get_x !perso - 2) (Objet.get_y !perso);
	      end;

	      numAnim := if !numAnim < 100 then !numAnim+1 else 0;

	      (*animation du personnage*)
	      let anim () =
	       if !animEnCours = "" then animRepos txPerso !scene
	       else if !animEnCours = "marche" then animMarche txPerso !scene !numAnim
	       else if !animEnCours = "saute" then animSaut txPerso !scene !numAnim
	       else if !animEnCours = "tombe" then animTombe txPerso !scene !numAnim
	       else if !animEnCours = "tire" then animTir txPerso !scene !numAnim;
	      in
	      anim ();
	      
	      Sdl.render_present r;
	      Sdl.delay 10l;

	     end
	     else begin
	       Sdl.delay 1000l;
	       let evt = getEvent () in
	       if evt = "q" then quit ();
	       if evt = "esc" then
		 pause:= false;
	       Sdl.delay 1000l;
	     end;
	     
	   done;

					 if !level > nombreLevel then play := false;	 
		   
	   done;
	    Sdl.render_present r;
	    
	 done;


	 
;;

(*compiler : ocamlbuild -use-ocamlfind -package tsdl,tsdl_mixer Braidoidvania.byte *)
