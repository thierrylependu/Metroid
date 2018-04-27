open Tsdl;;
open Result;;
open Scene;;
open Readfile;;

let fermer = ref false;;

let start_game = ref false;;

let jeu_en_pause = ref false;;

let attente = ref true;;

let prefixe_fichier = "lvl_" ;;
let ext_fichier = "" ;;

match Sdl.init Sdl.Init.video with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
| Ok () ->

   (* declaration touches menu *)
   let quit_from_menu_touch = Sdl.get_scancode_from_key Sdl.K.q in
   let start_touch = Sdl.get_scancode_from_key Sdl.K.return in

   (* declaration touches jeu *)
   let jump_touch = Sdl.get_scancode_from_key Sdl.K.up in
   let right_touch = Sdl.get_scancode_from_key Sdl.K.right in
   let left_touch = Sdl.get_scancode_from_key Sdl.K.left in
   let proj_touch = Sdl.get_scancode_from_key Sdl.K.space in
   let close_touch = Sdl.get_scancode_from_key Sdl.K.q in
   let pause_touch = Sdl.get_scancode_from_key Sdl.K.escape in

   (* declaration touches pause *)
   let continue_touch = Sdl.get_scancode_from_key Sdl.K.escape in
   let quit_touch_from_pause = Sdl.get_scancode_from_key Sdl.K.q in

   (* fonction pour quitter facilement avec la croix *)
   let quit ev =
     match Sdl.Event.(enum @@ get ev typ) with
     | `Quit -> fermer := true
     | _ -> ()
   in

   let fenetre liste_scene =
     fermer := false;
     match Sdl.init Sdl.Init.video with
     | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
     | Ok () ->
        match Sdl.create_window ~w:640 ~h:480 "Metroidvania" Sdl.Window.opengl with
        | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
        | Ok w ->
           match Sdl.create_renderer ~index:(-1) ~flags:(Sdl.Renderer.(accelerated + presentvsync)) w with
           | Error (`Msg e) -> Sdl.log "create_render error: %s" e; exit 1
           | Ok r ->
	      let () = Sdl.render_present r in
	      Sdl.delay 1000l; (* attendre 1000ms *)
              start_game := false;

              (* menu de demarrage *)
              while (not(!fermer) && not(!start_game)) do
                print_string "menu\n\n";
                let evts = Sdl.Event.create() in
	        let etat_clavier = Sdl.get_keyboard_state() in
	        while Sdl.poll_event (Some(evts)) do
                  quit evts
	        done;
                if etat_clavier.{quit_from_menu_touch}=1 then
                  fermer := true;
                if etat_clavier.{start_touch}=1 then
                  start_game := true;
              done;
              (* on sort du menu lorsqu'on a appuyé sur entree *)

              (* on cree une referance vers la scene *)
              let s = ref (List.hd liste_scene) in
	      let lvl_pas_fini = ref true in
              jeu_en_pause := false;
              attente := true;

              let pause() =
                Sdl.delay 1000l;
                print_string "pause\n\n";
                let evts = Sdl.Event.create() in
	        let etat_clavier = Sdl.get_keyboard_state() in
	        while Sdl.poll_event (Some(evts)) do
                  quit evts
	        done;
                if etat_clavier.{continue_touch}=1 then
                  begin
                    jeu_en_pause := false;
                    attente := true;
                  end;
                if etat_clavier.{quit_touch_from_pause}=1 then
                  fermer := true
              in

              let jeu() =
                print_string "jeu\n";
                let evts = Sdl.Event.create() in
	        let etat_clavier = Sdl.get_keyboard_state() in
	        while Sdl.poll_event (Some(evts)) do
                  quit evts
	        done;
	        if etat_clavier.{close_touch}=1 then
                  fermer := true
		else
                  if etat_clavier.{pause_touch}=1 then
                    begin
                      jeu_en_pause := true;
                      attente := true;
                    end
		  else
		    let va_tirer = (etat_clavier.{proj_touch}=1) in
                    let new_action =
                      if (etat_clavier.{right_touch}=1 && etat_clavier.{jump_touch}=1) then
                        (Saut_vers_droite,va_tirer)
                      else if (etat_clavier.{left_touch}=1 && etat_clavier.{jump_touch}=1) then
                        (Saut_vers_gauche,va_tirer)
                      else if etat_clavier.{right_touch}=1 then
                        (Marcher_droite,va_tirer)
                      else if etat_clavier.{left_touch}=1 then
                        (Marcher_gauche,va_tirer)
                      else if etat_clavier.{jump_touch}=1 then
                        (Saut_normal,va_tirer)
                      else (Aucune,va_tirer)
                    in
		    Scene.update_scene !s new_action;
                    Scene.affichage !s;
                    if (Scene.clear !s) then
                      lvl_pas_fini := false;
              in

	      let new_s_list = ref liste_scene in
	      while not(!fermer) && not((!new_s_list)=[]) do
		new_s_list := List.tl !new_s_list;

                while (not(!fermer) && not(!start_game)) do
                  print_string "press enter\n\n";
                  let evts = Sdl.Event.create() in
	          let etat_clavier = Sdl.get_keyboard_state() in
	          while Sdl.poll_event (Some(evts)) do
                    quit evts
	          done;
                  if etat_clavier.{quit_from_menu_touch}=1 then
                    fermer := true;
                  if etat_clavier.{start_touch}=1 then
                    start_game := true;
                done;

                start_game := false;
		lvl_pas_fini := true;
		while not(!fermer) && !lvl_pas_fini do
                  (* on attend pour effectuer un transition correcte entre le jeu et le menu pause *)
                  if (!attente) then
                    begin
                      Sdl.delay 1000l;
                      attente := false;
                    end;
                  if (!jeu_en_pause) then (* cas ou le jeu est en pause *)
                    pause()
                  else (* cas ou le jeu n'est pas en pause *)
                    jeu()
		done;
                if not((!new_s_list)=[]) then
		  s := (List.hd !new_s_list)
                else ()
	      done;
	      Sdl.destroy_window w;(* ferme la fenetre *)
	      Sdl.quit ();
   in

   let get_lvl_list() =
     let rec aux num acc =
       let filename =
         if ext_fichier = "" then
           prefixe_fichier^(string_of_int num)
         else
           prefixe_fichier^(string_of_int num)^"."^ext_fichier
       in
       try
	 let new_scene = Scene.make_scene filename in
	 let new_acc = (new_scene::acc) in
	 aux (num+1) new_acc
       with
       | Pas_de_porte -> aux (num+1) acc
       | Pas_de_perso -> aux (num+1) acc
       | Trop_de_perso -> aux (num+1) acc
       | Sys_error _ -> acc
     in
     List.rev (aux 1 [])
   in

   let main () =
     let scene_list = get_lvl_list() in
     if (List.length scene_list) = 0 then
       print_string "Erreur : Pas de fichier vailde\n"
     else
       fenetre scene_list
     ;
     exit 0
   in

   main ()

(*
  Pour compiler : ocamlbuild -use-ocamlfind -package tsdl,tsdl_mixer metroidvania.byte
  Pour executer : ./metroidvania.byte fichier_niveau
*)
