
(***************GRAPHIQUES ******************)





#open "graphics";;

open_graph ("710x710");;



let dessiner_liaison ((a, b), (c, d)) taille num =
	let (a',b') = (a*700/taille + 10,b*700/taille + 10) 
	and (c',d') = (c*700/taille + 10,d*700/taille + 10) in
   set_color black;
   fill_circle a' b' 15;
   moveto a' b';
  
   set_color black;
   lineto c' d';
   fill_circle c' d' 15;
   moveto (a' - 5) (b'-5);
    set_color white;
   draw_string (string_of_int num);;
   
   
let dessiner_liaison2 ((a, b), (c, d)) taille num =
	let (a',b') = (a*700/taille + 10,b*700/taille + 10) 
	and (c',d') = (c*700/taille + 10,d*700/taille + 10) in
   set_color black;
   if num mod 2 = 1 then set_color blue;
   fill_circle a' b' 15;
   moveto a' b';
   set_color black;
   lineto c' d';
   moveto (a' - 5) (b'-5);
    set_color white;
   draw_string (string_of_int num);;
   





   
let dessiner_circuit cir taille =
	let traj  = cir.perm.traj in
   let n = vect_length traj in
      for k = (n-1) downto 1 do
         dessiner_liaison2 (cir.pers.(traj.(k)), cir.pers.(traj.(k - 1))) taille traj.(k);
      done;
      dessiner_liaison2 (cir.pers.(traj.(0)),cir.pers.(traj.(0))) taille traj.(0);;


clear_graph ();;
let cir = circuit_aleatoire 100 50;;
dessiner_circuit cir 50;; cir.l;;
trajet_approche_charge_inf cir 500;;
clear_graph ();;
dessiner_circuit cir 50;;
cir.l;;



let dessiner_personnes pers =
   let n = vect_length pers in

      fill_circle 10 10 5;
      for k = 1 to (n - 1) / 2 do
         set_color green;
         let (a, b) = pers.(2 * k - 1) and (c, d) = pers.(2 * k) in
            set_color green;
            fill_circle (a * 20 + 10) (b * 20 + 10) 5;
            set_color black;
            moveto (a * 20 + 10) (b * 20 + 10);
            lineto (c * 20 + 10) (d * 20 + 10);
            fill_circle (c * 20 + 10) (d * 20 + 10) 5;
      done;;

