(******creation des personnes **********************)

let rec gen_liste t = function
  
  (*
		genere une liste de personnes aleatoires sous la forme:
		(depart, arrive)::reste de la liste
 	*)
    | 0 -> []
    | k ->
        let (a, b, c, d) = (random__int t, random__int t, random__int t, random__int t) in
          ((a, b), (c, d)) :: gen_liste t (k - 1);;

let rec min_liste = function
    | [] -> failwith "liste_vide"
    | [a] -> a
    | a :: q -> min a (min_liste q);;


let personnes_to_vect l =
  (*
 		transforme une liste de personnes en un tableau, le premier element, p.(0) est l'origine, puis les 
 		elements en position impairs correspondent aux departs, et ceux en position paire a l'arrivee d'une
 		personne 
	 *)
  
  let n = list_length l in
    let p = make_vect (2 * n + 1) (0, 0) in
      let rec remplir_tableau k = function
          | [] -> ()
          | ((d, f) :: q) -> (p.(k) <- d; p.(k + 1) <- f; remplir_tableau (k + 2) q)
      in remplir_tableau 1 l;
        p;;



(*

pour le pb du voyageur de commerce : 
let rec gen_lieu t = function
  
  (*
		genere une liste de couples (a,a) , a est un lieu
 	*)
    | 0 -> []
    | k ->
        let (a, b) = (random__int t, random__int t) in
          ((a, b), (a,b)) :: gen_lieu t (k - 1);;
          


*)



 (***************CREATION DE LA PERMUTATION INITIALE*********) 



type permutation = {traj : int vect; pos :  int vect };;	
type circuit  = {pers : (int*int) vect; perm : permutation; mutable l : int };;




 let rec retirer_ni_element = fun
            | 1 (a :: q) -> a, q
            | _ [] -> failwith "salut"
            | n (a :: q) -> let (b, l) = retirer_ni_element (n - 1) q
               in (b, a :: l);;


let permutation_aleatoire n =
	(*crée une permutation aléatoire de 1 a n *)
   let j = ref 0 and m = make_vect (n + 1) 0 in

      let rec liste_init = function
         | 0 -> []
         | n -> n :: liste_init (n - 1)

      in
          let l = liste_init n in
            let temp = ref (0, l) in
               for k = 1 to n do
              
                  j := random__int (n - k+1 ) + 1;
                  temp := retirer_ni_element !j (snd (!temp));
                  m.(k) <- fst (!temp);
               done;
               m;;


let echange m (i,j) = 
	let c = m.(i) in
		m.(i) <- m.(j);
		m.(j) <- c;;
		
let echange_bij perm (i, j) =
   echange (perm.pos) (perm.traj.(i), perm.traj.(j));
   echange perm.traj (i, j);;

let rearranger m = 
	(*réarrange le tableau des positions pour le rendre valide *)
	let n = vect_length m in
	for k = 1 to (n-1)/2 do
		if m.(2*k-1) > m.(2*k) then echange m (2*k,2*k-1);
	done; m;;


let reciproque m = 	
	(*crée le tableau traj a partir du tableau des positions *)
	let n = vect_length m in let t = make_vect n 0 in
		for k = 1 to (n-1) do
			t.(m.(k)) <- k;
		done;
		t;;


	
	
	
let perm_aleatoire n = 
	(*crée deux tableaux aléatoires , traj et pos , réciproques *)
	let pos = rearranger(permutation_aleatoire n) in
		let traj = reciproque pos in
			{traj = traj; pos = pos};;
			
			
(***recherche des TRANSPOSITIONS valides *)

exception non_valide;;

let est_valide pos =
   (*
		renvoie si une permutation est valide, prend comme argument le tableau des positions 
	*)
   let m = vect_length pos - 1 in
      try
         for k = 1 to m / 2 do
            if pos.(2 * k - 1) > pos.(2 * k) then raise non_valide
         done;
         true;
      with non_valide -> false;;






let distance (a, b) (c, d) = (*la distance sur N *)
   abs (c - a) + abs (d - b);;


let longueur_trajet traj per =
   let n = vect_length traj and d = ref 0 in
      for k = 0 to n - 2 do
         d := !d + distance per.(traj.(k)) per.(traj.(k + 1));
      done;
      
      !d;; 

let rec meilleures_transp cir = 
 (*choisit la meilleure transposition en terme de longueur *)
	let perm = cir.perm and n = vect_length cir.perm.traj in
		fun
   | i j when i >= (n - 1) && j >= (n) -> []
   | i j when j >= n -> meilleures_transp cir  (i + 1) (i+2)
   | i j ->
      begin
         echange_bij perm (i, j);
         if est_valide perm.pos && longueur_trajet perm.traj cir.pers < cir.l then
            begin
               echange_bij perm (i, j);
               (i, j) :: meilleures_transp cir i (j + 1)
            end
         else
            begin
               echange_bij perm (i, j);
               meilleures_transp cir i (j + 1) ;
            end;
      end;;

let rec premiere_transp cir = 
	(*choisit la premiere transposition qui améliore le circuit *)
  let perm = cir.perm and n = vect_length cir.perm.traj in
    fun
      | i j when i >= (n - 1) && j >= (n) -> []
      | i j when j >= n -> premiere_transp cir (i + 1) (i + 2)
      | i j ->
          begin
            echange_bij perm (i, j);
            if est_valide perm.pos && longueur_trajet perm.traj cir.pers < cir.l then
              begin
                echange_bij perm (i, j);
                [(i, j)]
              end
            else
              begin
                echange_bij perm (i, j);
                premiere_transp cir i (j + 1);
              end;
          end;;




let copy_perm perm = 
	let traj = copy_vect perm.traj and pos = copy_vect perm.pos in
		{traj = traj;pos = pos};;
	
let copy_circuit cir = 
	let pers = cir.pers and perm = copy_perm cir.perm and l = cir.l in
		{pers = pers;perm = perm;l = l };;



(************************solution trop lente *****


let rec liste_meilleures cir n = fun
   | i j when i = (n - 1) && j >= (n) -> []
   | i j when j = n -> liste_meilleures cir n (i + 1) (i+2)
   | i j ->
      begin
         echange_bij cir.perm (i, j);
         let l2 = longueur_trajet cir.perm.traj cir.pers in
         if est_valide cir.perm.pos && l2 < cir.l then
            begin
            	let cir2 = copy_circuit cir in
            	cir2.l <- l2;
               echange_bij cir.perm (i, j);
               cir2 :: liste_meilleures cir n i (j + 1)
            end
         else
            begin
               echange_bij cir.perm (i, j);
               liste_meilleures cir n i (j + 1) ;
            end;
      end;;





let ameliorer_circuit cir = 
	let n = vect_length cir.perm.pos in
		let rec aux l = function
			|[] -> l
			|a::q -> 
				let l2 = liste_meilleures a n 1 2 in
					begin
						match l2 with
							|[] -> aux (a::l) q
							|l2 -> aux l (l2 @ q)
					end
		in aux [] [cir];;


************************************************************************)




let circuit_aleatoire taille n = 
	(*génère une liste de personnes puis un circuit aléatoire *)
	let pers = personnes_to_vect (gen_liste taille n) in
		let perm = perm_aleatoire (2*n) in
			{pers = pers ; perm = perm;l = longueur_trajet perm.traj pers };;
			
			
			





let transposition_alea m =
   let i, j = ref 0, ref 0 in
      while !i = !j do
         i := random__int m + 1;
         j := random__int m + 1
      done;
      (!i, !j);;


exception max_depasse;;

let etape_alea_charge_inf cir max =
  
  (*modifie traj et pos pour une transposition aleatoire autorisee, et renvoie cette transposition.
	Le nouveau trajet n'est pas forcement meilleur *)
  
  let perm = cir.perm in
    let n = vect_length perm.traj - 1 and k = ref 0 in
      let trans = ref (transposition_alea n) in
        echange_bij perm !trans;
        let l2 = ref (longueur_trajet perm.traj cir.pers) in
          while (not est_valide perm.pos) or !l2 >= cir.l do
            incr k;
            echange_bij perm !trans;
            if !k > max then raise max_depasse;
            trans := transposition_alea n;
            echange_bij perm !trans;
            l2 := longueur_trajet perm.traj cir.pers
          done; !l2;;


let trajet_approche_charge_inf cir max =
  (*
		ameliore le trajet par effet de bord, tant que la fonction meilleur_trajet ne renvoie pas
		d'erreur pour "temps depasse"
	*)
  let d = ref 0 and it = ref 0 and en_cours = ref true in
    
    while !en_cours do
      incr it;
      try
        d := etape_alea_charge_inf cir max;
        cir.l <- !d;
      with max_depasse ->
            match premiere_transp cir 1 2 with
              | [] -> en_cours := false;
              | a :: q -> echange_bij cir.perm a;
    done; !it;;

let trajet_approche_charge_inf_rapide cir max =
  (*
		ameliore le trajet par effet de bord, tant que la fonction meilleur_trajet ne renvoie pas
		d'erreur pour "temps depasse"
	*)
  let d = ref 0 and it = ref 0 and en_cours = ref true in
    
    while !en_cours do
      incr it;
      try
        d := etape_alea_charge_inf cir max;
        cir.l <- !d;
      with max_depasse -> en_cours := false;
      done; !it;;


let rec ameliorer_circuit cir max_transp = 
	(*
		crée n copies du circuit et applique trajet_approche pour trouver une 
		solution optimale 
	*)
	function
	|0 -> []
	|n -> let cir2 = copy_circuit cir in 
		(let d = trajet_approche_charge_inf cir2 max_transp in
		cir2::ameliorer_circuit cir max_transp (n-1));;



let rec min_liste_norme f = function
	|[] -> failwith "pas ok"
	|[a] -> a
	|a::q -> let b = min_liste_norme f q in
		if f a < f b then a else b;;


(*

let l = ameliorer_circuit cir 100 500;;

let cir2 = min_liste_norme (function cir -> cir.l) l;;

clear_graph();;
dessiner_circuit cir 50;;
clear_graph ();;
dessiner_circuit cir2 50;;
charge cir;;
charge cir2;;
*)

(****exemples
let cir = circuit_aleatoire 30 100;;
cir.l;;
charge cir;;
trajet_approche_charge_inf cir 100;;

cir.l;;
charge cir;;

*)



(*************STATISTIQUES ******************)

(***SANS CHARGE ***)

let statistique n taille_v iter =
   let somme_avant = ref 0 and somme_après = ref 0 in
      let cir = ref (circuit_aleatoire taille_v n) in
         for i = 1 to iter do
            somme_avant := !somme_avant + !cir.l;
            let l = trajet_approche_charge_inf!cir n in
               somme_après := !somme_après + !cir.l;
               cir := circuit_aleatoire taille_v n;
         done; (!somme_avant / iter, !somme_après / iter);;



let charge_moyenne n taille iter = 
	let somme = ref 0 and cir = ref (circuit_aleatoire taille n) in
		for i = 1 to iter do
			let l = trajet_approche_charge_inf !cir n in
				somme := !somme + charge !cir;
				cir := 	circuit_aleatoire taille n;	
			done;
			!somme/iter;;



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




(*************GESTION DE LA CHARGE***********)

let charge cir =  (*calcule la charge d'un trajet *)
	let traj = cir.perm.traj in
	let n = vect_length traj and charge = ref 0 and charge_max = ref 0 in
		for k = 1 to (n-1) do
			if traj.(k) mod 2 = 0 then charge := !charge - 1
			else charge := !charge + 1;
			if !charge > !charge_max then charge_max := !charge;
	done;
	!charge_max;;
	
	
let circuit_aleatoire_charge1 taille n = 
	(*génère une liste de personnes puis un circuit aléatoire *)
	let pers = personnes_to_vect (gen_liste taille n) in
		let perm = permutation_aleatoire (n) in
			let pos = make_vect (2*n +1) 0 in
			for k = 1 to n do
				pos.(2*perm.(k)-1) <- 2*k-1;
				pos.(2*perm.(k)) <- 2*k;
				done; let traj = reciproque pos in
			{pers = pers ; perm = {traj = traj;pos = pos};l = longueur_trajet traj pers };;
			
			
			


let est_valide_charge cir ch_max = 
	charge cir <= ch_max && est_valide cir.perm.pos ;;
	
	
	
	let etape_alea_charge_finie cir max ch_max=
  
  (*modifie traj et pos pour une transposition aleatoire autorisee, et renvoie cette transposition.
	Le nouveau trajet n'est pas forcement meilleur *)
  
  let perm = cir.perm in
    let n = vect_length perm.traj - 1 and k = ref 0 in
      let trans = ref (transposition_alea n) in
        echange_bij perm !trans;
        let l2 = ref (longueur_trajet perm.traj cir.pers) in
          while (not est_valide_charge cir ch_max) or !l2 >= cir.l do
            incr k;
            echange_bij perm !trans;
            if !k > max then raise max_depasse;
            trans := transposition_alea n;
            echange_bij perm !trans;
            l2 := longueur_trajet perm.traj cir.pers
          done; !l2;;


let trajet_approche_charge_finie cir max ch_max = 
  (*
		ameliore le trajet par effet de bord, tant que la fonction meilleur_trajet ne renvoie pas
		d'erreur pour "temps depasse"
	*)
  let d = ref 0 and it = ref 0 and en_cours = ref true in
    try
    while !en_cours do
      incr it;
 
        d := etape_alea_charge_finie cir max ch_max;
        cir.l <- !d; 
        done; !it
      with max_depasse -> !it;;

(*
let cir = circuit_aleatoire 20 10;;
clear_graph();;
dessiner_circuit cir 20;;
let cir3 = copy_circuit cir;;



trajet_approche_charge_finie cir 500 20;;
trajet_approche_charge_inf cir3 500;;

etape_alea_charge_finie cir 1000 1;; *)