
include("functions.ml");;
include("graphics.ml");;

let l = ameliorer_circuit cir 100 500;;

let cir2 = min_liste_norme (function cir -> cir.l) l;;

clear_graph();;
dessiner_circuit cir 50;;
clear_graph ();;
dessiner_circuit cir2 50;;
charge cir;;
charge cir2;;



let cir = circuit_aleatoire 30 100;;
cir.l;;
charge cir;;
trajet_approche_charge_inf cir 100;;

cir.l;;
charge cir;;








(*Exemples charge finie*)

let cir = circuit_aleatoire 20 10;;
clear_graph();;
dessiner_circuit cir 20;;
let cir3 = copy_circuit cir;;



trajet_approche_charge_finie cir 500 20;;
trajet_approche_charge_inf cir3 500;;

etape_alea_charge_finie cir 1000 1;; 