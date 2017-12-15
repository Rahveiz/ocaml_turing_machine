(* TYPES *)

type direction = D | G ;;

type symbole = char;;

type position = int;;

type etat = int;;

type ruban = position -> symbole;;

type etat_global ={e:etat; r:ruban; p:position};;

type regle = {e1:etat;

              s1:symbole;

              e2:etat;

              s2:symbole;

              d:direction};;

type programme=regle list;;

type tm={ei:etat_global; ef:etat; pr:programme};;