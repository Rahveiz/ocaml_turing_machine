(* TYPES*)

#use "turing_types.ml";;

(* EXEC *)

(*Char list to string*)
let cl2s cl = String.concat "" (List.map (String.make 1) cl);;

(*Affiche etat global*)
let rec ruban_list pos r depart =
    if (depart == pos + 10) then (r depart)::[]
    else (r depart)::(ruban_list pos r (depart + 1));;

let affiche_etat_global e = print_string(cl2s (ruban_list e.p e.r (e.p - 10)));;

(* On teste avec le ruban ..#ab#..*)
let r1 (p:position) = match p with
    1->'1'
    |2->'1'
    |3->'1'
    |4->'1'
    |5->'1'
    |6->'0'
    |7->'0'
    |8->'1'
    |9->'1'
    |10->'0'
    |11->'1'
    |_->'#';;
let etat1 = {e=1; r=r1; p=1};;
let etat2 = {e=2; r=r1; p=2};;

(*Trouve la regle qui correspond a l'etat global*)
let rec find_regle e rg n =
    if ((e.e == (List.nth rg n).e1) && ((e.r e.p) == (List.nth rg n).s1)) then List.nth rg n else find_regle e rg (n+1);;

(*Renvoie la regle matchee*)
let match_regle e rg = let n = 0 in find_regle e rg n;;

(*Modification du ruban*)

let update_ruban pos r rg =
    fun (p:position) -> if (p == pos) then rg.s2 else r p;;

let suivant t e =
    if e.e == t.ef then (e, false)
    else
        ({e=(match_regle e t.pr).e2;
        r=update_ruban e.p e.r (match_regle e t.pr);
        p=if (match_regle e t.pr).d == G then (e.p) - 1 else (e.p) + 1},true);;


let etat_from_suivant (e, _) = e;;
let continuer_from_suivant (_, c) = c;;

let corps l = match l with
    [] -> failwith "pas de corps de liste"
    |t::r->r;;

let rec run t =
    let s = (suivant t t.ei) in
    affiche_etat_global t.ei;
    print_string "\n";
    if (continuer_from_suivant s) == true
    then
        run {ei=(etat_from_suivant s)
            ; ef=t.ef;
            pr=t.pr}
;;

(*On change les 1 en 0 et on va a droite, on s'arrete au premier 0 rencontre*)
let turing1 = {ei=etat1;ef=2;pr=[{e1=1;s1='1';e2=1;s2='0';d=D};{e1=1;s1='0';e2=2;s2='0';d=D}]};;
(*On change les 1 en 0 et on va a droite, on s'arrete au premier 0 rencontre qu'on remplace par 3*)
let turing2 = {ei=etat1;ef=2;pr=[{e1=1;s1='1';e2=1;s2='0';d=D};{e1=1;s1='0';e2=2;s2='3';d=D}]};;
(*On change les 1 en 0 et on va a droite, on s'arrete au premier 0 rencontre qu'on remplace par 3, on va a gauche et on remplace le 0 par A*)
let turing3 = {ei=etat1;ef=3;pr=[{e1=1;s1='1';e2=1;s2='0';d=D};{e1=1;s1='0';e2=2;s2='3';d=G};{e1=2;s1='0';e2=3;s2='A';d=G}]};;

print_string "Un exemple quelconque\n";;
print_string "Depart : ";;
affiche_etat_global etat1;;
print_string "\nOn change les 1 en 0 et on va a droite, on s'arrete au premier 0 rencontre : ";;
run turing1;;
print_string "\nOn change les 1 en 0 et on va a droite, on s'arrete au premier 0 rencontre qu'on remplace par 3 : ";;
run turing2;;
print_string "\nOn change les 1 en 0 et on va a droite, on s'arrete au premier 0 rencontre qu'on remplace par 3, on va a gauche et on remplace le 0 par A : ";;
run turing3;;


(* double la valeur d'un nombre binaire (entier positif)*)
let prog_M1 = [
{e1=0;s1='0';e2=1;s2='0';d=D};
{e1=0;s1='1';e2=1;s2='1';d=D};
{e1=0;s1='#';e2=3;s2='#';d=D};
{e1=1;s1='0';e2=1;s2='0';d=D};
{e1=1;s1='1';e2=1;s2='1';d=D};
{e1=1;s1='#';e2=2;s2='0';d=G};
{e1=2;s1='0';e2=2;s2='0';d=G};
{e1=2;s1='1';e2=2;s2='1';d=G};
{e1=2;s1='#';e2=3;s2='#';d=D};
];;

let rub5 (p:position) = match p with
    1->'1'
    |2->'0'
    |3->'1'
    |_->'#'
    ;;

let rub42 (p:position) = match p with
    1->'1'
    |2->'0'
    |3->'1'
    |4->'0'
    |5->'1'
    |6->'0'
    |_->'#'
    ;;

let bin_5 = {e=0;r=rub5;p=1};;
let tm1 = {ei=bin_5;ef=3;pr=prog_M1};;
let bin_42 = {e=0;r=rub42;p=1};;
let tm2 = {ei=bin_42;ef=3;pr=prog_M1};;

print_string "Exemple du cours : doubler la valeur d'un nombre binaire\n";;
print_string "Entree de depart (5) : ";;
affiche_etat_global bin_5;;
print_string "\n";;
print_string "Resultat : \n";;
run tm1;;
print_string "\nOn a bien 10, c'est correct";;
print_string "\n";;
print_string "\nOn essaye un autre nombre (42) : ";;
affiche_etat_global bin_42;;
print_string "\nResultat : \n";;
run tm2;;
print_string "\nGoet !";;
