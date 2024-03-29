(* [split_string s] retourne la liste de caractères correspondant �  la chaîne [s] *)
let split_string s =
  let rec aux i n =
    if i >= n
    then []
    else (String.get s i)::aux (i+1) n
  in
  aux 0 (String.length s)

(* [parse_input ()] lit deux lignes sur l'entr�e standard du programme. 
   La premi�re doit comporter un entier, la seconde une cha�ne de caract�res. 
   Retourne un couple composé de l'entier de la premi�re ligne et de la liste des caract�res de la seconde.
   
   L�ve l'exception [Failure "int_of_string"] si la premi�re ligne ne repr�sente pas un entier.
 *)

  
let parse_input () =
  let nb_antennas = int_of_string (read_line ()) in
  let phrase = read_line () in
  nb_antennas,split_string phrase;;

let distance_espace b =
  if b = ' ' then int_of_char b - 32 else int_of_char b - 64;;

let distance a b =
  int_of_char a - int_of_char b;;

let rec liste_sens_trigo dis =
  if dis = 0 then 'E'::[] else 'P'::(liste_sens_trigo (dis-1));;

let rec liste_sens_antitrigo dis =
  if dis = 0 then 'E'::[] else 'N'::(liste_sens_antitrigo (dis-1));;


let creation_liste ancien b =
  let courant = distance_espace b in
  let dis = ancien - courant in
  if dis >= 0 && dis <= 13 then liste_sens_trigo dis else
    if dis > 0 && dis > 13 then liste_sens_antitrigo (courant + 27 - ancien) else
      if dis <= 0 && dis <= -13 then liste_sens_trigo(dis+27) else
	liste_sens_antitrigo (abs(dis));;

let _ = distance_espace ' ' ;;

let _ = creation_liste 0 ' ';;


let rec lecture_input input ancien =
  match input with
    |[]->[]
    |e::l' ->
  
      (creation_liste ancien e)::(lecture_input l'(distance_espace e));;


let test = ['A';'B'];;


let _ = lecture_input test 0;;


(*Partie 2*)

type antenne={num:char list; pos: char ; dis: int};;

let distanceAll a b =
  if a = ' ' then
    if b = ' ' then 0 else distance_espace b
  else
    if b = ' ' then distance_espace a else distance a b;;

let distance3 a = if a = ' ' then 0 else distance_espace a;;
    

let insert_dis antenne distance = {num=antenne.num;pos=antenne.pos;dis=distance};;

let insert_pos antenne position = {num=antenne.num;pos=position;dis=antenne.dis};;

let rec min_antenne a1 a2 = if a1.dis <= a2.dis then a1 else a2;;

let rec min_antenne_liste l =
  match l with
    |[a]-> a
    |a::l' -> min_antenne a (min_antenne_liste l')
    |[]-> failwith "Liste vide";;

let rec set_distance l lettre =
  match l with
    |[]->[]
    |a::l'->
      let distance =abs(distanceAll a.pos lettre) in
      if distance > 13 then (insert_dis a (27 - distance))::(set_distance l' lettre)
      else (insert_dis a distance)::(set_distance l' lettre);;

let rec ecrire_antenne nb = 
    if nb = 0 then [] else (ecrire_antenne 0)@['S']@(split_string(string_of_int nb));;

let rec init_liste_antenne acc nb_antenne =
  let antenne = {num=ecrire_antenne acc;pos=' ';dis=0} in
  if acc <= nb_antenne then antenne::(init_liste_antenne (acc+1) nb_antenne) else [];;

let rec modifier_antenne num_antenne lettre antenne  =
  if antenne.num = num_antenne then insert_pos antenne lettre else antenne;;

let rec modifier_liste liste_antenne num_antenne lettre =
  List.map (modifier_antenne num_antenne lettre) liste_antenne;;

let rec lecture input liste_antenne =
  match input with
    |[] -> []
    |a::l' ->
      let choix_antenne = min_antenne_liste(set_distance liste_antenne a) in
      let creation = creation_liste (distance3 choix_antenne.pos) a in
      let liste_suivante = modifier_liste liste_antenne choix_antenne.num a in
      (choix_antenne.num@creation)::(lecture l' liste_suivante);;

let test2 = ['C';'C';'S'];;

let test = ['A';'Z'];;

let _ = List.concat (lecture test (init_liste_antenne 1 2));;

let rec imprimer_liste l =
  match l with
    |[] -> failwith "Empty list"
    |[a] -> Printf.printf "%c" a
    |a::l' -> imprimer_liste l';;

let rec print_list myList =
  match myList with
    | [] -> print_endline "\n Ceci est la sequence d envoie!"
    | head::body -> 
      begin
	print_char head;
	print_list body
      end;;

Printf.printf "En tapant 1 vous affichez l aide il faut ensuite recompiler. Tapez n'importe quel autre chiffre pour ne pas l'afficher\n";;
Printf.printf "Rentrez le nombre d antenne a utiliser! Pressez ENTER\n";;
Printf.printf "Rentrez le message a transmettre a nos voisins intergalactiques! Pressez ENTER\n";;

let rec compteur_duree list init =
  match list with
    |[] -> init
    |a::l' ->
      if a = 'N' then 3 + compteur_duree l' init else
	if a = 'P' then 3 + compteur_duree l' init else
	  if a = 'E' then 5 + compteur_duree l' init else
	      match l' with
		|[] -> compteur_duree l' init
		| b::l'' ->
		  1 + compteur_duree l'' init;;

let lire_entree_option () =
  int_of_string (read_line ());;

let cat nom_fichier =
  let f = open_in nom_fichier in
  let rec cat_rec() =
    try
      print_string(input_line f);print_newline(); cat_rec();
    with End_of_file-> close_in f
  in cat_rec();;

let main () =
  let option = lire_entree_option () in
  if option = 1 then cat "help.txt" else
    let (a,b) = parse_input () in
    let tab = List.concat (lecture b (init_liste_antenne 0 (a-1))) in
    let duree = compteur_duree tab 0 in
    print_list tab ; Printf.printf "Compteur : %i sec\n" duree;;
			

let _ = main();;


