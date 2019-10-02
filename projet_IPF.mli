(** [distance_espace ()] donne la distance d'un caract�re au caract�re espace set � 64
@param b un char
@return un int la distance*)
val distance_espace : char -> int;;

(** [distance ()] retourne la distance entre deux caract�res
@param a un char b un char
@return un int la distance*)
val distance : char -> char -> int;;

(** [distanceAll] renvoie la distance entre deux char qui peuvent �tre aussi l'espace
@param a un char b un char
@return un int la distance*)
val distanceAll : char -> char -> int;;

(** [distance3] la distance a l'espace avec le char qui peut �tre l'espace
@param a un char
@return un int la distance*)
val distance3 : char -> int;;

(** [antenne] cr�ation d'un type antenne qui comprend un nom, une positon courante et la distance au prochain caract�re � atteindre*)
type antenne = { num : char list; pos : char; dis : int; };;

(** [insert_dis ()] insert la distance dans une antenne
@param antenne une antenne distance un int
@return une antenne*)
val insert_dis : antenne -> int -> antenne;;

(**[insert_pos()] insert une position dans une antenne
@param antenne une antenne pos un char
@return une antenne*)
val insert_pos : antenne -> char -> antenne;;

(**[min ()] retourne l'antenne minimum selon la distance
@param a1 une antenne a2 une antenne
@return une antenne*)
val min_antenne : antenne -> antenne -> antenne;;

(** [min_liste()] retourne le min d'une liste d'antenne
@param liste_antenne une liste d'antenne
@return une antenne*)
val min_antenne_liste : antenne list -> antenne;;

(** [set_distance ()] ajoute la distance dans chaque antenne d'une liste par rapport � un caract�re
@param liste_antenne une liste d'antenne lettre un char
@return une liste d'antenne*)
val set_distance : antenne list -> char -> antenne list;;

(** [modifier ()] modifie la pos d'une antenne si elle a le bonne identifiant 
@param num_antenne un char list lettre un char antenne une antenne
@return une antenne*)
val modifier_antenne : char list -> char -> antenne -> antenne;;

(** [modifier_liste] applique la fonction pr�c�dent � une liste d'antenne
@param liste_antenne une liste num_antenne char list lettre char
@return une liste d'antenne*)
val modifier_liste : antenne list -> char list -> char -> antenne list;;

(** [ecrire()] ecrit l'id d'une antenne sous la forme d'une char list
@param nb num�ro de l'antenne
@return char list*)
val ecrire_antenne : int -> char list;;

(** [init_liste ()] creer une liste d'antenne sur la donn�e de leur nombre
@param acc un accumulateur nb_antenne le nombre d'antenne
@return une liste d'antenne*)
val init_liste_antenne : int -> int -> antenne list;;

val split_string : string -> char list;;

(** [parse_input ()] lit deux lignes sur l'entr�e standard du programme. 
   La premi�re doit comporter un entier, la seconde une cha�ne de caract�res. 
   Retourne un couple composé de l'entier de la premi�re ligne et de la liste des caract�res de la seconde.
   L�ve l'exception [Failure "int_of_string"] si la premi�re ligne ne repr�sente pas un entier.
 *)
val parse_input : unit -> int * char list;;

(** [liste_sens_trigo ()] cr�er une liste comportant le nombre de 'P' puis 'E' donn� en param�tre
@param dis un int
@return une liste*)
val liste_sens_trigo : int -> char list;;

(** [liste_sens_antitrigo ()] cr�er une liste comportant le nombre de 'N' puis 'E' donn� en param�tre
@param dis un int
@return une liste*)
val liste_sens_antitrigo : int -> char list;;

(** [creation_liste()] creer la liste associ� � la lecture d'un caract�re
@param ancien int du caract�re pr�c�dent b char caract�re courant
@return liste *)
val creation_liste : int -> char -> char list;;

(**[lecture_input()]creer la liste des mouvements pour la partie 1 du projet
@param input liste de char ancien la valeur num�rique de l'ancien char 
@return une char list list*)
val lecture_input : char list -> int -> char list list;;

(** [lecture ()] cr�er la liste de mouvement des antennes en fonction d'une liste de char
@param input liste de char liste_antenne liste d'antenne
@return une liste de liste*)
val lecture : char list -> antenne list -> char list list;;

(** [print_list()] print une liste de char sur la sortie standard
@param myList une liste
@return unit *)
val print_list : char list -> unit;;

(** [compteur_duree ()] calcul la duree d'envoie d'un message
@param list une liste de char init la valeur de d�part 0
@return un int*)
val compteur_duree : char list -> int -> int;;

(** [lire_entree_option()] lit le choix d'affichage de l'aide ou pas
@param unit
@return unit*)
val lire_entree_option : unit -> int;;

(** [cat ()] lit un fichier
@param nom_fichier 
@return unit*)
val cat : string -> unit;;

(** [main ()] fonction principale du programme 
@param unit
@return unit*)
val main : unit -> unit;;
