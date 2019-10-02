

let distance_espace b =
  int_of_char b - 64;;

let distance a b =
  int_of_char a - int_of_char b;;

let distanceAll a b =
  if a = ' ' then
    if b = ' ' then 0 else distance_espace b
  else
    if b = ' ' then distance_espace a else distance a b;;

let distance3 a = if a = ' ' then 0 else distance_espace a;;
