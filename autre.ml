let rec existe g a =
  match g with
    |[] -> false
    |arc::g' ->
      if arc = a then true else existe g' a;;

let _ = existe [((1,2),3)] ((1,2),4);;

let rec add lettre l g =
  match l with
    |[] -> []
    | e::l' ->
      if existe g ((lettre,e),0) then add lettre l' g else add lettre l' (((lettre,e),0)::g);;

let rec add_list l1 l2 g =
  match l1 with
    |[]->failwith "Empty"
    |[a]->(("S1",a),0)
    |a::l1' ->
      let g' = add a l2 g in
      add_list l1' l2 g';;

let rec add_antenne l1 g =
  let a =List.hd l1 in
	match l1 with
	  |[]-> []
	  |[a] -> (((a,a),0)::g)
	  |b::l1' ->
	    add_antenne l1' (((a,b),0)::g);;

let create_graphe l1 l2 g=
  (add_list l1 l2 g)::(add_antenne l1 g);;

let _ = create_graphe ["E";"B";"Z"] ["S0";"S1";"S1"] [];;

let list_vertices graph =
  List.fold_left (fun acc ((a, b), _) ->
    let acc = if List.mem b acc then acc else b::acc in
    let acc = if List.mem a acc then acc else a::acc in
    acc
  ) [] graph
 
let neighbors v =
  List.fold_left (fun acc ((a, b), d) ->
    if a = v then (b, d)::acc else acc
  ) []
 
let remove_from v lst =
  let rec aux acc = function [] -> failwith "remove_from"
  | x::xs -> if x = v then List.rev_append acc xs else aux (x::acc) xs
  in aux [] lst
 
let with_smallest_distance q dist =
  match q with
  | [] -> assert false
  | x::xs ->
      let rec aux distance v = function
      | x::xs ->
          let d = Hashtbl.find dist x in
          if d < distance
          then aux d x xs
          else aux distance v xs
      | [] -> (v, distance)
      in
      aux (Hashtbl.find dist x) x xs
 
let dijkstra max_val zero add graph source target =
  let vertices = list_vertices graph in
  let dist_between u v =
    try List.assoc (u, v) graph
    with _ -> zero
  in
  let dist = Hashtbl.create 1 in
  let previous = Hashtbl.create 1 in
  List.iter (fun v ->                  (* initializations *)
    Hashtbl.add dist v max_val         (* unknown distance function from source to v *)
  ) vertices;
  Hashtbl.replace dist source zero;    (* distance from source to source *)
  let rec loop = function [] -> ()
  | q ->
      let u, dist_u =
        with_smallest_distance q dist in   (* vertex in q with smallest distance in dist *)
      if dist_u = max_val then
        failwith "vertices inaccessible";  (* all remaining vertices are inaccessible from source *)
      if u = target then () else begin
        let q = remove_from u q in
        List.iter (fun (v, d) ->
          if List.mem v q then begin
            let alt = add dist_u (dist_between u v) in
            let dist_v = Hashtbl.find dist v in
            if alt < dist_v then begin       (* relax (u,v,a) *)
              Hashtbl.replace dist v alt;
              Hashtbl.replace previous v u;  (* previous node in optimal path from source *)
            end
          end
        ) (neighbors u graph);
        loop q
      end
  in
  loop vertices;
  let s = ref [] in
  let u = ref target in
  while Hashtbl.mem previous !u do
    s := !u :: !s;
    u := Hashtbl.find previous !u
  done;
  (source :: !s)
 
let () =
  let graph =
    [ ("S0", "E"), 25;
      ("E", "B"), 15;
      ("E2", "Z"), 30;
      ("B1","E2"),1;
      ("E", "S1"), 1;
      ("B", "S11"), 1;
      ("B", "Z"), 15;
      ("S11", "Z"), 5;
      ("S1","B1"), 10 ;]
  in
  let p = dijkstra max_int 0 (+) graph "S0" "Z" in
  print_endline (String.concat " : " p)
