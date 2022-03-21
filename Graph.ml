open Vertex
open PosExpression
open LabelExpression
open Edge
open RegExpGenerator

(*int->set edge : edge -> int*labelExpressionset*)
module Graph = Map.Make (Vertex)
                       
module IntInt = struct
  type t = int * int
  let compare = Stdlib.compare
end
                  
module IntIntSet = Set.Make (IntInt)
                           
module IntMap = Map.Make (Int)

module PriorityQueue = Psq.Make (Vertex) (Int)

let graph_printer g = 
  Graph.iter (fun key value -> 
      print_string "From "; Vertex.print key; print_string " : \n";
      EdgeSet.iter (Edge.print) value) g

let int_graph_printer g =
  Graph.iter (fun key set -> 
      print_string "From "; Vertex.print key; print_string " : \n";
      IntIntSet.iter (fun v -> print_string "To :";
                               print_int (fst v); print_string " -> ";
                               print_int (snd v);
                               print_string "\n"
        ) set) g

(* add a Forward of a f_pos and a Backward of a b_pos to a set of pos expressions *) 
let add_expressions f_pos b_pos set =
  let f_set = PosExpressionSet.add (Forward f_pos) set
  in PosExpressionSet.add (Backward b_pos) f_set

(* update the IntRegexMap m at the key k by adding the content of the RegexpSet v 
   to the value of the map at the key k*)
let add_to_map k v m =
  try
    let s = IntRegexpMap.find k m in 
    let new_set = 
      RegexpSet.fold (fun v acc ->
          RegexpSet.add v acc) v s
    in
    IntRegexpMap.add k new_set m
  with
    Not_found -> IntRegexpMap.add k v m

(* from the IntIntRegexMap, creates two maps, one which has as key the the left side of the 
IntIntRegexMap key and the other map which has as key the right side of the IntIntRegexpMap key, 
as values they will share the same *)
let before_after_maps m =
  let maps = (IntRegexpMap.empty, IntRegexpMap.empty) in
  IntIntRegexpMap.fold (fun (i,j) v acc ->
      let new_i_map = add_to_map i v (fst acc) in
      let new_j_map = add_to_map j v (snd acc) in
      new_i_map, new_j_map
    ) m maps 

(* add to the set s the regexp of the maps at the index i,
it will check the regexps of the first map at key i and decorate them
with a Before, then it will check the regexps of the second map at key i and
decorate them with an After *)
let add_before_after s i maps =
  let aux s i m before =
    try
      let set = IntRegexpMap.find i m in
      let add = RegexpSet.fold (fun v acc ->
                    if before then
                      PosExpressionSet.add (Before v) acc
                    else
                      PosExpressionSet.add (After v) acc) set s
      in add
      with
        Not_found -> s
      in
      let i_map = fst maps in
      let j_map = snd maps in
      let i_set = aux s i i_map true in
      let j_set = aux i_set i j_map false
      in j_set

let extract_maker pos in_len out_len maps = 
  let left_set = PosExpressionSet.empty in
  let right_set = PosExpressionSet.empty in 
  let left = add_expressions pos (in_len - pos) left_set in
  let right = add_expressions (pos + out_len) (in_len - pos - out_len) 
      right_set in
  let left_before_after = add_before_after left pos maps in
  let right_before_after = add_before_after right (pos + out_len) maps in
  LabelExpression.Extract(left_before_after, right_before_after)
   
(*
a version not optimized which does the job of add_before_after but without
creating maps, only using the initial IntIntRegexpMap 
let transfer s1 s2 before =
  RegexpSet.fold(fun x acc ->
                  if before then
                    PosExpressionSet.add (Before x) acc
                  else PosExpressionSet.add (After x) acc) s1 s2
                            

let extract_maker pos in_len out_len maps = 
  let left_set = PosExpressionSet.empty in
  let right_set = PosExpressionSet.empty in 
  let left = add_expressions pos (in_len - pos) left_set in
  let right = add_expressions (pos + out_len) (in_len - pos - out_len) 
      right_set in

  let m1 = IntIntRegexpMap.filter (fun (i,_) _ -> if i = pos then true else false) maps in
  let m2 = IntIntRegexpMap.filter (fun (_,i) _ -> i = pos) maps in
  let m3 = IntIntRegexpMap.filter (fun (i,_) _ -> i = (pos + out_len)) maps in
  let m4 = IntIntRegexpMap.filter (fun (_,i) _ -> i = (pos + out_len)) maps in
  let s1 = IntIntRegexpMap.fold(fun _ v acc ->
      transfer v acc true) m1 left in

  let s2 = IntIntRegexpMap.fold(fun _ v acc ->
      transfer v acc false) m2 s1 in
  let s3 = IntIntRegexpMap.fold(fun _ v acc ->
      transfer v acc true) m3 right in
  let s4 = IntIntRegexpMap.fold(fun _ v acc ->
      transfer v acc false) m4 s3 in
  LabelExpression.Extract(s2, s4)   
 *)

(* look in the input for all the substring positions values to
create the forward and before, these positions will also be used
to look for the After and Before *)                        
let position input in_len sub set maps = 
  let sub_len = String.length sub in 
  let rec aux str ind acc = 
    match ind with
    | _ when ind = in_len -> acc
    | _ -> 
        try 
          let in_sub = String.sub str 0 sub_len in
          if in_sub = sub then 
            let extract = extract_maker ind in_len sub_len maps in
            aux (String.sub str sub_len (in_len - sub_len - ind))
              (ind + sub_len) (LabelExpressionSet.add extract acc)
          else
            aux (String.sub str 1 (in_len - 1 - ind))
              (ind + 1) acc
        with Invalid_argument _ -> acc
  in aux input 0 set
    
let label_maker input output in_len key value maps = 
  let sub_len = value - key in 
  let sub = String.sub output key sub_len in 
  let const_set = LabelExpressionSet.add (Const sub) LabelExpressionSet.empty in
  Vertex.Node(value),  position input in_len sub const_set maps

(* create all the transitions for a key k *)
let transition_single input output in_len out_len key map maps =
  let rec aux value acc = 
    match value with
    | _ when value = key -> Graph.add (Vertex.Node(key)) acc map
    | _ -> let new_set = EdgeSet.add (label_maker input output in_len 
                                        key value maps) acc in
        aux (value - 1) new_set
  in aux (out_len) EdgeSet.empty

(* create the transitions for every key k *)
let transition_all input output maps =
  let in_len = String.length input in
  let out_len = String.length output in
  let rec aux key acc =
    match key with
    | _ when key = (out_len) -> Graph.add (Vertex.Node key) EdgeSet.empty acc 
    | _ -> aux (key + 1) (transition_single input output in_len out_len key acc maps)
  in aux 0 Graph.empty

(* create a map which will attribute an increasing index to every index encountered 
in the Graph, the first element of the pair is the increasing index,
the second element is the Map *)
let index_map g =
  snd (Graph.fold (fun k _ acc ->  
      match k with
      | Node i ->
          (fst acc + 1), 
          (IntMap.add i (fst acc) (snd acc))) g (0, IntMap.empty))
  
let index_map_printer m = IntMap.iter
                            (fun k v -> print_int k; print_string "->"; print_int v; print_string "\n") m
   
(* reindex the vertices in a edge set with the help of a int map *)
let edge_reindex int_map edge_set =
  EdgeSet.fold (fun v acc ->
      let new_edge = Vertex.Node((IntMap.find (Vertex.get_index(fst v)) int_map)), (snd v)
      in EdgeSet.add new_edge acc) edge_set EdgeSet.empty

(* reindex the values of the vertices of a graph *)
let reindex g =
  let int_map = index_map g in
  (* let _ = index_map_printer int_map in *)
  snd (Graph.fold (fun _ v acc ->
      (fst acc + 1),
      (Graph.add (Node (fst acc)) (edge_reindex int_map v) (snd acc)))
         g (0, Graph.empty))

(* create a new graph where the edges will be decorated with the weight of the expression set
instead of the expression set like in the initial graph *)
let edges_converter g =
  Graph.fold (fun k set acc ->
      let set = EdgeSet.fold (fun elt acc ->
          let weight = fst (Edge.label_set_minimum (snd elt)) in
          IntIntSet.add (Vertex.get_index(fst elt), weight) acc) set IntIntSet.empty in
      Graph.add k set acc) g Graph.empty

(* create a precedence array *)
let dijkstra g source =
  let converted_graph = edges_converter g in
  let size = Graph.cardinal converted_graph in
  let dist = Array.init size (fun i -> if i = (Vertex.get_index source) then 0 else max_int) in
  let pred = Array.init size (fun i -> if i = (Vertex.get_index source) then 0 else (-1)) in
  let pq = ref (PriorityQueue.add source 0 PriorityQueue.empty) in
  while not (PriorityQueue.is_empty !pq) do
    let min = PriorityQueue.pop !pq in
    match min with
    | Some ((u, _), rest) ->
    let ref_rest = ref rest in
    pq := !ref_rest;
    let neighbours = Graph.find u converted_graph in
    let u_index = Vertex.get_index u in
    IntIntSet.iter (fun (v, w) ->
        let new_dist = dist.(u_index) + w in (* w is the distance between u and w *)
        if dist.(v) > new_dist then
          begin
          pq := PriorityQueue.add (Node v) new_dist !pq;
          pred.(v) <- u_index;
          dist.(v) <- new_dist;
          end
      ) neighbours
    | None -> failwith "None"     
  done;
  pred

(*check the expression which has the minimum weight in the edge between n1 and n2 *)
let cheapest_expr g n1 n2 =
  let edge_set = Graph.find n1 g in
  let edge = EdgeSet.find_first (fun (Node(ind), _) -> if ind = Vertex.get_index n2 then true else false) edge_set in
  let label_expr_minimum = Edge.label_set_minimum (snd edge) in
  snd label_expr_minimum
                             
let extract_program g =
  let pred = dijkstra g (Vertex.Node 0) in
  let size = Graph.cardinal g in
  let rec aux index acc =
    let index_node = Vertex.Node index in
    if pred.(index) = 0 then (cheapest_expr g (Vertex.Node 0) index_node) :: acc
    else if pred.(index) = (-1) then raise Not_found
    else
      let parent = pred.(index) in
      aux parent ((cheapest_expr g (Vertex.Node parent) index_node) :: acc) 
  in aux (size - 1) []

let print_program l =
  let print_expression e =
  match e with
    | AST.Const s -> print_string ("const(\"" ^ s ^ "\")")
    | Extract (s1, s2) -> 
        print_string "extract(";
        PosExpression.print s1;
        print_string ",";
        PosExpression.print s2;
        print_string ")";
  in
  let rec printer l =
    match l with
    | [] -> print_string "\n"
    | hd :: [] -> print_expression hd; printer []
    | hd :: tl -> print_expression hd; print_string "; "; printer tl
  in
  printer l

let new_vertex_index i1 i2 t2 =
  i1*(t2+1)+i2

(* Fait l'intersection de extract contenant des ensembles de posexpression *)
let extract_custom_intersection setextract2 elt acc =
  let my_function =  (fun elt2 b ->
      let new_extract=LabelExpression.fusion_label elt elt2 in
      match new_extract with
        LabelExpression.Extract(s1,s2)->
        if (PosExpressionSet.is_empty s1)
        ||(PosExpressionSet.is_empty s2) then
          b
        else
          LabelExpressionSet.add new_extract b
      | _ -> failwith "absurde2"
    ) in
  LabelExpressionSet.fold my_function setextract2 acc

(* Fusion des labels d'arète  *)
let my_label_fusion setlabel1 setlabel2 =
  let setconsts=LabelExpressionSet.inter
      (LabelExpressionSet.filter LabelExpression.is_const setlabel1)
      (LabelExpressionSet.filter LabelExpression.is_const setlabel2) in
  let setextract1=(LabelExpressionSet.filter (fun x->not(LabelExpression.is_const x)) setlabel1) in
  let setextract2=(LabelExpressionSet.filter (fun x->not(LabelExpression.is_const x)) setlabel2) in
  let my_fun=(extract_custom_intersection setextract2) in
  let setextract=LabelExpressionSet.fold my_fun setextract1 LabelExpressionSet.empty in
  LabelExpressionSet.union setconsts setextract

(* fusion des aretes (2eme iteration) *)
let second_couple_edge  t2 set_D2 elt1 acc =
  let (i1,setlabel1)=elt1 in
  EdgeSet.fold (fun elt2 b ->
      let (i2,setlabel2) = elt2 in
      let label_fusion =  (my_label_fusion setlabel1 setlabel2) in
      if LabelExpressionSet.is_empty label_fusion then
        b
      else
          let direction = Vertex.new_couple_node i1 i2 t2 in
            EdgeSet.add ((direction),label_fusion) b
       )
     set_D2 acc

let couple_edge t2 set_D1 set_D2=
  EdgeSet.fold (second_couple_edge t2 set_D2) set_D1 EdgeSet.empty

(* Pour chaque couple clé val de g1 pour chaque couple clé val de g2 je crée le noeud (clé1,clé2)
   pour chaque arete dans val1 et pour chaque arete dans val2 je crée une arete (cle1,clé2) vers tout elt de setD1*setD2*)

(*2eme iteration sur les noeuds pour faire le produit
  cartesien des noeuds*)
    
let second_cartesian_fold puit t2 g2 key data acc =
  Graph.fold (fun snd_key snd_data b ->
      let new_vertex = (Vertex.new_couple_node key snd_key t2) in
      let new_EdgeSet = (couple_edge t2 data snd_data) in
      if EdgeSet.is_empty new_EdgeSet && not(new_vertex = Vertex.Node(puit)) then
          b
      else
        Graph.add new_vertex new_EdgeSet b)g2 acc

(* Enleve des arètes les noeuds qui n'existent plus *)
let clear_edgeset intersect  =
  Graph.fold (fun node edge_set b ->
      let cleared_edge_set=EdgeSet.filter                 
          (fun (node_destination,_) ->
             Graph.mem (node_destination) intersect ) edge_set in
        Graph.add node cleared_edge_set b
             ) intersect Graph.empty

(* Fait l'intersection entre 2 noeuds *)
let intersect g1 g2 =
   let t2=(Graph.cardinal g2) - 1 in 
   let t1=(Graph.cardinal g1) - 1 in
   let puit = new_vertex_index t1 t2 t2 in
   let uncleared_intersect=Graph.fold (second_cartesian_fold puit t2 g2) g1 Graph.empty in
   let intersect = clear_edgeset uncleared_intersect in
   reindex intersect

(* Fait l'intersection de tout les graphe d'une liste*)
let rec intersection_all_graphs l =
  match l with
    [] -> failwith "fichier vide"
  |[h]-> h
  |h::t::tl ->
    let inter_aux = intersect h t in
    intersection_all_graphs (inter_aux::tl)

                                                           
     
