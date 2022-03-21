module Vertex = struct
  type t = Node of int
                     
  let compare a b =
    match a, b with
      Node i1, Node i2 -> i1 - i2
                                 
  let get_index n =
  match n with
    Node i -> i
                    
  let new_couple_node n1 n2 t2 =
    let (i1,i2)=
      (match (n1,n2) with
         (Node v1, Node v2) -> (v1,v2)
      ) in
    let res = i1*(t2+1)+i2
    in
    (*print_int res;print_string "(";print_int i1;print_string ",";
      print_int i2;print_string ")\n";*)
    Node (res)
  let print n =
    match n with
    | Node n -> print_int n
end
