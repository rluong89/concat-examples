open AST
       
module PosExpression = struct
  type t = pos_expression

  let class_weight c =
    match c with
    | Alphanumeric -> 2
    | Alpha -> 2
    | Numeric -> 2
    | Lower -> 2
    | Upper -> 2
    | Special _ -> 3
    
  let regexp_weight tk =
    List.fold_left (fun acc x ->
        match x with
        | Plus(c) | PlusComp(c) -> class_weight c + acc) 0 tk
      
  let pos_expr_weight pos_expr =
    match pos_expr with 
    | Forward _ | Backward _ -> 1
    | After (_, tk, _) | Before (_, tk, _) -> regexp_weight tk
  (*
  let compare a b =
    match a, b with
    | Forward _, Forward _ | Backward _, Backward _ -> if a = b then 0 else -1
    | Forward _, _ | Backward _, (After _ | Before _) -> -1 
    | Backward _, Forward _ | 
      (After _ | Before _), (Forward _ | Backward _) -> 1 
    | _ -> failwith "compare_before_after a b"  
   *)

  let compare = Stdlib.compare

  let print_ini (ini : AST.ini) =
    match ini with
    | Epsilon_d -> print_string ""
    | Start -> print_string "Start"

  let print_fin (fin : AST.fin) =
    match fin with
    | Epsilon_f -> print_string ""
    | End -> print_string "End"

  let print_class cl =
    match cl with
    | Alphanumeric -> print_string "Alphanumeric"
    | Alpha -> print_string "Alpha"
    | Numeric -> print_string "Numeric"
    | Lower -> print_string "Lower"
    | Upper -> print_string "Upper"
    | Special c -> print_string "Special(";
                   print_char c;
                   print_string ")"

  let print_token tk =
    match tk with
    | Plus(cl) -> print_string "Plus("; print_class cl; print_string ")"
    | PlusComp(cl) -> print_string "PlusComp("; print_class cl; print_string ")"

  let rec print_token_list l =
    match l with
    | [] -> ()
    | hd :: [] -> print_token hd
    | hd :: tl -> print_token hd; print_string ";"; print_token_list tl
    
  let print_class_list l =
    print_string "[";
    let rec aux l =
    match l with
    | [] -> print_string "Pas de classe pertinentes \n"
    | hd :: [] -> print_class hd; print_string "]\n"
    | hd :: tl -> print_class hd; print_string ";"; aux tl
    in aux l

  let print e = 
    match e with
    | Forward i -> print_string "f("; print_int i; print_string ")"
    | Backward i -> print_string "b("; print_int i; print_string ")"
    | After (ini, l, fin) ->
       print_string "After(";
       print_ini ini;
       print_token_list l;
       print_fin fin;
       print_string ")"
    | Before (ini, l, fin) ->
       print_string "Before(";
       print_ini ini; print_token_list l;
       print_fin fin;
       print_string ")"
end

module PosExpressionSet = Set.Make(PosExpression)
