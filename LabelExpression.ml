open PosExpression
open AST
       

module LabelExpression = struct
  type t = 
      Const of string 
    | Extract of PosExpressionSet.t * PosExpressionSet.t

  (* returns the minimum pos expression from a pos expression set,
   Forward 1 acts here as a dummy value *)
  let pos_expr_set_minimum s =
    PosExpressionSet.fold (fun x acc ->
        let pos_expr_weight = PosExpression.pos_expr_weight x in
          if (fst acc = -1) || (fst acc > pos_expr_weight) then (pos_expr_weight, x)
          else acc
      ) s (-1, Forward 1)

  (* returns the value of the minimum expression from a label and the expression with the lowest weight *)            
  let label_expr_minimum e =
    match e with
    | Const s -> 1, AST.Const s
    | Extract (s1, s2) ->
       let s1_minimum = pos_expr_set_minimum s1 in
       let s2_minimum = pos_expr_set_minimum s2 in
       (fst s1_minimum) + (fst s2_minimum), AST.Extract((snd s1_minimum, snd s2_minimum))
                                                       
  let compare = Stdlib.compare

  let is_const e =
    match e with
      Const _ -> true
    | _ -> false
             
  let fusion_label e1 e2 =
    match (e1,e2) with
      (Extract(setf1,setf2),Extract(sets1,sets2))->
      let set_res1 = (PosExpressionSet.inter setf1 sets1) in
      let set_res2 = (PosExpressionSet.inter setf2 sets2) in
      Extract(set_res1,set_res2)
    |_-> failwith "absurde"
        
  let print e = 
    match e with
    | Const s -> print_string ("Const(\"" ^ s ^ "\")")
    | Extract (s1, s2) -> 
        print_string " extract{";
        PosExpressionSet.iter (PosExpression.print) s1; 
        print_string "},";
        print_string "{";
        PosExpressionSet.iter (PosExpression.print) s2;
        print_string "}";
end
    
module LabelExpressionSet = Set.Make(LabelExpression)
