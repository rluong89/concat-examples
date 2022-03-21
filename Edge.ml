open LabelExpression
open Vertex
       
module Edge = struct
  type t = Vertex.t * LabelExpressionSet.t

  let compare = Stdlib.compare
  let print e =
    match e with 
    | Vertex.Node(v), s -> print_string " To : "; print_int v; print_string " | "; 
                           LabelExpressionSet.iter (LabelExpression.print) s; print_string "\n" 

  let label_set_minimum label_set =
    LabelExpressionSet.fold (fun x acc ->
        let minimum = LabelExpression.label_expr_minimum x in
        if (fst acc = -1) || (fst acc > fst minimum) then minimum
        else acc       
      ) label_set (-1, AST.Const "dummy")
end

module EdgeSet = Set.Make(Edge)
