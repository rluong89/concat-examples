open Graph
open Lecture
open RegExpGenerator

let txt_file = file_to_list Sys.argv.(1)

let g = List.map (fun x -> transition_all (fst x) (snd x) (before_after_maps (generate_regexp (fst x)))) txt_file
(* version unique map *)
(* let g = List.map (fun x -> transition_all (fst x) (snd x) (generate_regexp (fst x))) txt_file *)

(* sans les regexp *)
(* let g' = List.map (fun x -> transition_all (fst x) (snd x) (IntRegexpMap.empty, IntRegexpMap.empty)) txt_file *)
(* let _ = graph_printer (List.hd g') *)
                      
let cartesian_product = intersection_all_graphs g
                                                
let l = extract_program cartesian_product
let () = print_string "\n"
let () = print_program l
