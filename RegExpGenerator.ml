open AST
(* open PosExpression *)

module IntInt = struct
  type t = int * int
  let compare = Stdlib.compare
end
    
module IntIntRegexpMap = Map.Make(IntInt)
    
module IntRegexpMap = Map.Make(Int)
    
module Regexp = struct
  type t = regexp
  let compare = Stdlib.compare
end
      
module RegexpSet = Set.Make(Regexp)

module CharSet = Set.Make(Char)                      

let handled_special_chars =
  ['/'; '-'; '('; ')']

let is_numeric c =
  c >= '0' && c <= '9'

let is_lower c=
  c >= 'a' && c <= 'z'

let is_upper c =
  c >= 'A' && c <= 'Z'

let is_special c =
  if List.mem c handled_special_chars then true else false
                                        
type char_sets =
  { class_name : string;
    mutable plus : CharSet.t;
    mutable plus_comp : CharSet.t; 
  }
  
let cs_make s =
  { class_name = s;
    plus = CharSet.empty;
    plus_comp = CharSet.empty; 
  }
  
let add_if_not_mem x l =
  if not (List.mem x l) then x :: l else l

(* will add the char c to every class in the list l 
in the char set of the plus attribute of the record *)
let add_plus l c =
  List.iter (fun x -> x.plus <- CharSet.add c x.plus) l 

(* will add the char c to every class in the list l 
in the char set of the plus_comp attribute of the record *)
let add_plus_comp l c =
  List.iter (fun x -> x.plus_comp <- CharSet.add c x.plus_comp) l 

(* update the plus and pluscomp field of the classes *)
let update plus plus_comp c =
  add_plus plus c; 
  add_plus_comp plus_comp c

(* function which will create a char list from a string s *)
let explode s =
  let rec acc i l =
    if i < 0 then l else acc (i - 1) (s.[i] :: l) in
  acc (String.length s - 1) []

(* create a char set from the string s *)
let str_to_set s =
  let ch_list = explode s in 
  List.fold_left (fun acc x -> CharSet.add x acc) 
    CharSet.empty ch_list

(* create the char sets of every class for their plus and pluscomp field *)
let sets_maker s =
  let alphanumeric = cs_make "alphanumeric" in
  let alpha = cs_make "alpha" in
  let numeric = cs_make "numeric" in
  let lower = cs_make "lower" in
  let upper = cs_make "upper" in
  let special = cs_make "special" in
  let all_classes = [special; alphanumeric; alpha; numeric; lower; upper;] in
  let length = ref 0 in
  String.iter (fun c ->
      length := !length + 1;
      match c with
      | _ when is_numeric c -> 
          let plus = [alphanumeric; numeric] in
          let plus_comp = [alpha; lower; upper] in
          update plus plus_comp c
      | _ when is_lower c -> 
          let plus = [alphanumeric; alpha; lower] in
          let plus_comp = [numeric; upper] in
          update plus plus_comp c
      | _ when is_upper c -> 
          let plus = [alphanumeric; alpha; upper] in
          let plus_comp = [numeric; lower] in
          update plus plus_comp c
      | _ when is_special c ->
          let plus = [special] in
          let plus_comp = [alphanumeric; alpha; numeric; upper; lower] in
          update plus plus_comp c;
      | _ -> 
          add_plus_comp all_classes c) s;
  all_classes

(* removes the classes which didn't match any character or all the character
of the string s *)
let cardinal_filter l s = 
  List.filter 
    (fun x -> CharSet.cardinal x.plus > 0 
              && not (CharSet.equal x.plus s)) l
    
(* removes the redundant classes for the plus field *)
let redundancy_filter_plus l =
  let rec aux classes acc l =
    match l with
    | [] -> List.rev classes
    | hd :: tl -> 
        if List.mem hd.plus acc then aux classes acc tl
        else aux (hd :: classes) (hd.plus :: acc) tl 
  in aux [] [] l

(* find the index of x in the list l *)
let pos x l =
  let rec aux l ind =
    match l with
    | [] -> -1
    | hd :: tl -> if x = hd then ind else aux tl (ind + 1) 
  in aux l 0
    
let rec redundancy_filter_aux x l acc sub =
  match l with
  | [] -> (List.rev acc), sub
  | hd :: tl -> 
      if hd = x || not (CharSet.equal x.plus hd.plus_comp)
      then (redundancy_filter_aux x tl (hd :: acc) sub)
      else if 
        (pos hd l) < (pos x l) then
        redundancy_filter_aux x tl acc (sub + 1)
      else redundancy_filter_aux x tl acc sub 
            
(* removes the redundant classes by comparing plus and pluscomp fields *)
let redundancy_filter_plus_comp l = 
  let rec aux l ind =
    try 
      let nth = List.nth l ind in
      let new_list, sub = redundancy_filter_aux nth l [] 0 in
      aux new_list (ind + 1 - sub)
    with Failure _ -> l 
  in aux l 0

(* creates a list of special char from a char set *)
let special s =
  CharSet.fold (fun x acc -> (AST.Special x :: acc)) s [] 
    
let class_of_str str s =
  match str with
  | "alphanumeric" -> [Alphanumeric]
  | "alpha" -> [Alpha]
  | "numeric" -> [Numeric]
  | "lower" -> [Lower]
  | "upper" -> [Upper]
  | "special" -> special s
  | _ -> failwith "no matching class"
    
let class_list l s = 
  List.fold_left (fun acc x -> (class_of_str x.class_name s) @ acc) [] l 
  
let pertinent_classes s = 
  let set_list = sets_maker s in
  let sp_chars = (List.hd set_list).plus in
  let set_s = str_to_set s in
  let p1 = cardinal_filter set_list set_s in 
  let p2 = redundancy_filter_plus p1 in
  (*
  let _ = List.iter (fun x -> print_string x.class_name;
                      print_string "\n") p2 in
   *)
  let p3 = redundancy_filter_plus_comp p2 in
  let class_l = class_list p3 sp_chars in
  (*
  List.iter PosExpression.print_class class_l;
  print_string "\n";
   *)
  class_l

(* verifie si un charactere est dans une classe *)
let rec char_in_class cl character =
   match cl with
       Alphanumeric->
       (char_in_class Alpha character)||(char_in_class Numeric character)
  | Numeric -> is_numeric character
  | Alpha -> (char_in_class Lower character)||(char_in_class Upper character)
  | Lower -> is_lower character
  | Upper -> is_upper character
  | Special (c) -> c=character

(* teste si un charactere est dans un token *)
let char_in_token token character =
  match token with
    Plus(cl) -> char_in_class cl  character
  |PlusComp(cl) -> not(char_in_class cl  character)

(* Mange les tokens d'une regexp et renvoie le couple (i,j) sur lequel 
   les tokens on matcher (-1,-1) si la regexp ne matche pas *)
                     
let token_eat tokens input start =
  let length = String.length input in
  let rec aux tokens index acc =
    if index < length then
  (match tokens with
      hd::tl -> let pointer = input.[index] in
      if char_in_token hd pointer then
        aux tokens (index+1) (pointer::acc)
      else
        (
      if acc=[] then (-1,-1) else aux tl index [])
    |[] -> (start,index))
  
    else
      (match tokens with
         _::t ->if acc=[] then (-1,-1) else if t=[] then (start,index) else (-1,-1)
       |[] -> (start,index)
      )    
  in aux tokens start []

(* trouve tout les 1ers matchs dans une chaine *)
let get_all_matches_index token input =
  let length = String.length input in
  let rec find_match  index =
    if index=length then [] else(
    let pointer = input.[index] in
    if char_in_token token pointer then
      index::(ignore_match index)
    else
      find_match (index+1))
  and ignore_match index =
    if index=length then [] else(
    let pointer = input.[index] in
    if char_in_token token pointer then
      (ignore_match (index+1))
    else
      find_match (index))
  in
  find_match 0
      
(* gére les constructeur start et end *)
let handle_start_end  tokens input start_epsilon end_epsilon =
  match (start_epsilon,end_epsilon) with
    (Start,_) -> let (i,j) = token_eat tokens input  0 in 
    if (i=0) then (i,j) else (-1,-1)
  | (_,End) ->
    let rev_string = BatString.rev input in
    let rev_tokens = List.rev tokens in
    let l = String.length input in
    let (ires_aux,jres_aux) = token_eat rev_tokens rev_string  0 in
    let (ires,jres) = (l-jres_aux,l-ires_aux) in
    let length = String.length input in
    if not(jres=length) then (-1,-1) else (ires,jres)
  | (Epsilon_d,Epsilon_f) ->
    let first_matchs = get_all_matches_index (List.hd tokens) input in 
    if not(first_matchs=[]) then
      if List.length tokens > 1 then
        (
      let find_match =
        List.find_opt (fun x -> not((-1,-1)=token_eat tokens input x))
          first_matchs in
      match find_match with
        None -> (-1,-1)
      |Some x -> token_eat tokens input x
    )
      else
        let tete = List.hd first_matchs in
        token_eat tokens input tete
      else (-1,-1)

(* teste si une regexp matche l'input entre i et j *)
let does_it_match regexp input (i,j) =
  let (ini,tokens,fin) = regexp in
  (i,j)=(handle_start_end   tokens input ini fin)

(* renvoie la liste des couples jusqu'a length *)
let rec couple i j length= 
  if j > length then
    if i>= length then
      []
    else
      (couple (i+1) (i+2) length)
  else
    (i,j)::(couple i (j+1) length)

(* Renvoie la liste des couples qui sont prefixe de (i,j) *)
let prefixe (i,j) couples = List.filter (fun (x,y) -> i=x && y<j) couples
                                        
(* Renvoie la liste des couples qui sont avant (i,j) i.e. on peut les coller ajouter au début*)
let prefixe_end (i,_) couples = List.filter (fun (x,y) -> x<i && y<=(i+1)) couples

(* cree une regexp a partir de la liste des tokens *)
let create_basic_regexp fin classe =
  (Epsilon_d,[classe],fin)

let print_ini ini =
  match ini with
    Epsilon_d -> print_string "€"
  |Start -> print_string "Start "

let print_fin fin =
  match fin with
    Epsilon_f -> print_string "€"
  |End-> print_string " End"

let print_class classe =
  match classe with
        Alphanumeric-> "Alphanumeric"
      | Numeric -> "Numeric" 
      | Alpha -> "Alpha"
      | Lower -> "Lower"
      | Upper -> "Upper"
      | Special _ -> "Special"

let print_token token =
  match token with
    Plus(cl) -> print_string ("Plus("^print_class cl^") ")
  |PlusComp(cl)-> print_string ("PlusComp("^print_class cl^") ")

let print_tokens tokens = List.iter ( fun x -> print_token x ) tokens

let print_regexp regexp =
  let (ini,tokens,fin) =regexp in
   print_ini ini;print_tokens tokens;print_fin fin

(* Si la regexp matche aux indices (i,j) alors renvoie un 
   nouvel ensemble de regexp contenant la regexp qui match 
   en plus *)
     
let new_map_regexp regexp input (i,j) set_regexp =
  let plus_match = does_it_match regexp input (i,j) in
  let map_regexp_plus =
    (if plus_match then 
     (RegexpSet.add regexp set_regexp)
    else
      set_regexp) in
  map_regexp_plus

(* Ajoute un token a la fin d'une regexp *)
let add_token_at_end plus pluscomp elt fin=
  match elt with
    (ini,list,_) -> ((ini,list@[plus],fin),(ini,list@[pluscomp],fin))

(* Generation dynamique de regexp *)
let create_dynamically_new_set input set set1_regexp plus pluscomp fin (i,j)=
  RegexpSet.fold
    (fun elt new_set ->
       let (reg_plus,reg_comp) = add_token_at_end plus pluscomp elt fin in
       let (_,test_length,_) = reg_plus in
       if (List.length (test_length) < 5 ) then
         let set_regexp_plus_complex = new_map_regexp reg_plus input (i,j) new_set in
         let set_regexp_pluscomp_complex = new_map_regexp reg_comp input (i,j) set_regexp_plus_complex in
         let aux = set_regexp_pluscomp_complex in
         (if fin = End  then
            let (reg_plus,reg_comp) = add_token_at_end plus pluscomp elt Epsilon_f in
            let set_regexp_plus_complex_end = new_map_regexp reg_plus input (i,j) aux in
            let set_regexp_pluscomp_complex_end = new_map_regexp reg_comp input (i,j) set_regexp_plus_complex_end in
            set_regexp_pluscomp_complex_end
          else
            aux
         )
       else
         new_set
    ) set set1_regexp 

(* premiere passe de generation *)
let first_iter liste_token liste_couple input length  =
  List.fold_left (fun map_regexp (i,j) ->
      let fin = (if j >= length then End else Epsilon_f) in
      let regexpset=List.fold_left (fun set_regexp (plus,pluscomp) ->
          let plus_regexp = create_basic_regexp fin plus in
          let pluscomp_regexp = create_basic_regexp fin pluscomp in
          let set_regexp_plus = new_map_regexp plus_regexp input (i,j) set_regexp in 
          let set_regexp_pluscomp = new_map_regexp pluscomp_regexp input (i,j) set_regexp_plus in
          let res_regexp_long1 = set_regexp_pluscomp in
          let prefixes = prefixe (i,j) liste_couple in
          List.fold_left (fun set1_regexp (x,y) ->
              match (IntIntRegexpMap.find_opt (x,y) map_regexp) with
                None -> set1_regexp
              | Some(set) ->  create_dynamically_new_set input set set1_regexp plus pluscomp fin (i,j)
            ) res_regexp_long1 prefixes
        ) RegexpSet.empty liste_token in
      IntIntRegexpMap.add (i,j) regexpset map_regexp
    ) IntIntRegexpMap.empty liste_couple

(* Ajoute un token au début de la regexp *)
let add_token_at_beginning plus pluscomp elt=
  match elt with
    (ini,list,_) -> ((ini,plus::list,End),(ini,pluscomp::list,End))


(* crée les regexp de fin dynamiquement i.e. en ajoutant au début *)
let create_regexp_end_dynamically map_first_iter (i,j) map_inter3 x plus pluscomp input=
  let set_regexp=IntIntRegexpMap.find (i,j) map_first_iter in
                let aux = IntIntRegexpMap.find (x,j) map_first_iter in
                let aux_bis =
                  (match IntIntRegexpMap.find_opt (x,j) map_inter3 with
                     None -> RegexpSet.empty
                   | Some s -> s ) in
                let new_set = RegexpSet.fold
                    (fun elt new_set ->
                       let (reg_plus,reg_comp) = add_token_at_beginning plus pluscomp elt in
                       let (_,test_length,_) = reg_plus in
                       if (List.length (test_length) < 5 ) then
                         let map_regexp_plus_complex =
                           new_map_regexp reg_plus input (x,j) new_set in
                         let map_regexp_pluscomp_complex =
                           new_map_regexp reg_comp input (x,j) map_regexp_plus_complex in
                         map_regexp_pluscomp_complex
                       else
                         new_set
                    ) set_regexp aux
                in
                let union_sets=(RegexpSet.union new_set aux_bis) in
                IntIntRegexpMap.add (x,j) union_sets map_inter3

(* Genere toutes les expressions reguliere d'une input *)
let generate_regexp input =
  let length = String.length input in
  let liste_filtrer = pertinent_classes input in
  let liste_token = List.map (fun x -> (Plus(x),PlusComp(x))) liste_filtrer in
  let liste_couple = couple 0 1 length in
  (* Premiere passe pour generer les regexp avec prog dynamique *)
  
  let map_first_iter = first_iter liste_token liste_couple input length in

  (* 2éme passe pour les regexp avec End on ajoute au début
     lors de la phase de programmation dynamique *)
  
  let map_res = List.fold_left (fun  map_intermediaire (i,j)->
      let prefixes = prefixe_end (i,j) liste_couple in
      let map_intermediaire =
        IntIntRegexpMap.add (i,j) (IntIntRegexpMap.find (i,j) map_first_iter) map_intermediaire in
      let new_map =
        List.fold_left (fun map_inter2 (plus,pluscomp) ->
            List.fold_left(fun map_inter3 (x,_) ->
                 create_regexp_end_dynamically map_first_iter (i,j) map_inter3 x plus pluscomp input
              ) map_inter2 prefixes
          ) map_intermediaire liste_token in
      new_map
    ) IntIntRegexpMap.empty liste_couple in
  map_res
