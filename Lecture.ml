
(* Recupere l'indice de tabulation du string *)
let rec get_tabulation s n =
  if s.[n] = '\t' then
    n
  else
    get_tabulation s (n+1)
      


(* Renvoie la liste des couples (i,j)
  ou i et j sont des string qui sont séparé par
   \t dans le fichier filename *)
let file_to_list filename =
  let file = open_in filename in
  let rec fill_list acc =
    try
      let line = input_line file in
      let tab_index=get_tabulation line 0 in
      fill_list (((String.sub line 0 tab_index),(String.sub line (tab_index+1) ((String.length line)-(tab_index+1))))::acc)
    with End_of_file -> acc
  in fill_list []
    


