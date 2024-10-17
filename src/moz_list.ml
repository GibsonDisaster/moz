let rec remove_duplicates l =
  let rec contains l n =
    match l with
    | [] -> false
    | h :: t -> h = n || contains t n
  in
  match l with
  | [] -> []
  | h :: t ->
    let acc = remove_duplicates t in
    if contains acc h then acc else h :: acc
;;