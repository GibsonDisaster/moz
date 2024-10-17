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

let rec step_range start e step =
  if start >= e then [ start ] else [ start ] @ step_range (start + step) e step
;;

let make_coords start stop step =
  List.map
    (fun x -> List.map (fun y -> x, y) (step_range start stop step))
    (step_range start stop step)
  |> List.concat
;;