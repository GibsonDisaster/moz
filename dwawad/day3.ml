open Core
module CharSet = Set.Make (Char)

let elem lst ch = List.exists ~f:(fun x -> Char.equal x ch) lst

let cut_string s =
  let chars = List.init (String.length s) ~f:(String.get s) in
  let len = String.length s / 2 in
  List.take chars len, List.drop chars len
;;

let foo (c1, c2) = CharSet.of_list c1, CharSet.of_list c2

let get_inter (c1, c2) =
  Set.to_list (Set.inter (CharSet.of_list c1) (CharSet.of_list c2))
;;

let get_letter_score ch =
  match ch with
  | ch when Char.is_lowercase ch -> Char.to_int ch - 96
  | ch when Char.is_uppercase ch -> Char.to_int ch - 38
  | _ -> 0
;;

let file_contents_a =
  let lines = In_channel.read_all "inputs/input3.txt" |> String.split_lines in
  let cut_lines = List.map lines ~f:cut_string in
  cut_lines
;;

let solution_a =
  let cut_lines = file_contents_a in
  let dups = List.map cut_lines ~f:get_inter |> List.concat in
  List.map dups ~f:get_letter_score |> List.fold_left ~init:0 ~f:( + )
;;

(*
   let file_contents_b =
  let lines = In_channel.read_all "inputs/input3.txt" |> String.split_lines in
  let grouped = List.groupi ~break:(fun i _ _ -> i mod 3 = 0) lines in
  let intersections =
    List.map grouped ~f:(fun (x :: y :: z :: _) ->
      get_inter (String.to_list x, String.to_list y)
      @ get_inter (String.to_list y, String.to_list z)
      @ get_inter (String.to_list x, String.to_list z))
  in
  let sorted =
    List.map intersections ~f:(fun x -> List.sort x ~compare:Char.compare)
  in
  let dups =
    List.map sorted ~f:(fun x -> List.find_all_dups x ~compare:Char.compare)
  in
  List.map dups ~f:(fun (x :: _) -> get_letter_score x)
;;
*)

(* let solution_b = List.fold_left ~init:0 ~f:( + ) file_contents_b *)
