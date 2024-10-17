open Batteries

(*     [D]     *)
(* [N] [C]     *)
(* [Z] [M] [P] *)
(*  1   2   3  *)

(* LAST IN FIRST OUT

   NOT!
   FIRST IN FIRST OUT
   you fucking moron
   val stacks : char list list *)

(* use this to change a sublist in stacks *)
(* stacks := [1] :: List.tl !stacks *)
(* too tired to fix or explain, above code does not work *)
(* but should be enough to figure out how to do it *)

let _stacks =
  BatHashtbl.of_list
    [ 1, String.to_list "nz"
    ; 2, String.to_list "dcm"
    ; 3, String.to_list "p"
    ; 4, String.to_list ""
    ; 5, String.to_list ""
    ; 6, String.to_list ""
    ; 7, String.to_list ""
    ; 8, String.to_list ""
    ; 9, String.to_list ""
    ]
;;

let stacks =
  BatHashtbl.of_list
    [ 1, String.to_list "zpbqmdn"
    ; 2, String.to_list "vhdmqzlc"
    ; 3, String.to_list "gzfvdrhq"
    ; 4, String.to_list "nfdgh"
    ; 5, String.to_list "qfn"
    ; 6, String.to_list "tbfzvqd"
    ; 7, String.to_list "hsvdztmq"
    ; 8, String.to_list "qnpfgm"
    ; 9, String.to_list "mrwb"
    ]
;;

let ( << ) f g x = f (g x)

let rec make_triplets = function
  | [] -> []
  | x :: xs -> [ List.at x 0, List.at x 1, List.at x 2 ] @ make_triplets xs
;;

let rec parse_line line =
  match line with
  | [] -> []
  | x :: _ ->
    if Char.is_digit x
    then (
      let num = List.take_while Char.is_digit line in
      [ String.to_int (String.of_list num) ]
      @ parse_line (List.drop_while Char.is_digit line))
    else parse_line (List.drop_while (not << Char.is_digit) line)
;;

(* [ [ x; y; z; ] ... ] *)
let parsed_input =
  let file_contents =
    File.lines_of "inputs/input5.txt"
    |> BatList.of_enum
    |> fun x -> List.map String.to_list x
  in
  List.map parse_line file_contents |> make_triplets
;;

let move_x_to_y x y =
  let src_list = BatHashtbl.find stacks x in
  let dest_list = BatHashtbl.find stacks y in
  BatHashtbl.remove stacks x;
  BatHashtbl.add stacks x (List.tl src_list);
  BatHashtbl.remove stacks y;
  BatHashtbl.add stacks y ([ List.hd src_list ] @ dest_list)
;;

let move_whole (x, y, z) =
  let src_list = BatHashtbl.find stacks y in
  let dest_list = BatHashtbl.find stacks z in
  BatHashtbl.remove stacks y;
  BatHashtbl.add stacks y (List.drop x src_list);
  BatHashtbl.remove stacks z;
  BatHashtbl.add stacks z (List.take x src_list @ dest_list)
;;

let rec run_instr = function
  | 0, y, z -> move_x_to_y y z
  | x, y, z ->
    move_x_to_y y z;
    run_instr (x - 1, y, z)
;;

let run_solution_a () = Seq.iter run_instr (Seq.of_list parsed_input)

let solution_a () =
  let result = BatHashtbl.to_list stacks in
  List.filter_map
    (fun (x, y) -> if List.is_empty y then None else Some (x, List.hd y))
    result
;;

let run_solution_b () = Seq.iter move_whole (Seq.of_list parsed_input)

let solution_b () =
  let result = BatHashtbl.to_list stacks in
  List.filter_map
    (fun (x, y) -> if List.is_empty y then None else Some (x, List.hd y))
    result
;;
