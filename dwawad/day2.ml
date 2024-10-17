open Core

type move =
  | Rock
  | Paper
  | Scissor
  | Illegal

type player =
  | Elf
  | Henning
  | Tie
  | IllegalPlayer

type outcome =
  | Win
  | Lose
  | Draw
  | IllegalOutcome

let make_move = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissor
  | _ -> Illegal
;;

let move_score = function
  | Rock -> 1
  | Paper -> 2
  | Scissor -> 3
  | _ -> 0
;;

let winner = function
  | [ Rock; Scissor ] -> Rock, Scissor, Elf
  | [ Scissor; Paper ] -> Scissor, Paper, Elf
  | [ Paper; Rock ] -> Paper, Rock, Elf
  | [ Scissor; Rock ] -> Scissor, Rock, Henning
  | [ Paper; Scissor ] -> Paper, Scissor, Henning
  | [ Rock; Paper ] -> Rock, Paper, Henning
  | [ Rock; Rock ] -> Rock, Rock, Tie
  | [ Scissor; Scissor ] -> Scissor, Scissor, Tie
  | [ Paper; Paper ] -> Paper, Paper, Tie
  | _ -> Illegal, Illegal, IllegalPlayer
;;

let score_round = function
  | opp, hen, Elf -> move_score opp + 6, move_score hen
  | opp, hen, Henning -> move_score opp, 6 + move_score hen
  | opp, hen, Tie -> 3 + move_score opp, 3 + move_score hen
  | _, _, _ -> 0, 0
;;

let file_input = "inputs/input2.txt"

let rec player_score = function
  | [] -> 0
  | (_, x) :: rest -> x + player_score rest
;;

let parsed_input =
  let file_contents = In_channel.read_all file_input |> String.split_lines in
  let rounds =
    List.map file_contents ~f:(fun x -> String.split_on_chars x ~on:[ ' ' ])
  in
  let made_moves = List.map rounds ~f:(fun x -> List.map ~f:make_move x) in
  let round_winners = List.map made_moves ~f:winner in
  let round_scores = List.map round_winners ~f:score_round in
  round_scores
;;

let solution_a = player_score parsed_input

(* Solution B requires a bit of a rewrite *)

let make_outcome = function
  | "X" -> Lose
  | "Y" -> Draw
  | "Z" -> Win
  | _ -> IllegalOutcome
;;

let make_b_round = function
  | [ opp; hen ] -> make_move opp, make_outcome hen
  | _ -> Illegal, IllegalOutcome
;;

let get_move = function
  | opp, Draw -> 3 + move_score opp
  | opp, Win ->
    (match opp with
     | Rock -> 6 + move_score Paper
     | Scissor -> 6 + move_score Rock
     | Paper -> 6 + move_score Scissor
     | _ -> 0)
  | opp, Lose ->
    (match opp with
     | Paper -> move_score Rock
     | Rock -> move_score Scissor
     | Scissor -> move_score Paper
     | _ -> 0)
  | _ -> 0
;;

let b_input =
  let file_contents = In_channel.read_all file_input |> String.split_lines in
  let rounds =
    List.map file_contents ~f:(fun x -> String.split_on_chars x ~on:[ ' ' ])
  in
  let made_rounds = List.map rounds ~f:make_b_round in
  let results = List.map made_rounds ~f:get_move in
  List.fold_left results ~f:( + ) ~init:0
;;

let solution_b = b_input
