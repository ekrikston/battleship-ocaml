open Board
open Opponent
open Random

(* Documented in game.mli as a "public" function. *)
type t = { player_board : Board.t; computer_board : Board.t }

(* Documented in game.mli as a "public" function. *)
let empty = { player_board = Board.empty; computer_board = Board.empty }

(** [verify_board_number input] is true when input is a string representing an
    int between 0 and 14 (inclusive) and is otherwise false. Example:
    [verify_board_number "16"] evaluates to false. *)
let verify_board_number (input : string) : bool =
  try
    if int_of_string input >= 0 && int_of_string input < 15 then true else false
  with _ -> false

(* Documented in game.mli as a "public" function. *)
let init_state (board_number : string) : t =
  if verify_board_number board_number then
    {
      player_board =
        Board.init_board
          [
            ("Carrier", 5); ("Battleship", 4); ("Cruiser", 3); ("Destroyer", 2);
          ];
      computer_board = Opponent.choose_board (int_of_string board_number);
    }
  else
    raise
      (Failure
         "Please choose a valid board by entering a number between 0 and 14.")

(* Documented in game.mli as a "public" function. *)
let add_player_boat (input : string) (state : t) : t =
  {
    player_board = Board.add_boat input state.player_board;
    computer_board = state.computer_board;
  }

(* Documented in game.mli as a "public" function. *)
let get_player_board (state : t) : string array array =
  match state with
  | { player_board = player_board'; computer_board = computer_board' } ->
      Board.get_board player_board'

(* Documented in game.mli as a "public" function. *)
let get_computer_board (state : t) : string array array =
  match state with
  | { player_board = player_board'; computer_board = computer_board' } ->
      Board.get_board computer_board'

let player_view_list (row : string array) : string list =
  Array.fold_left
    (fun acc x ->
      acc @ [ (if not (x = "H" || x = "M" || x = "0") then "0" else x) ])
    [] row

(* Documented in game.mli as a "public" function. *)
let get_computer_board_player_view (state : t) : string list list =
  let computer_board_saa = get_computer_board state in
  Array.fold_left
    (fun acc x -> acc @ [ player_view_list x ])
    [] computer_board_saa

(* Documented in game.mli as a "public" function. *)
let get_num_computer_sunk (state : t) : int =
  Board.get_num_sunk state.computer_board

(* Documented in game.mli as a "public" function. *)
let get_num_player_sunk (state : t) : int =
  Board.get_num_sunk state.player_board
  [@@coverage off]

(* Documented in game.mli as a "public" function. *)
let get_player_boats (state : t) : string list =
  Board.get_boats state.player_board

(* Documented in game.mli as a "public" function. *)
let get_computer_boats (state : t) : string list =
  Board.get_boats state.computer_board

(* Documented in game.mli as a "public" function. *)
let check_computer_sunk (state : t) : string =
  Board.check_if_boat_sunk state.computer_board

(* Documented in game.mli as a "public" function. *)
let check_player_sunk (state : t) : string =
  Board.check_if_boat_sunk state.player_board
  [@@coverage off]

(* Documented in game.mli as a "public" function. *)
let check_for_computer_hit (input : string) (state : t) : bool =
  Board.check_for_hit input.[0]
    (int_of_string (String.make 1 input.[1]))
    state.player_board

(* Documented in game.mli as a "public" function. *)
let check_for_player_hit (input : string) (state : t) : bool =
  Board.check_for_hit input.[0]
    (int_of_string (String.make 1 input.[1]))
    state.computer_board
  [@@coverage off]

(* Documented in game.mli as a "public" function. *)
let guess_against_computer (input : string) (state : t) : t =
  {
    player_board = state.player_board;
    computer_board = Board.guess_coordinate input state.computer_board;
  }

(* Documented in game.mli as a "public" function. *)
let computer_turn_to_guess (state : t) : t * string =
  let guess = Board.make_random_guess state.player_board in
  match guess with
  | row', col' ->
      ANSITerminal.print_string
        [ ANSITerminal.blue; ANSITerminal.Bold ]
        ("\nThe computer has made the guess: "
        ^ (String.make 1 row' ^ string_of_int col')
        ^ "!\n\n");
      let new_pb =
        Board.guess_coordinate
          (String.make 1 row' ^ string_of_int col')
          state.player_board
      in
      ( { player_board = new_pb; computer_board = state.computer_board },
        String.make 1 row' ^ string_of_int col' )
  [@@coverage off]

(* Documented in game.mli as a "public" function. *)
let player_sink (boat : string) (state : t) : t =
  {
    player_board = Board.sink boat state.player_board;
    computer_board = state.computer_board;
  }
  [@@coverage off]

(* Documented in game.mli as a "public" function. *)
let computer_sink (boat : string) (state : t) : t =
  {
    player_board = state.player_board;
    computer_board = Board.sink boat state.computer_board;
  }

(* Documented in game.mli as a "public" function. *)
let print_player_board (state : t) : unit = Board.print_board state.player_board
  [@@coverage off]

(* Documented in game.mli as a "public" function. *)
let print_computer_board (state : t) : unit =
  Board.print_hits_and_misses state.computer_board
  [@@coverage off]