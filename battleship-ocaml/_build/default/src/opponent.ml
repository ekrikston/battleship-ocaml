open Board

let boat_list =
  [ ("Carrier", 5); ("Battleship", 4); ("Cruiser", 3); ("Destroyer", 2) ]

(* Opponent board 0. *)
let board0 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier A 1 V"
  |> Board.add_boat "Battleship A 2 V"
  |> Board.add_boat "Cruiser A 3 V"
  |> Board.add_boat "Destroyer A 4 V"

(* Opponent board 1. *)
let board1 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier E 0 H"
  |> Board.add_boat "Battleship F 1 H"
  |> Board.add_boat "Cruiser G 2 H"
  |> Board.add_boat "Destroyer H 3 H"

(* Opponent board 2. *)
let board2 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier A 0 V"
  |> Board.add_boat "Battleship B 1 H"
  |> Board.add_boat "Cruiser C 2 H"
  |> Board.add_boat "Destroyer D 3 H"

(* Opponent board 3. *)
let board3 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier A 0 H"
  |> Board.add_boat "Battleship H 3 H"
  |> Board.add_boat "Cruiser B 7 V"
  |> Board.add_boat "Destroyer A 6 H"

(* Opponent board 4. *)
let board4 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier C 2 H"
  |> Board.add_boat "Battleship A 7 V"
  |> Board.add_boat "Cruiser D 1 V"
  |> Board.add_boat "Destroyer H 1 H"

(* Opponent board 5. *)
let board5 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier A 0 H"
  |> Board.add_boat "Battleship B 0 H"
  |> Board.add_boat "Cruiser C 0 H"
  |> Board.add_boat "Destroyer D 0 H"

(* Opponenent board 6. *)
let board6 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier B 0 V"
  |> Board.add_boat "Battleship B 3 V"
  |> Board.add_boat "Cruiser B 5 V"
  |> Board.add_boat "Destroyer B 7 V"

(* Opponent board 7. *)
let board7 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier F 0 H"
  |> Board.add_boat "Battleship H 3 H"
  |> Board.add_boat "Cruiser A 0 V"
  |> Board.add_boat "Destroyer B 6 V"

(* Opponent board 8. *)
let board8 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier B 1 V"
  |> Board.add_boat "Battleship H 0 H"
  |> Board.add_boat "Cruiser C 4 H"
  |> Board.add_boat "Destroyer D 3 H"

(* Opponent board 9. *)
let board9 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier B 3 H"
  |> Board.add_boat "Battleship D 2 V"
  |> Board.add_boat "Cruiser E 4 V"
  |> Board.add_boat "Destroyer A 6 H"

(* Opponent board 10. *)
let board10 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier A 0 H"
  |> Board.add_boat "Battleship H 3 H"
  |> Board.add_boat "Cruiser E 4 H"
  |> Board.add_boat "Destroyer A 6 H"

(* Opponent board 11. *)
let board11 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier A 2 H"
  |> Board.add_boat "Battleship D 3 H"
  |> Board.add_boat "Cruiser C 0 V"
  |> Board.add_boat "Destroyer H 0 H"

(* Opponent board 12. *)
let board12 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier B 6 V"
  |> Board.add_boat "Battleship C 0 H"
  |> Board.add_boat "Cruiser B 7 V"
  |> Board.add_boat "Destroyer H 2 H"

(* Opponent board 13. *)
let board13 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier A 0 H"
  |> Board.add_boat "Battleship H 3 H"
  |> Board.add_boat "Cruiser A 7 V"
  |> Board.add_boat "Destroyer D 2 H"

(* Opponent board 14. *)
let board14 =
  Board.init_board boat_list
  |> Board.add_boat "Carrier C 0 V"
  |> Board.add_boat "Battleship H 3 H"
  |> Board.add_boat "Cruiser B 7 V"
  |> Board.add_boat "Destroyer A 6 H"

(* Documented in opponent.mli as a "public" function. *)
let choose_board (i : int) : Board.t =
  match i with
  | 0 -> board0
  | 1 -> board1
  | 2 -> board2
  | 3 -> board3
  | 4 -> board4
  | 5 -> board5
  | 6 -> board6
  | 7 -> board7
  | 8 -> board8
  | 9 -> board9
  | 10 -> board10
  | 11 -> board11
  | 12 -> board12
  | 13 -> board13
  | 14 -> board14
  | _ -> raise (Failure "no existing board")
