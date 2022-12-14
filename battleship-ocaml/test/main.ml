open OUnit2
open Battleship
open Game
open Board
(*******************************************************************************
   TEST PLAN
 ******************************************************************************)

(** The primary testing methodology our team abided by is achieving extensive
    statement coverage (a glass-box testing methodology). We used the tool
    Bisect to see how extensive our statement coverage was. We have written 71
    test cases. With the nature of our project, though we only wrote 71 test
    cases, almost every function is innately interdependent on at least 1 other
    function, subsequently meaning we tested the functionality of several
    functions all at once with a single test case. *)

(** For our Board compilation unit (board.ml), we achieved 91% code coverage.
    After having analyzed the 8% of code that had not been tested by our OUnit
    test cases, we have seen that the code that hadn't been tested was, for the
    most part, branches of pattern match statements that should not have been
    reached. In order to resolve warnings of inexhaustive pattern matches, we
    added "catch-all" branches that should never be reached. For example, there
    were a few pattern matches where we matched against the
    string-representation of a placement orientation. An orientation, in our
    game, can either be "H" or "V." Although the functions we used this pattern
    matching for required that the orientation be a valid orientation (so an "H"
    or "V"), we placed catch-all branches at the end of the pattern matching
    function to resolve the inexhaustive pattern match warning. Because of that,
    it was impossible for us to test these lines of code. *)

(** For our Board compilation unit (board.ml), it is also important we
    acknowledge that we turned Bisect coverage off for a few functions. The
    functions we turned off coverage for were functions had a return type of
    unit, or functions that printed the board. We also turned off coverage for a
    function that made a random guess on the computer's behalf as there is no
    way for us to test what that result could have been. *)

(** For our Game compilation unit (game.ml), we achieved 100% code coverage. *)

(** For our Game compilation unit (game.ml), it is also important we acknowledge
    that we turned Bisect coverage off for a few functions. The functions we
    turned off coverage for were functions had a return type of unit, or
    functions that printed the board. We also turned off coverage for any
    functions that facilitated the guessing of the computer against the player
    as computerized guesses are dependent on the randomized function, which
    subsequently means we were unable to test these functions as we cannot
    anticipate what the randomized guess would have been. *)

(** Because there were functions, both within the Game compilation unit
    (game.ml) and Board compilation unit (board.ml) that we turned off coverage
    for, we manually tested those that were not able to be tested. For any
    functions that resulted in printing, we manually checked on the terminal to
    make sure that the functions were printing the right boards. For any
    functions that depended on randomized coverage, we were able to guarantee
    that the functions worked appropriately by playing the game fully-through
    several times to ensure that the computerized guesses were not resulting in
    unforseen errors (such as guessing the same coordinate multiple times). *)

(** We believe that, between manual and OUnit testing, we have been able to, as
    closely as possible, demonstrate correctness of our system with our
    glass-box, statement coverage methodology. Firstly, our manual testing was
    able to show us that any functions missed by our OUnit tests were working as
    expected (in accordance with their respective specifications). Our manual
    testing included playing the game fully through for all 15 computer boards
    multiple times through to ensure that the right error messages appeared for
    each kind of wrong input, to ensure the game ended when it should, to ensure
    that repeat guesses could not be made (by the player or the computer), etc.
    Our OUnit test cases were also able to test any functions not dependent on
    randomization or functions that had a return type of unit. Because of this,
    our OUnit test cases were able to show us that any errors that should be
    handled are being handled appropriately and any functionality for correct
    inputs is also being handled correctly. *)

(** We also want to note that we did not track coverage of the Opponent
    compilation unit. The compilation unit simply contains 15 hard-coded
    opponent boards the player can choose from and play against. We manually
    tested each of the boards to ensure that there were no errors with their
    initialization and did not find it necessary to have OUnit tests to test
    every board. However, we did reach around 90% coverage. *)

(*******************************************************************************
   Test case generators for Game compilation unit. 
 ******************************************************************************)

(** [init_state_exception_test name num exp_outp] constructs an OUnit test named
    [name] that asserts the quality of [exp_outp] with [Game.init_state num]. *)
let init_state_exception_test (name : string) (num : string) (exp_outp : exn) :
    test =
  name >:: fun _ -> assert_raises exp_outp (fun () -> Game.init_state num)

(** [add_player_boat_test name state boat exp_outp] constructs an OUnit test
    named [name] that asserts the quality of [exp_outp] with
    [Game.add_player_boat state input]. *)
let add_player_boat_test (name : string) (state : Game.t) (input : string)
    (exp_outp : Game.t) : test =
  name >:: fun _ -> assert_equal exp_outp (Game.add_player_boat input state)

(** [add_player_boat_exception_test name board input exp_outp] constructs an
    OUnit test named [name] that asserts the quality of [exp_outp] with
    [Game.add_player_boat state input]. *)
let add_player_boat_exception_test (name : string) (board : Game.t)
    (input : string) (exp_outp : exn) : test =
  name >:: fun _ ->
  assert_raises exp_outp (fun () -> Game.add_player_boat input board)

(** [get_player_board_test name state exp_outp] constructs an OUnit test named
    [name] that asserts the quality of [exp_outp] with
    [Game.get_player_board state]. *)
let get_player_board_test (name : string) (state : Game.t)
    (exp_outp : string array array) : test =
  name >:: fun _ -> assert_equal exp_outp (Game.get_player_board state)

(** [get_computer_board_test name state exp_outp] constructs an OUnit test named
    [name] that asserts the quality of [exp_outp] with
    [Game.get_computer_board state]. *)
let get_computer_board_test (name : string) (state : Game.t)
    (exp_outp : string array array) : test =
  name >:: fun _ -> assert_equal exp_outp (Game.get_computer_board state)

(** [get_computer_board_player_view_test name state exp_outp] constructs an
    OUnit test named [name] that asserts the quality of [exp_outp] with
    [Game.get_computer_board_player_view state]. *)
let get_computer_board_player_view_test (name : string) (state : Game.t)
    (exp_outp : string list list) : test =
  name >:: fun _ ->
  assert_equal exp_outp (Game.get_computer_board_player_view state)

(** [get_num_computer_sunk_test name state exp_outp] constructs an OUnit test
    named [name] that asserts the quality of [exp_outp] with
    [Game.get_num_computer_sunk state]. *)
let get_num_computer_sunk_test (name : string) (state : Game.t) (exp_outp : int)
    : test =
  name >:: fun _ -> assert_equal exp_outp (Game.get_num_computer_sunk state)

(** [get_player_boats_test name state exp_outp] constructs an OUnit test named
    [name] that asserts the quality of [exp_outp] with
    [Game.get_player_boats state]. *)
let get_player_boats_test (name : string) (state : Game.t)
    (exp_outp : string list) : test =
  name >:: fun _ -> assert_equal exp_outp (Game.get_player_boats state)

(** [get_computer_boats_test name state exp_outp] constructs an OUnit test named
    [name] that asserts the quality of [exp_outp] with
    [Game.get_computer_boats state]. *)
let get_computer_boats_test (name : string) (state : Game.t)
    (exp_outp : string list) : test =
  name >:: fun _ -> assert_equal exp_outp (Game.get_computer_boats state)

(** [check_computer_sunk_test name state exp_outp] constructs an OUnit test
    named [name] that asserts the quality of [exp_outp] with
    [Game.check_computer_sunk state]. *)
let check_computer_sunk_test (name : string) (state : Game.t)
    (exp_outp : string) : test =
  name >:: fun _ -> assert_equal exp_outp (Game.check_computer_sunk state)

(** [check_for_computer_hit_test name input state exp_outp] constructs an OUnit
    test named [name] that asserts the quality of [exp_outp] with
    [Game.check_for_computer_hit input state]. *)
let check_for_computer_hit_test (name : string) (input : string)
    (state : Game.t) (exp_outp : bool) : test =
  name >:: fun _ ->
  assert_equal exp_outp (Game.check_for_computer_hit input state)

(** [guess_against_computer_test name state input exp_outp] constructs an OUnit
    test named [name] that asserts the quality of [exp_outp] with
    [Game.guess_against_computer state input]. *)
let guess_against_computer_test (name : string) (state : Game.t)
    (input : string) (exp_outp : Game.t) : test =
  name >:: fun _ ->
  assert_equal exp_outp (Game.guess_against_computer input state)

(** [guess_against_computer_exception_test name state input exp_outp] constructs
    an OUnit test named [name] that asserts the quality of [exp_outp] with
    [Game.guess_against_computer state input]. *)
let guess_against_computer_exception_test (name : string) (state : Game.t)
    (input : string) (exp_outp : exn) : test =
  name >:: fun _ ->
  assert_raises exp_outp (fun () -> Game.guess_against_computer input state)

(** [computer_sink_test name state boat exp_outp] constructs an OUnit test named
    [name] that asserts the quality of [exp_outp] with
    [Game.computer_sink state boat]. *)
let computer_sink_test (name : string) (state : Game.t) (boat : string)
    (exp_outp : Game.t) : test =
  name >:: fun _ -> assert_equal exp_outp (Game.computer_sink boat state)

(*******************************************************************************
    Test case generators for Board compilation unit.
  ******************************************************************************)

(** [is_blank_test name board exp_outp] constructs an OUnit test named [name]
    that asserts the quality of [exp_outp] with [Board.is_blank board]. *)
let is_blank_test (name : string) (board : Board.t) (exp_outp : bool) : test =
  name >:: fun _ -> assert_equal exp_outp (Board.is_blank board)

(** [get_board_test name board exp_outp] constructs an OUnit test named [name]
    that asserts the quality of [exp_outp] with [Board.get_board board]. *)
let get_board_test (name : string) (board : Board.t)
    (exp_outp : string array array) : test =
  name >:: fun _ -> assert_equal exp_outp (Board.get_board board)

(** [get_boats_test name board exp_outp] constructs an OUnit test named [name]
    that asserts the quality of [exp_outp] with [Board.get_boats board]. *)
let get_boats_test (name : string) (board : Board.t) (exp_outp : string list) :
    test =
  name >:: fun _ -> assert_equal exp_outp (Board.get_boats board)

(** [get_num_sunk name board exp_outp] constructs an OUnit test named [name]
    that asserts the quality of [exp_outp] with [Board.get_num_sunk board]. *)
let get_num_sunk_test (name : string) (board : Board.t) (exp_outp : int) : test
    =
  name >:: fun _ -> assert_equal exp_outp (Board.get_num_sunk board)

(** [add_boat_test name board input exp_outp] constructs an OUnit test named
    [name] that asserts the quality of [exp_outp] with
    [Board.add_boat board input]. *)
let add_boat_test (name : string) (board : Board.t) (input : string)
    (exp_outp : Board.t) : test =
  name >:: fun _ -> assert_equal exp_outp (Board.add_boat input board)

(** [add_boat_exception_test name board input exp_outp] constructs an OUnit test
    named [name] that asserts the quality of [exp_outp] with
    [Board.add_boat board input]. *)
let add_boat_exception_test (name : string) (board : Board.t) (input : string)
    (exp_outp : exn) : test =
  name >:: fun _ ->
  assert_raises exp_outp (fun () -> Board.add_boat input board)

(** [guess_coordinate_test name board input exp_outp] constructs an OUnit test
    named [name] that asserts the quality of [exp_outp] with
    [Board.guess_coordinate board input]. *)
let guess_coordinate_test (name : string) (board : Board.t) (input : string)
    (exp_outp : Board.t) : test =
  name >:: fun _ -> assert_equal exp_outp (Board.guess_coordinate input board)

(** [guess_coordinate_exception_test name board input exp_outp] constructs an
    OUnit test named [name] that asserts the quality of [exp_outp] with
    [Board.gues_coordinate board input]. *)
let guess_coordinate_exception_test (name : string) (board : Board.t)
    (input : string) (exp_outp : exn) : test =
  name >:: fun _ ->
  assert_raises exp_outp (fun () -> Board.guess_coordinate input board)

(** [check_if_boat_sunk_test name board exp_outp] constructs an OUnit test named
    [name] that asserts the quality of [exp_outp] with
    [Board.check_if_boat_sunk board]. *)
let check_if_boat_sunk_test (name : string) (board : Board.t)
    (exp_outp : string) : test =
  name >:: fun _ -> assert_equal exp_outp (Board.check_if_boat_sunk board)

(** [check_for_hit_test name row col board exp_outp] constructs an OUnit test
    named [name] that asserts the quality of [exp_outp] with
    [Board.check_for_hit row col board]. *)
let check_for_hit_test (name : string) (row : char) (col : int)
    (board : Board.t) (exp_outp : bool) : test =
  name >:: fun _ -> assert_equal exp_outp (Board.check_for_hit row col board)

(** [sink_test name board boat exp_outp] constructs an OUnit test named [name]
    that asserts the quality of [exp_outp] with [Board.sink board]. *)
let sink_test (name : string) (board : Board.t) (boat : string)
    (exp_outp : Board.t) : test =
  name >:: fun _ -> assert_equal exp_outp (Board.sink boat board)

(*******************************************************************************
   Boards to run tests on Board compilation unit.
 ******************************************************************************)
let boats_list =
  [ ("Carrier", 5); ("Battleship", 4); ("Cruiser", 3); ("Destroyer", 2) ]

(* board0 = empty board. *)
let board0 = Board.init_board boats_list

(* board_saa = board0 string array array representation. *)

let board0_saa =
  [|
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
  |]

(* board0_boats = board0 list of boats. *)
let board0_boats = []

(* board1 = board with one horizontally-placed boat. *)
let board1 = Board.add_boat "Cruiser A 0 H" (Board.init_board boats_list)

(* board1_saa = board1 string array array representation. *)

let board1_saa =
  [|
    [| "3"; "3"; "3"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
  |]

(* board1_boats = board1 list of boats. *)
let board1_boats = [ "Cruiser" ]

(* board2 = board with all boats placed—some horizontal, some vertical. *)
let board2 =
  Board.init_board boats_list
  |> Board.add_boat "Cruiser A 0 H"
  |> Board.add_boat "Carrier B 7 V"
  |> Board.add_boat "Destroyer C 1 H"
  |> Board.add_boat "Battleship B 5 V"

(* board2_saa = board2 string array array representation. *)
let board2_saa =
  [|
    [| "3"; "3"; "3"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "2"; "2"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
  |]

(* board2_boats = board2 list of boats. *)
let board2_boats = [ "Cruiser"; "Carrier"; "Destroyer"; "Battleship" ]

(* board3 = fully set up board with 1 sunk boat. *)
let board3 =
  Board.init_board boats_list
  |> Board.add_boat "Cruiser A 0 H"
  |> Board.add_boat "Carrier B 7 V"
  |> Board.add_boat "Destroyer C 1 H"
  |> Board.add_boat "Battleship B 5 V"
  |> Board.guess_coordinate "A0"
  |> Board.guess_coordinate "A1"
  |> Board.guess_coordinate "A2"
  |> Board.sink "Cruiser"

(* board3_saa = board3 string array array representation. *)

let board3_saa =
  [|
    [| "H"; "H"; "H"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "2"; "2"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
  |]

(* board3_boats = board3 list of boats. *)
let board3_boats = [ "Cruiser"; "Carrier"; "Destroyer"; "Battleship" ]

(* board4 = board2 after a single guess has been made. *)
let board4 =
  Board.init_board boats_list
  |> Board.add_boat "Cruiser A 0 H"
  |> Board.add_boat "Carrier B 7 V"
  |> Board.add_boat "Destroyer C 1 H"
  |> Board.add_boat "Battleship B 5 V"
  |> Board.guess_coordinate "A3"

(* board4_boats = board4 list of boats. *)
let board4_boats = [ "Cruiser"; "Carrier"; "Destroyer"; "Battleship" ]

(* board5 = board3 before the boat has officially been sunk. *)
let board5 =
  Board.init_board boats_list
  |> Board.add_boat "Cruiser A 0 H"
  |> Board.add_boat "Carrier B 7 V"
  |> Board.add_boat "Destroyer C 1 H"
  |> Board.add_boat "Battleship B 5 V"
  |> Board.guess_coordinate "A0"
  |> Board.guess_coordinate "A1"
  |> Board.guess_coordinate "A2"

(* board6 *)
let board6 =
  Board.init_board boats_list
  |> Board.add_boat "Cruiser A 5 V"
  |> Board.add_boat "Carrier A 0 H"
  |> Board.add_boat "Destroyer H 0 H"
  |> Board.add_boat "Battleship D 0 V"

(* board6_boats = list of boats on board6 *)
let board6_boats = [ "Cruiser"; "Carrier"; "Destroyer"; "Battleship" ]

(*******************************************************************************
   States to run tests on Game compilation unit.
 ******************************************************************************)

(* state0 = empty state or empty player board and opponent board 3. *)
let state0 = Game.init_state "5"

(* state0_computer_saa = string array array representation of state0 player board. *)
let state0_computer_saa =
  [|
    [| "5"; "5"; "5"; "5"; "5"; "0"; "0"; "0" |];
    [| "4"; "4"; "4"; "4"; "0"; "0"; "0"; "0" |];
    [| "3"; "3"; "3"; "0"; "0"; "0"; "0"; "0" |];
    [| "2"; "2"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
  |]

(* state0_player_boats = list of boats placed on state0 player board. *)
let state0_player_boats = []

(* state0_computer_boats = list of boats placed on state0 computer board. *)
let state0_computer_boats = [ "Carrier"; "Battleship"; "Cruiser"; "Destroyer" ]

(* state1 = one player boat added to empty state. *)
let state1 = Game.init_state "5" |> Game.add_player_boat "Destroyer A 0 H"

(* state1_player_saa = string array array representation of state1 player board. *)
let state1_player_saa =
  [|
    [| "2"; "2"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
  |]

(* state2 = state with all player boats placed—some horizontal, some vertical. *)
let state2 =
  Game.init_state "4"
  |> Game.add_player_boat "Cruiser A 0 H"
  |> Game.add_player_boat "Carrier B 7 V"
  |> Game.add_player_boat "Destroyer C 1 H"
  |> Game.add_player_boat "Battleship B 5 V"

(* board2_player_saa = board2 string array array representation. *)
let state2_player_saa =
  [|
    [| "3"; "3"; "3"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "2"; "2"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "4"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "5" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
    [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
  |]

(* state2_comp_board_player_sll = board2 player view of comp board in state2. *)
let state2_comp_board_player_sll =
  [
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
  ]

(* state3 = opponent board 3 with a boat about to be, but not yet registered as, sunk. *)
let state3 =
  Game.init_state "3"
  |> Game.guess_against_computer "A0"
  |> Game.guess_against_computer "A1"
  |> Game.guess_against_computer "A2"

(* state3_comp_board_player_sll = state3 player view of comp board in state3. *)
let state3_comp_board_player_sll =
  [
    [ "H"; "H"; "H"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
  ]

(* state4 = opponent board 3 with a boat about to be, but not yet registered as, sunk. *)
let state4 =
  Game.init_state "3"
  |> Game.guess_against_computer "A0"
  |> Game.guess_against_computer "A1"
  |> Game.guess_against_computer "A2"
  |> Game.computer_sink "Cruiser"

(* state5 = opponent board 2 with a boat about to be, but not yet registered as, sunk. *)
let state5 =
  Game.init_state "2"
  |> Game.guess_against_computer "D3"
  |> Game.guess_against_computer "D4"

(* state6 = opponent board 7 with one guess made against computer. *)
let state6 = Game.init_state "7" |> Game.guess_against_computer "D3"

(* state7 = opponent board 7 with two guesses made against computer. *)
let state7 =
  Game.init_state "7"
  |> Game.guess_against_computer "D3"
  |> Game.guess_against_computer "E5"

(* state8 = opponent board with missed guesses on it. *)
let state8 = Game.init_state "9" |> Game.guess_against_computer "A0"

(* state8_comp_board_player_sll = state3 player view of comp board in state3. *)
let state8_comp_board_player_sll =
  [
    [ "M"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
    [ "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" ];
  ]

(*******************************************************************************
   Tests.
 ******************************************************************************)

let tests =
  "full test suite"
  >::: [
         is_blank_test "board0 is blank" board0 true;
         is_blank_test "board1 is not blank" board1 false;
         is_blank_test "board2 is not blank" board2 false;
         get_board_test "board0 string array array representation" board0
           board0_saa;
         get_board_test "board1 string array array representation" board1
           board1_saa;
         get_board_test "board2 string array array representation" board2
           board2_saa;
         get_board_test "board3 string array array representation" board3
           board3_saa;
         get_boats_test "board0 list of placed boats" board0 board0_boats;
         get_boats_test "board1 list of placed boats" board1 board1_boats;
         get_boats_test "board2 list of placed boats" board2 board2_boats;
         get_boats_test "board3 list of placed boats" board3 board3_boats;
         get_boats_test "board4 list of placed boats" board4 board4_boats;
         get_num_sunk_test "board0 has no sunk boats" board0 0;
         get_num_sunk_test "board2 has no sunk boats" board2 0;
         get_num_sunk_test "board3 has sunk boats" board3 1;
         add_boat_test "add cruiser to empty board" board0 "Cruiser A 0 H"
           board1;
         add_boat_exception_test
           "input for adding boat contains boat that doesn't exist" board0
           "Crsr A 0 H" (Failure "This boat does not exist!");
         add_boat_exception_test "input for adding boat contains no space"
           board0 "CruiserA0H"
           (Failure
              "Please try again. There are several issues with the placement \
               you have tried.");
         add_boat_exception_test
           "input for adding boat contains row that doesn't exist" board0
           "Cruiser X 0 H"
           (Failure
              "Invalid row. Please try again with a new row for your \
               coordinate. A valid row is an uppercase character between 'A' \
               and 'H'.");
         add_boat_exception_test
           "input for adding boat contains column that doesn't exist" board0
           "Cruiser A 14 H"
           (Failure
              "Invalid column. Please try again with a new column for your \
               coordinate. A valid column is a number between 0 and 7.");
         add_boat_exception_test
           "input for adding boat contains orientation that doesn't exist"
           board0 "Cruiser A 0 Horizontal"
           (Failure
              "Invalid orientation. Please try again with a new orientation \
               for your boat's placement. A valid orientation is an uppercase \
               character 'H' or 'V'.");
         add_boat_exception_test
           "input for adding boat overlaps with already existing boat" board1
           "Destroyer A 2 V"
           (Failure
              "Invalid coordinate. Please try again with a coordinate that \
               does not already contain a boat.");
         add_boat_exception_test
           "input for adding boat contains an already-placed boat" board1
           "Cruiser B 0 H"
           (Failure
              "Invalid boat. Please try again with a boat that is still \
               unplaced.");
         guess_coordinate_test "board2 after a single guess" board2 "A3" board4;
         guess_coordinate_exception_test
           "input for guessing coordinate contains a space" board2 "A 4"
           (Failure "Please try again with no spaces in your guess.");
         guess_coordinate_exception_test
           "input for guessing coordinate contains invalid row" board2 "J4"
           (Failure
              "Invalid row. Please try again with a new row for your \
               coordinate. A valid row is an uppercase character between 'A' \
               and 'H'.");
         guess_coordinate_exception_test
           "input for guessing coordinate starts with a space" board2 " A4"
           (Failure "Please try again with no spaces preceding your guess.");
         guess_coordinate_exception_test
           "input for guessing coordinate contains invalid column" board2 "A9"
           (Failure
              "Invalid column. Please try again with a new column for your \
               coordinate. A valid column is a number between 0 and 7.");
         guess_coordinate_exception_test
           "input for guessing coordinate is empty" board2 ""
           (Failure "It appears you haven't made a guess. Please guess again!");
         guess_coordinate_exception_test
           "input for guessing coordinate contains int larger than 10" board2
           "A10" (Failure "Please guess again.");
         check_if_boat_sunk_test "boat has just been sunk for board5" board5
           "Cruiser";
         check_if_boat_sunk_test "no boat has just been sunk for board0" board0
           "";
         check_if_boat_sunk_test "no boat has just been sunk for board4" board4
           "";
         check_for_hit_test "a boat will be hit for board4 with coordinate A0"
           'A' 0 board4 true;
         check_for_hit_test
           "a boat will not be hit for board4 with coordinate A6" 'A' 6 board4
           false;
         sink_test "board5 becomes board3 when cruiser is sunk" board5 "Cruiser"
           board3;
         add_player_boat_test "add destroyer to player board in state0" state0
           "Destroyer A 0 H" state1;
         add_player_boat_exception_test
           "input for adding boat contains boat that doesn't exist" state0
           "Crsr A 0 H" (Failure "This boat does not exist!");
         add_player_boat_exception_test
           "input for adding boat contains row that doesn't exist" state0
           "Cruiser X 0 H"
           (Failure
              "Invalid row. Please try again with a new row for your \
               coordinate. A valid row is an uppercase character between 'A' \
               and 'H'.");
         add_player_boat_exception_test
           "input for adding boat contains column that doesn't exist" state0
           "Cruiser A 14 H"
           (Failure
              "Invalid column. Please try again with a new column for your \
               coordinate. A valid column is a number between 0 and 7.");
         add_player_boat_exception_test
           "input for adding boat contains orientation that doesn't exist"
           state0 "Cruiser A 0 Horizontal"
           (Failure
              "Invalid orientation. Please try again with a new orientation \
               for your boat's placement. A valid orientation is an uppercase \
               character 'H' or 'V'.");
         add_player_boat_exception_test
           "input for adding boat contains integer for row and integer for \
            column"
           state0 "Cruiser 0 0 H"
           (Failure
              "Invalid row. Please try again with a new row for your \
               coordinate. A valid row is an uppercase character between 'A' \
               and 'H'.");
         add_player_boat_exception_test
           "input for adding boat incorrectly contains character for column"
           state0 "Cruiser A A H"
           (Failure
              "Invalid column. Please try again with a new column for your \
               coordinate. A valid column is a number between 0 and 7.");
         add_player_boat_exception_test
           "input for adding boat overlaps with already existing boat" state1
           "Battleship A 1 V"
           (Failure
              "Invalid coordinate. Please try again with a coordinate that \
               does not already contain a boat.");
         add_player_boat_exception_test
           "input for adding boat contains an already-placed boat" state1
           "Destroyer B 0 H"
           (Failure
              "Invalid boat. Please try again with a boat that is still \
               unplaced.");
         get_num_computer_sunk_test "state0 has no sunk boats" state0 0;
         guess_against_computer_exception_test
           "input for guessing coordinate contains a space" state2 "A 4"
           (Failure "Please try again with no spaces in your guess.");
         guess_against_computer_exception_test
           "input for guessing coordinate contains invalid row" state2 "J4"
           (Failure
              "Invalid row. Please try again with a new row for your \
               coordinate. A valid row is an uppercase character between 'A' \
               and 'H'.");
         guess_against_computer_exception_test
           "input for guessing coordinate starts with a space" state2 " A4"
           (Failure "Please try again with no spaces preceding your guess.");
         guess_against_computer_exception_test
           "input for guessing coordinate contains invalid column" state2 "A9"
           (Failure
              "Invalid column. Please try again with a new column for your \
               coordinate. A valid column is a number between 0 and 7.");
         guess_against_computer_exception_test
           "input for guessing coordinate is empty" state2 ""
           (Failure "It appears you haven't made a guess. Please guess again!");
         guess_against_computer_exception_test
           "input for guessing coordinate contains int larger than 10" state2
           "A10" (Failure "Please guess again.");
         get_player_board_test
           "board2 player board string array array representation" state2
           state2_player_saa;
         get_player_board_test
           "board1 player board string array array representation" state1
           state1_player_saa;
         get_computer_board_test
           "board0 computer board string array array representation" state0
           state0_computer_saa;
         get_num_computer_sunk_test "state3 has no sunk boats" state3 0;
         get_num_computer_sunk_test "state2 has no sunk boats" state2 0;
         get_num_computer_sunk_test "state4 has one sunk boat" state4 1;
         get_player_boats_test "state0 has no placed player boats" state0
           state0_player_boats;
         get_computer_boats_test "state0 has all placed computer boats" state0
           state0_computer_boats;
         check_for_computer_hit_test
           "coordinate B7 guessed against computer in state2 will result in a \n\
           \            hit" "B7" state2 true;
         check_for_computer_hit_test
           "coordinate A7 guessed against computer in state2 will not result \
            in a hit"
           "A7" state2 false;
         check_computer_sunk_test
           "state5 has a boat that will have sunk as of the last guess made \
            against the computer"
           state5 "Destroyer";
         init_state_exception_test "number greater than 14" "16"
           (Failure
              "Please choose a valid board by entering a number between 0 and \
               14.");
         init_state_exception_test "empty input for choosing board" ""
           (Failure
              "Please choose a valid board by entering a number between 0 and \
               14.");
         init_state_exception_test
           "input for choosing a board has a valid number followed by more \
            characters"
           "4a"
           (Failure
              "Please choose a valid board by entering a number between 0 and \
               14.");
         guess_against_computer_test
           "state7 is the state after state6 has had a guess made against the \
            computer"
           state6 "E5" state7;
         get_computer_board_player_view_test
           "state2 computer board in the player view with a string list list \
            representation"
           state2 state2_comp_board_player_sll;
         get_computer_board_player_view_test
           "state3 computer board in the player view with a string list list \
            representation"
           state3 state3_comp_board_player_sll;
         get_boats_test "board6 list of boats" board6 board6_boats;
         get_computer_board_player_view_test
           "state8 computer board in the player view with a string list list \
            representation"
           state8 state8_comp_board_player_sll;
       ]

let _ = run_test_tt_main tests
