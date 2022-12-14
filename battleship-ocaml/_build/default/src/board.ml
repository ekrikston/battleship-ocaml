(* Type representing boats holding the name of a boat and the coordinates it
    occupies on the given board. *)
type boat = { name : string; coord_lst : (char * int) list }

(* Documented in board.mli as a "public" function. *)
type t = {
  board : string array array;
  boat_names : (string * int) list;
  boats : boat list;
  guessed_list : (char * int) list;
  unguessed_list : (char * int) list;
  sunk : string list;
}

(* Documented in board.mli as a "public" function. *)
let empty =
  {
    board =
      [|
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
      |];
    boat_names = [];
    boats = [];
    guessed_list = [];
    unguessed_list = [];
    sunk = [];
  }

(* Documented in board.mli as a "public" function. *)
let init_board (boats : (string * int) list) =
  {
    board =
      [|
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
        [| "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0" |];
      |];
    boat_names = boats;
    boats = [];
    guessed_list = [];
    unguessed_list =
      [
        ('A', 0);
        ('A', 1);
        ('A', 2);
        ('A', 3);
        ('A', 4);
        ('A', 5);
        ('A', 6);
        ('A', 7);
        ('B', 0);
        ('B', 1);
        ('B', 2);
        ('B', 3);
        ('B', 4);
        ('B', 5);
        ('B', 6);
        ('B', 7);
        ('C', 0);
        ('C', 1);
        ('C', 2);
        ('C', 3);
        ('C', 4);
        ('C', 5);
        ('C', 6);
        ('C', 7);
        ('D', 0);
        ('D', 1);
        ('D', 2);
        ('D', 3);
        ('D', 4);
        ('D', 5);
        ('D', 6);
        ('D', 7);
        ('E', 0);
        ('E', 1);
        ('E', 2);
        ('E', 3);
        ('E', 4);
        ('E', 5);
        ('E', 6);
        ('E', 7);
        ('F', 0);
        ('F', 1);
        ('F', 2);
        ('F', 3);
        ('F', 4);
        ('F', 5);
        ('F', 6);
        ('F', 7);
        ('G', 0);
        ('G', 1);
        ('G', 2);
        ('G', 3);
        ('G', 4);
        ('G', 5);
        ('G', 6);
        ('G', 7);
        ('H', 0);
        ('H', 1);
        ('H', 2);
        ('H', 3);
        ('H', 4);
        ('H', 5);
        ('H', 6);
        ('H', 7);
      ];
    sunk = [];
  }

(* Documented in board.mli as a "public" function. *)
let is_blank (board : t) : bool =
  if
    board
    = init_board
        [ ("Carrier", 5); ("Battleship", 4); ("Cruiser", 3); ("Destroyer", 2) ]
  then true
  else false

(* Documented in board.mli as a "public" function. *)
let get_board (board : t) : string array array =
  match board with
  | { board = board'; _ } -> board'

(** [get_boat_names boats acc] is a string list of the names of the boats given
    in a list of boats (of type boat). If a boat list is empty,
    [get_boat_names boats acc] returns acc. *)
let rec get_boat_names (boats : boat list) (acc : string list) : string list =
  match boats with
  | [] -> acc
  | { name; coord_lst } :: t -> get_boat_names t (name :: acc)

(* Documented in board.mli as a "public" function. *)
let get_boats (board : t) : string list =
  match board with
  | { board; boat_names; boats } -> get_boat_names boats []

(* Documented in board.mli as a "public" function. *)
let get_num_sunk (board : t) : int = List.length board.sunk

(** [not_placed name lst] is true when a boat named [name] does not occur in a
    given boat list [lst]. *)
let rec not_placed (name : string) (lst : boat list) : bool =
  match lst with
  | [] -> true
  | h :: t -> if h.name = name then false else not_placed name t

(** [valid_name placed_boats all_boat_names input] is true when [input] is a
    valid boat name (meaning it has not yet been placed on the board and exists
    in the list of all valid boat names) and is false otherwise. Example: false
    will be returned if a cruiser has already been placed on a board and [input]
    is "Cruiser." *)
let rec valid_name (placed_boats : boat list)
    (all_boat_names : (string * int) list) (input : string) : bool =
  match all_boat_names with
  | [] -> false
  | (boat, _) :: t ->
      if boat = input then if not_placed boat placed_boats then true else false
      else valid_name placed_boats t input

(** [valid_column col dir length] is true when adding a boat to column [col]
    with length [length] in the direction [dir] will not run off the board when
    placed and is otherwise false. Requires: dir is a valid direction, being
    either H or V. *)
let valid_column (col : string) (dir : string) (length : int) : bool =
  if
    not
      (col = "0" || col = "1" || col = "2" || col = "3" || col = "4"
     || col = "5" || col = "6" || col = "7")
  then
    raise
      (Failure
         "Invalid column. Please try again with a new column for your \
          coordinate. A valid column is a number between 0 and 7.")
  else
    match dir with
    | "H" ->
        let i = int_of_string col in
        if i + (length - 1) <= 7 && i >= 0 then true else false
    | "V" -> int_of_string col >= 0 && int_of_string col <= 7
    | _ ->
        raise
          (Failure
             "Invalid orientation. Please try again with a new orientation for \
              your boat's placement. A valid orientation is an uppercase \
              character 'H' or 'V'.")

(** [valid_row row dir length] is true when adding a boat to row [row] with
    length [length] in the direction [dir] will not run off the board when
    placed and is otherwise false. Requires: dir is a valid direction, being
    either H or V. *)
let valid_row (row : string) (dir : string) (length : int) : bool =
  match dir with
  | "H" ->
      if String.length row <> 1 then false
      else if row.[0] >= 'A' && row.[0] <= 'H' then true
      else false
  | "V" -> row.[0] >= 'A' && Char.chr (Char.code row.[0] + length) <= 'H'
  | _ ->
      raise
        (Failure
           "Invalid orientation. Please try again with a new orientation for \
            your boat's placement. A valid orientation is an uppercase \
            character 'H' or 'V'.")

(** [no_overlap_boat coord_lst coord] is true when placing a given coordinate
    [coord] will not cause an overlap with the given boat coordinates and false
    otherwise. *)
let rec no_overlap_boat (coord_lst : (char * int) list) (coord : char * int) :
    bool =
  match coord_lst with
  | [] -> true
  | c :: t -> if c = coord then false else no_overlap_boat t coord

(** [no_overlap boats_lst coord] is true when placing a given coordinate [coord]
    will not cause an overlap anywhere on the baord and false otherwise. *)
let rec no_overlap (boats : boat list) (coord : char * int) : bool =
  match boats with
  | [] -> true
  | b :: t -> no_overlap_boat b.coord_lst coord && no_overlap t coord

(** [valid_coordinates boat_list coord_lst] is true if the coordinates in
    [coord_lst] being placed on the board will not cause overlap with any of the
    boats currently placed on the board and false otherwise. *)
let rec valid_coordinates (boats : boat list) (coord_lst : (char * int) list) :
    bool =
  match coord_lst with
  | [] -> true
  | c :: t -> no_overlap boats c && valid_coordinates boats t

(** [valid_direction dir] is true if dir is "V" or "H" and false otherwise. *)
let valid_direction (dir : string) : bool =
  if dir = "V" || dir = "H" then true else false

(** [get_length_of_boat boats name] is the length of the boat [name]. Example:
    [get_length_of_boat boats "Destroyer"] is 2. *)
let rec get_length_of_boat (boats : (string * int) list) (name : string) : int =
  match boats with
  | [] -> raise (Failure "This boat does not exist!")
  | (name', length') :: t ->
      if name' = name then length' else get_length_of_boat t name

(** [get_coordinates row col dir length] is the list of coordinates that will be
    occupied if a boat of length [length] is placed in row [row], column [col],
    and in the direction [dir]. *)
let rec get_coordinates (row : char) (col : int) (dir : string) (length : int) :
    (char * int) list =
  match dir with
  | "H" ->
      if length = 0 then []
      else
        List.append
          [ (row, col + length - 1) ]
          (get_coordinates row col dir (length - 1))
  | "V" ->
      if length = 0 then []
      else
        List.append
          [ (Char.chr (Char.code row + length - 1), col) ]
          (get_coordinates row col dir (length - 1))
  | _ ->
      raise
        (Failure
           "Invalid orientation. Please try again with a new orientation for \
            your boat's placement. A valid orientation is an uppercase \
            character 'H' or 'V'.")

(** [valid_input board input] is true when the command given in [input] can
    placed a boat on the board without breaking any of the esatblished rules. *)
let valid_input (board : t) (input : string list) : bool =
  match input with
  | [ boat'; row'; col'; dir' ] ->
      if
        valid_name board.boats board.boat_names boat'
        && valid_row row' dir' (get_length_of_boat board.boat_names boat')
        && valid_column col' dir' (get_length_of_boat board.boat_names boat')
        && valid_coordinates board.boats
             (get_coordinates row'.[0] (int_of_string col') dir'
                (get_length_of_boat board.boat_names boat'))
        && valid_direction dir'
      then true
      else false
  | _ -> false

(** [index row] is the corresponding index of a row [row] represented by a
    character. Ex: [index 'A'] is 0. *)
let index (row : char) : int = Char.code row - Char.code 'A'

(** [update_board board name row col acc length dir] is the new board after a
    boat has been added to the board [b]. *)
let rec update_board (board : t) (name : string) (row : string) (col : string)
    (acc : int) (length : int) (dir : string) : t =
  match dir with
  | d when d = "H" ->
      board.board.(index row.[0]).(int_of_string col + (acc - 1)) <-
        string_of_int length;
      if acc = 1 then
        {
          board = board.board;
          boat_names = board.boat_names;
          boats =
            {
              name;
              coord_lst = get_coordinates row.[0] (int_of_string col) "H" length;
            }
            :: board.boats;
          guessed_list = board.guessed_list;
          unguessed_list = board.unguessed_list;
          sunk = board.sunk;
        }
      else update_board board name row col (acc - 1) length d
  | d when d = "V" ->
      board.board.(index row.[0] + (acc - 1)).(int_of_string col) <-
        string_of_int length;
      if acc = 1 then
        {
          guessed_list = board.guessed_list;
          unguessed_list = board.unguessed_list;
          board = board.board;
          boat_names = board.boat_names;
          boats =
            {
              name;
              coord_lst = get_coordinates row.[0] (int_of_string col) "V" length;
            }
            :: board.boats;
          sunk = board.sunk;
        }
      else update_board board name row col (acc - 1) length d
  | _ ->
      raise
        (Failure
           "Invalid orientation. Please try again with a new orientation for \
            your boat's placement. A valid orientation is an uppercase \
            character 'H' or 'V'.")

(** [direction_input_invalid board input] is true when the input contains a
    valid row, valid column, valid boat, but not a valid direction. Ex:
    [direction_input_invalid board ("Destroyer"; "A"; "1"; "bad")] is true. *)
let direction_input_invalid (board : t) (input : string list) : bool =
  match input with
  | [ boat'; row'; col'; dir' ] ->
      if
        valid_name board.boats board.boat_names boat'
        && valid_row row' dir' (get_length_of_boat board.boat_names boat')
        && valid_column col' dir' (get_length_of_boat board.boat_names boat')
        && valid_coordinates board.boats
             (get_coordinates row'.[0] (int_of_string col') dir'
                (get_length_of_boat board.boat_names boat'))
        && not (valid_direction dir')
      then true
      else false
  | _ -> false
  [@@coverage off]

(** [column_input_invalid board input] is true when the input contains a valid
    row, valid direction, valid boat, but not a valid column. Ex:
    [column_input_invalid board ("Destroyer"; "A"; "14"; "H")] is true. *)
let column_input_invalid (board : t) (input : string list) : bool =
  match input with
  | [ boat'; row'; col'; dir' ] ->
      if
        valid_name board.boats board.boat_names boat'
        && valid_row row' dir' (get_length_of_boat board.boat_names boat')
        && (not
              (valid_column col' dir'
                 (get_length_of_boat board.boat_names boat')))
        && valid_coordinates board.boats
             (get_coordinates row'.[0] (int_of_string col') dir'
                (get_length_of_boat board.boat_names boat'))
        && valid_direction dir'
      then true
      else false
  | _ -> false

(** [row_input_invalid board input] is true when the input contains a valid
    column, valid direction, valid boat, but not a valid row. Ex:
    [row_input_invalid board ("Destroyer"; "5"; "1"; "H")] is true. *)
let row_input_invalid (board : t) (input : string list) : bool =
  match input with
  | [ boat'; row'; col'; dir' ] ->
      if
        valid_name board.boats board.boat_names boat'
        && (not
              (valid_row row' dir' (get_length_of_boat board.boat_names boat')))
        && valid_column col' dir' (get_length_of_boat board.boat_names boat')
        && valid_coordinates board.boats
             (get_coordinates row'.[0] (int_of_string col') dir'
                (get_length_of_boat board.boat_names boat'))
        && valid_direction dir'
      then true
      else false
  | _ -> false

(** [coordinate_input_invalid board input] is true when the input contains a
    valid column, valid row, valid direction, valid boat, but the coordinate as
    a pair is invalid because it causes an overlap. *)
let coordinate_input_invalid (board : t) (input : string list) : bool =
  match input with
  | [ boat'; row'; col'; dir' ] ->
      if
        valid_name board.boats board.boat_names boat'
        && valid_row row' dir' (get_length_of_boat board.boat_names boat')
        && valid_column col' dir' (get_length_of_boat board.boat_names boat')
        && (not
              (valid_coordinates board.boats
                 (get_coordinates row'.[0] (int_of_string col') dir'
                    (get_length_of_boat board.boat_names boat'))))
        && valid_direction dir'
      then true
      else false
  | _ -> false

(** [boat_input_invalid board input] is true when the input contains a valid
    row, valid column, and valid direction, boat is invalid. Ex:
    [boat_input_invalid board ("Leed"; "A"; "1"; "H")] is true. *)
let boat_input_invalid (board : t) (input : string list) =
  match input with
  | [ boat'; row'; col'; dir' ] ->
      if
        (not (valid_name board.boats board.boat_names boat'))
        && valid_row row' dir' (get_length_of_boat board.boat_names boat')
        && valid_column col' dir' (get_length_of_boat board.boat_names boat')
        && valid_coordinates board.boats
             (get_coordinates row'.[0] (int_of_string col') dir'
                (get_length_of_boat board.boat_names boat'))
        && valid_direction dir'
      then true
      else false
  | _ -> false

(* Documented in board.mli as a "public" function. *)
let invalid_input_message (board : t) (input : string list) : string =
  if boat_input_invalid board input then
    "Invalid boat. Please try again with a boat that is still unplaced."
  else if row_input_invalid board input then
    "Invalid row. Please try again with a new row for your coordinate. A valid \
     row is an uppercase character between 'A' and 'H'."
  else if column_input_invalid board input then
    "Invalid column. Please try again with a new column for your coordinate. A \
     valid column is a number between 0 and 7."
  else if coordinate_input_invalid board input then
    "Invalid coordinate. Please try again with a coordinate that does not \
     already contain a boat."
  else if direction_input_invalid board input then
    "Invalid orientation. Please try again with a new orientation for your \
     boat's placement. A valid orientation is an uppercase character 'H' or \
     'V'."
  else
    "Please try again. There are several issues with the placement you have \
     tried."

(* Documented in board.mli as a "public" function. *)
let add_boat (input : string) (board : t) =
  let split_input = String.split_on_char ' ' input in
  let is_valid = valid_input board split_input in
  if is_valid then
    match split_input with
    | [ boat'; row'; col'; dir' ] ->
        update_board board boat' row' col'
          (get_length_of_boat board.boat_names boat')
          (get_length_of_boat board.boat_names boat')
          dir'
    | _ -> board
  else raise (Failure (invalid_input_message board split_input))

(** [name_of_boat_hit row col lst boat] is [boat] if the boat named [boat] has
    been hit. If the boat named [boat] has not been hit, then
    [hit_coords row col coord_lst boat] is "". *)
let rec name_of_boat_hit (row : char) (col : int) (lst : (char * int) list)
    (boat : string) : string =
  match lst with
  | [] -> ""
  | (row', col') :: t ->
      if row = row' && col = col' then boat else name_of_boat_hit row col t boat

(** [check_for_hit r c lst] is the name of a boat that is placed on the board
    that has been hit. If no boats have been hit, then [check_for_hit r c lst]
    is "". *)
let rec check_for_hit_helper (row : char) (col : int) (boats : boat list) :
    string =
  match boats with
  | [] -> ""
  | { name; coord_lst } :: t ->
      if name_of_boat_hit row col coord_lst name = "" then
        check_for_hit_helper row col t
      else name

(* Documented in board.mli as a "public" function. *)
let check_for_hit (row : char) (col : int) (board : t) : bool =
  if check_for_hit_helper row col board.boats = "" then false else true

(** [add_hit_or_miss b r c hit] mutates the board array at coordinate ([r], [c])
    to indicate whether a guess caused a hit or a miss. If the guess resulted in
    a miss, the current value at ([r], [c]) on the board [b] is changed to "M"
    and if the guess resulted in a hit, the current value at ([r], [c]) on the
    board [b] is changed to "H." *)
let add_hit_or_miss (board : string array array) (row : char) (col : int)
    (hit : string) : unit =
  if hit = "" then board.(index row).(col) <- "M"
  else board.(index row).(col) <- "H"

(** [remove_from_unguessed unguessed_lst row col] is the unguessed list without
    the most recent guess ([row], [col]) made. *)
let remove_from_unguessed (unguessed_lst : (char * int) list) (row : char)
    (col : int) : (char * int) list =
  List.filter (fun (row', col') -> not (row = row' && col = col')) unguessed_lst

(** [remove_hit_from_coord_lst row col coord_lst] is the coordinate list
    [coord_lst] without the most recent guess ([row], [col] made. )*)
let rec remove_hit_from_coord_lst (row : char) (col : int)
    (lst : (char * int) list) : (char * int) list =
  match lst with
  | [] -> failwith "impossible"
  | (row', col') :: t ->
      if row = row' && col = col' then t
      else (row', col') :: remove_hit_from_coord_lst row col t

(** [update_boat_list row col lst n] is the updated list of boats placed on the
    board after a guess has been made. *)
let rec update_boat_list (row : char) (col : int) (boats : boat list)
    (boat : string) : boat list =
  match boats with
  | [] -> failwith "impossible"
  | { name; coord_lst } :: t ->
      if boat = name then
        { name; coord_lst = remove_hit_from_coord_lst row col coord_lst } :: t
      else { name; coord_lst } :: update_boat_list row col t boat

(** [is_unguessed input guessed_lst] is true when a given coordinate has not yet
    been guessed in the guessed list [guessed_lst] and is false otherwise. *)
let rec is_unguessed (input : string) (guessed_lst : (char * int) list) : bool =
  match guessed_lst with
  | [] -> true
  | (row', col') :: t ->
      if row' = input.[0] && string_of_int col' = String.make 1 input.[1] then
        false
      else is_unguessed input t

(** [valid_coordinate input] is true if the length of the list [input] is 1, if
    the length of the string inside of [input] is 2, if the first character of
    that string is a valid row character between 'A' and 'H', and if the second
    character of that string is a valid character between '0' and '7' and is
    otherwise false. *)
let valid_coordinate (input : string list) (board : t) : bool =
  match input with
  | [ input' ] ->
      if
        String.length input' = 2
        && (input'.[0] = 'A'
           || input'.[0] = 'B'
           || input'.[0] = 'C'
           || input'.[0] = 'D'
           || input'.[0] = 'E'
           || input'.[0] = 'F'
           || input'.[0] = 'G'
           || input'.[0] = 'H')
        && (input'.[1] = '0'
           || input'.[1] = '1'
           || input'.[1] = '2'
           || input'.[1] = '3'
           || input'.[1] = '4'
           || input'.[1] = '5'
           || input'.[1] = '6'
           || input'.[1] = '7')
        && is_unguessed input' board.guessed_list
      then true
      else false
  | _ -> false
  [@@coverage off]

(** [invalid_coordinate_message input] is the correct error message in the form
    of a string that should appear to the user based on why their input did not
    follow the given directions. *)
let invalid_coordinate_message (input : string) : string =
  if input = "" then "It appears you haven't made a guess. Please guess again!"
  else if input.[0] = ' ' then
    "Please try again with no spaces preceding your guess."
  else if
    not
      (input.[0] = 'A'
      || input.[0] = 'B'
      || input.[0] = 'C'
      || input.[0] = 'D'
      || input.[0] = 'E'
      || input.[0] = 'F'
      || input.[0] = 'G'
      || input.[0] = 'H')
  then
    "Invalid row. Please try again with a new row for your coordinate. A valid \
     row is an uppercase character between 'A' and 'H'."
  else if input.[1] = ' ' then "Please try again with no spaces in your guess."
  else if
    not
      (input.[1] = '0'
      || input.[1] = '1'
      || input.[1] = '2'
      || input.[1] = '3'
      || input.[1] = '4'
      || input.[1] = '5'
      || input.[1] = '6'
      || input.[1] = '7')
  then
    "Invalid column. Please try again with a new column for your coordinate. A \
     valid column is a number between 0 and 7."
  else "Please guess again."
  [@@coverage off]

(* Documented in board.mli as a "public" function. *)
let guess_coordinate (input : string) (board : t) : t =
  let split_input = String.split_on_char ' ' input in
  let is_valid = valid_coordinate split_input board in
  if is_valid then
    let row = input.[0] in
    let col = int_of_string (String.make 1 input.[1]) in
    let i = true in
    if i then (
      let hit_boat = check_for_hit_helper row col board.boats in
      add_hit_or_miss board.board row col hit_boat;
      if hit_boat = "" then
        {
          board = board.board;
          boat_names = board.boat_names;
          boats = board.boats;
          guessed_list = (row, col) :: board.guessed_list;
          unguessed_list = remove_from_unguessed board.unguessed_list row col;
          sunk = board.sunk;
        }
      else
        {
          board = board.board;
          boat_names = board.boat_names;
          boats = update_boat_list row col board.boats hit_boat;
          guessed_list = (row, col) :: board.guessed_list;
          unguessed_list = remove_from_unguessed board.unguessed_list row col;
          sunk = board.sunk;
        })
    else raise (Failure "Please try again!")
  else raise (Failure (invalid_coordinate_message input))

(* Documented in board.mli as a "public" function. *)
let make_random_guess (board : t) : char * int =
  match board.unguessed_list with
  | [] -> raise (Failure "All guesses exhausted.")
  | h :: t ->
      let num_left = List.length board.unguessed_list in
      let guess = Random.int (num_left - 1) in
      List.nth board.unguessed_list guess
  [@@coverage off]

(** [is_sunk_helper sunk_list boat] is true when a boat [boat] within the
    board's sunk list has already been sunk and is otherwise false. *)
let rec is_sunk_helper (sunk_list : string list) (boat : string) : bool =
  match sunk_list with
  | [] -> false
  | h :: t -> if h = boat then true else is_sunk_helper t boat

(** [is_sunk board boat] is true when a boat named [boat] on the board [board]
    has been sunk and is otherwise false. *)
let rec is_sunk (board : t) (boat : string) : bool =
  is_sunk_helper board.sunk boat

(** [check_if_boat_sunk_helper board boats] is a string indicating if the most
    recent guess on a board [board] has sunk a boat within the list of boats on
    the board [boats]. The string "" indicates that no boats were sink on the
    most recent turn. *)
let rec check_if_boat_sunk_helper (board : t) (boats : boat list) =
  match boats with
  | [] -> ""
  | { name; coord_lst } :: t ->
      if coord_lst = [] && not (is_sunk board name) then name
      else check_if_boat_sunk_helper board t

(* Documented in board.mli as a "public" function. *)
let check_if_boat_sunk (board : t) : string =
  check_if_boat_sunk_helper board board.boats

(* Documented in board.mli as a "public" function. *)
let sink (boat : string) (board : t) =
  if boat = "" then board
  else
    {
      board = board.board;
      boat_names = board.boat_names;
      boats = board.boats;
      guessed_list = board.guessed_list;
      unguessed_list = board.unguessed_list;
      sunk = boat :: board.sunk;
    }

(** [print_row row] prints the string array [row]. *)
let print_row (row : string array) : unit =
  Array.iter
    (fun y ->
      if y = "H" then (ANSITerminal.print_string [ ANSITerminal.red ]) (y ^ " ")
      else if y = "M" then
        (ANSITerminal.print_string [ ANSITerminal.white ]) (y ^ " ")
      else if y = "0" then
        (ANSITerminal.print_string [ ANSITerminal.blue ]) (y ^ " ")
      else (ANSITerminal.print_string [ ANSITerminal.green ]) (y ^ " "))
    row;
  print_endline ""
  [@@coverage off]

(* Documented in board.mli as a "public" function. *)
let print_board (board : t) = Array.iter print_row board.board [@@coverage off]

(* Documented in board.mli as a "public" function. *)
let print_hits_and_misses (board : t) : unit =
  let hits_and_misses =
    Array.map
      (fun x -> Array.map (fun y -> if y = "H" || y = "M" then y else "0") x)
      board.board
  in
  Array.iter
    (fun x ->
      Array.iter
        (fun y ->
          if y = "H" then
            (ANSITerminal.print_string [ ANSITerminal.red ]) (y ^ " ")
          else if y = "M" then
            (ANSITerminal.print_string [ ANSITerminal.white ]) (y ^ " ")
          else (ANSITerminal.print_string [ ANSITerminal.yellow ]) (y ^ " "))
        x;
      print_endline "")
    hits_and_misses
  [@@coverage off]
