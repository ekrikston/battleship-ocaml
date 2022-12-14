(** The module compilation unit Game initializes a game state. Additionally, it
    modifies and holds all of the data managing the interaction of a game
    between a player and a computer opponent. *)

type t
(** The abstract type value representing a battleship game state. *)

val empty : t
(** [empty] is a state with two empty boards. *)

val init_state : string -> t
(** [init_state input] is the initial state of the game holding both an empty
    player board and an initialized computerized opponent board. A
    newly-initialized empty board will contain 8 rows and 8 columns with each
    cell holding a "0" to indicate that there are no ships or guesses, yet, on
    that cell. A newly-initialized computer opponent board will contain 8 rows
    and 8 columns with 4 ships placed on the board and a "0" on every other cell
    to indicate that there are no guesses, yet. *)

val add_player_boat : string -> t -> t
(** [add_player_boat state boat] is the state [state] with the boat named [boat]
    added to the player board within state [state]. *)

val get_player_board : t -> string array array
(** [get_player_board state] is the string array array representation of a
    player board in game state [state]. *)

val get_computer_board : t -> string array array
(** [get_computer_board state] is the string array array representation of a
    computer board in game state [state]. *)

val get_computer_board_player_view : t -> string list list
(** [get_computer_board_player_view state] is the string array array
    representaiton of a computer board in game state [state] with only hits and
    misses (no boat placements). *)

val get_num_computer_sunk : t -> int
(** [get_num_computer_sunk state] is the number of boats that have been sunk on
    the computer opponent board in the game state [state]. *)

val get_num_player_sunk : t -> int
(** [get_num_player_sunk state] is the number of boats that have been sunk on
    the player board in the game state [state]. *)

val get_player_boats : t -> string list
(** [get_player_boats state] is the list of boat names currently placed on the
    player board within game state [state]. *)

val get_computer_boats : t -> string list
(** [get_computer_boats state] is the list of boat names currently placed on the
    computer board within game state [state]. *)

val check_computer_sunk : t -> string
(** [check_computer_sunk state] is a string indicating if the most recent guess
    on the computer opponent board within state [state] sunk a boat. The string
    "" indicates that no boats were sink on the most recent turn. Example: if
    the most recent boat that was sunk on the board was a cruiser,
    [check_computer_sunk state] returns "Cruiser." *)

val check_player_sunk : t -> string
(** [check_player_sunk state] is a string indicating if the most recent guess on
    the player board within state [state] sunk a boat. The string "" indicates
    that no boats were sink on the most recent turn. Example: if the most recent
    boat that was sunk on the board within game state [state] was a cruiser,
    [check_player_sunk state] returns "Cruiser." *)

val check_for_computer_hit : string -> t -> bool
(** [check_for_computer_hit input state] is true if the most recent guess on a
    computer board within state [state] hit a boat and false otherwise. Example:
    if the most recent player guess against the computer board hit a boat within
    the board [state], [check_for_computer_hit input state] returns true. *)

val check_for_player_hit : string -> t -> bool
(** [check_for_player_hit input state] is true if the most recent guess on a
    player board within state [state] hit a boat and false otherwise. Example:
    if the most recent computer guess against the player board hit a boat within
    the board [state], [check_for_player_hit input state] returns true. *)

val guess_against_computer : string -> t -> t
(** [guess_against_computer state input] is the state [state] after the player
    has made a guess [input] against the computer opponent. *)

val computer_turn_to_guess : t -> t * string
(** [computer_turn_to_guess state] is the record (state [state], turn) where
    turn is a string indicating whose turn it is ("computer" or "player") after
    the computer has made a guess against the player. If the computer has made a
    correct guess, then the next turn is the computer's turn and is otherwise
    the player's turn. *)

val player_sink : string -> t -> t
(** [player_sink boat state] is the state [state] once a boat named [boat] on
    the player board has been sunk. *)

val computer_sink : string -> t -> t
(** [computer_sink boat state] is the state [state] once a boat named [boat] on
    the computer board has been sunk. *)

val print_player_board : t -> unit
(** [print_player_board state] prints the player board within state [state]. In
    the cells where a guess has been made, there is a "H" or a "M" to indicate a
    hit or miss. The cells where a boat is placed are also indicated. *)

val print_computer_board : t -> unit
(** [print_computer_board state] prints the opponent computer board within state
    [state]. In the cells where a guess has been made by the player, there is a
    "H" or "M" to indicate a hit or miss. The cells where a boat is placed are
    not indicated. *)
