(** The module compilation unit Board initializes individual boards,
    additionally modifying and holding all of the data for a single board
    (whether a computer board or a player board). *)

type t
(** The abstract type value representing a battleship board. *)

val empty : t
(** [empty] is an empty board. *)

val init_board : (string * int) list -> t
(** [init_board boats] initializes an empty board. A newly_initialized empty
    board will contain 8 rows and 8 columns with each cell holding a "0" to
    indicate that there are no ships or guesses on the cell. *)

val is_blank : t -> bool
(** [is_blank board] is true when the board [board] is the intial state (every
    cell holds "0" to indicate no guesses or placed ships) and is false when
    there are ships and/or guesses on the board. *)

val get_board : t -> string array array
(** [get_board board] is an array representation of the board [board]. If the
    board currently has no placed boats or guesses on it, then [get_board board]
    is an array containing 8 arrays which each contain 8 elements all
    initialized to "0" to indicate that there are no ships or guesses, yet, on
    that cell. *)

val get_boats : t -> string list
(** [get_boats board] is the list of boats that are currently placed on the
    board [b]. If the board currently has no placed boats, [get_boats board] is
    []. Example: when only a cruiser is placed on a given board,
    [get_boats board] is ["Cruiser"]. *)

val get_num_sunk : t -> int
(** [get_num_sunk board] is the number of boats that have been sunk on the
    board. If no boats have been sunk, then [get_num_sunk board] is 0. *)

val invalid_input_message : t -> string list -> string
(** [invalid_input_message board input] is the correct error message that should
    be displayed to the user when a player input is invalid. *)

val add_boat : string -> t -> t
(** [add_boat input board] is the new board when a boat has been added to the
    board [board] with the boat, coordinate placement, and orientation all being
    determined by the player's input [input]. If the player's input is invalid,
    then [add_boat b input] will, instead, raise an exception. *)

val guess_coordinate : string -> t -> t
(** [guess_boat input board] is the board after a guess has been made. If the
    guessed cell (determined by [input]) holds a boat, the cell is changed to
    indicate it has been hit. If the guessed cell does not hold a boat, the cell
    is changed to indicate it has been missed. *)

val make_random_guess : t -> char * int
(** [make_random_guess board] is a coordinate in the form (row, column) that has
    not, yet, been guessed on the board [board] where row will contain a
    character in 'A' - 'H' and column will hold an integer in 0 to 7. *)

val check_if_boat_sunk : t -> string
(** [check_if_boat_sunk board] is a string indicating if the most recent guess
    on a board [board] sunk a boat. The string "" indicates that no boats were
    sink on the most recent turn. Example: if the most recent boat that was sunk
    on the board [board] was a cruiser, [check_if_boat_sunk board] returns
    "Cruiser." *)

val check_for_hit : char -> int -> t -> bool
(** [check_for_hit row col board] is true if the most recent guess on a board
    [board] hit a boat and false otherwise. Example: if the most recent guess
    hit a boat on the board [board], [check_for_hit row col board] returns true. *)

val is_sunk : t -> string -> bool
(** [is_sunk board name] is true if boat [name] has been sunk in board [board]
    and false if boat [name] has not been sunk. Requires: name is a valid string
    representing a boat ("Cruiser", "Battleship", "Carrier", "Destroyer"). *)

val sink : string -> t -> t
(** [sink boat board] is the board [board] once a boat named [boat] has been
    sunk. *)

val print_board : t -> unit
(** [print_board board] prints the board [board]. *)

val print_hits_and_misses : t -> unit
(** [print_hits_and_misses board] prints the board [board] with hits being
    represented as H, misses being represented as M, and coordinates not yet
    guessed as 0. *)
