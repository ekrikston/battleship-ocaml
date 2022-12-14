(** The Opponent compilation unit manages the selection and initialization of an
    opponent board. *)

val choose_board : int -> Board.t
(** [choose_board number] returns the board associated with the int [number]
    passed in. If the int [number] passed in is outside the bounds of the number
    of opponent boards available, an exception will be raised. Example: if
    choose_board 0 is called, board0 of type Board.t is returned. *)
