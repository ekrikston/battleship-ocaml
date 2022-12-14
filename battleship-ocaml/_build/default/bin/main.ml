open Battleship
open Game

let quit_state = Game.empty

let quit_message () =
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "\nGAME OVER! You quit. Play again soon! \n\n"

let quit_play_state = (Game.empty, "")

let rec initialize_game () =
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "\nChoose a board by picking a number between 0 and 14 (inclusive):\n\n";
  print_string "> ";
  let input = read_line () in
  if input = "Quit" then quit_state
  else
    try Game.init_state input
    with Failure str ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        ("\n\n" ^ str);
      initialize_game ()

and retry s =
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "\n\n\n\
     Here are some reminders for the instructions as to how to place a boat \n\
     on your board:\n\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "First, the name of the boat followed by a space. \n\
     Next, the letter of the row you want the boat in followed by a space. \n\
     Next, the number of the column you want the leftmost part of the boat in \
     followed by a space. \n\
     Last, either H or V whether you want your boat to be horizontal or \
     vertical, respectively. \n\
     Make sure you do not overlap any boats. \n\
     Example: Cruiser A 0 H\n\n";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Note: The grid is 8x8 with rows A-H (top to bottom) and columns 0-7 (left \
     to right). \n\n";
  print_string "> ";
  let user_input = read_line () in
  if user_input = "Quit" then quit_state
  else
    try
      let new_state = Game.add_player_boat user_input s in
      if List.length (Game.get_player_boats new_state) < 4 then
        set_up new_state false
      else new_state
    with Failure str ->
      ANSITerminal.print_string
        [ ANSITerminal.black; ANSITerminal.Bold ]
        "\n\
         _________________________________________________________________________\n";
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        ("\n" ^ str);
      retry s

and set_up (state : Game.t) (instructions : bool) : Game.t =
  ANSITerminal.print_string
    [ ANSITerminal.black; ANSITerminal.Bold ]
    "\n\
     _________________________________________________________________________\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nYour current board looks like this:\n\n";
  Game.print_player_board state;
  if instructions then (
    ANSITerminal.print_string
      [ ANSITerminal.red; ANSITerminal.Bold ]
      "\nHere are the instructions for how to place a boat on your board:\n";
    ANSITerminal.print_string [ ANSITerminal.white ]
      "First, the name of the boat followed by a space. \n\
       Next, the letter of the row you want the boat in followed by a space. \n\
       Next, the number of the column you want the leftmost part of the boat \
       in followed by a space. \n\
       Last, either H or V whether you want your boat to be horizontal or \
       vertical, respectively. \n\
       Make sure you do not overlap any boats. \n\
       Example: Cruiser A 0 H\n\n";
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Note: The grid is 8x8 with rows A-H (top to bottom) and columns 0-7 \
       (left to right). \n\n";
    print_string "> ";
    let user_input = read_line () in
    if user_input = "Quit" then quit_state
    else
      try
        let (new_state : Game.t) = Game.add_player_boat user_input state in
        if List.length (Game.get_player_boats new_state) < 4 then
          set_up new_state false
        else new_state
      with Failure str ->
        ANSITerminal.print_string
          [ ANSITerminal.black; ANSITerminal.Bold ]
          "\n\
           _________________________________________________________________________\n";
        ANSITerminal.print_string
          [ ANSITerminal.red; ANSITerminal.Bold ]
          ("\n" ^ str);
        retry state)
  else (
    ANSITerminal.print_string
      [ ANSITerminal.red; ANSITerminal.Bold ]
      "\nPlease place another boat on your board:\n\n";
    print_string "> ";
    let user_input = read_line () in
    if user_input = "Quit" then quit_state
    else
      try
        let (new_state : Game.t) = Game.add_player_boat user_input state in
        if List.length (Game.get_player_boats new_state) < 4 then
          set_up new_state false
        else new_state
      with Failure str ->
        ANSITerminal.print_string
          [ ANSITerminal.black; ANSITerminal.Bold ]
          "\n\
           _________________________________________________________________________\n";
        ANSITerminal.print_string
          [ ANSITerminal.red; ANSITerminal.Bold ]
          ("\n" ^ str);
        retry state)

and first_message s =
  ANSITerminal.print_string [ ANSITerminal.yellow ] "\nOpponent board:\n\n";
  Game.print_computer_board s;
  ANSITerminal.print_string
    [ ANSITerminal.black; ANSITerminal.Bold ]
    "\n\
     _________________________________________________________________________\n";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\
     Please guess a coordinate on the board where you think your opponent's\n\
     ship may be. Enter a character A-H for the row, followed by a number 0-7\n\
     for the column. \n\
     Note: your guess may not contain spaces: \n"

and second_message () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\
     Please guess a coordinate on the board where you think your opponent's\n\
     ship may be. Enter a character A-H for the row, followed by a number 0-7\n\
     for the column. \n\
     Note: your guess may not contain spaces: \n"

and retry_play (s : Game.t) first : Game.t * string =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Please guess a coordinate on the board where you think your opponent's\n\
     ship may be. Enter a character A-H for the row, followed by a number 0-7\n\
     for the column. \n\
     Note: your guess may not contain spaces: \n\n";
  print_string "> ";
  let user_input = read_line () in
  if user_input = "Quit" then quit_play_state
  else
    try
      let new_state = Game.guess_against_computer user_input s in
      Game.print_computer_board s;
      print_endline "";
      let computer_sunk = Game.check_computer_sunk new_state in
      if computer_sunk = "" then
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "You sunk no new ships with that guess. \n"
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          ("Congrats! You sunk your opponent's " ^ computer_sunk
         ^ " ship with that guess!\n");
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\nPlease press return to continue. \n";
      print_string "> ";
      match read_line () with
      | _ ->
          let final_state = Game.computer_sink computer_sunk new_state in
          if Game.get_num_computer_sunk final_state < 4 then
            play final_state "computer" first
          else (final_state, "player")
    with Failure str ->
      ANSITerminal.print_string
        [ ANSITerminal.black; ANSITerminal.Bold ]
        "\n\
         _________________________________________________________________________\n";
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        ("\n" ^ str);
      retry_play s first

and play s turn first : Game.t * string =
  match turn with
  | "player" -> (
      if first then first_message s else second_message ();
      print_string "> ";
      try
        let user_input = read_line () in
        if user_input = "Quit" then quit_play_state
        else normal_player_turn user_input s
      with Failure str ->
        ANSITerminal.print_string
          [ ANSITerminal.black; ANSITerminal.Bold ]
          "\n\
           _________________________________________________________________________\n";
        ANSITerminal.print_string
          [ ANSITerminal.red; ANSITerminal.Bold ]
          ("\n" ^ str);
        retry_play s first)
  | "computer" -> normal_computer_turn s
  | _ -> raise (Failure "invalid player")

and normal_computer_turn s =
  let new_state, input = Game.computer_turn_to_guess s in
  let player_sunk = Game.check_player_sunk new_state in
  Game.print_player_board s;
  let final_state = Game.computer_sink player_sunk new_state in
  let hit = Game.check_for_computer_hit input s in
  if player_sunk = "" && hit then
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\n\
       Hit! The opponent gets to guess again. \n\
       The opponent has sunk no new ships with their guess.\n"
  else if player_sunk <> "" then
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ("Hit! The opponent has sunk your " ^ player_sunk
     ^ " ship with that guess!\nThe opponent gets to guess again\n")
  else
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\n\
       Miss! Your turn! \n\
       The opponent has sunk no new ships with their guess.\n";
  ANSITerminal.print_string
    [ ANSITerminal.black; ANSITerminal.Bold ]
    "\n\
     _________________________________________________________________________\n";
  if Game.get_num_computer_sunk final_state >= 4 then (final_state, "computer")
  else (
    ANSITerminal.print_string
      [ ANSITerminal.red; ANSITerminal.Bold ]
      "\nPlease press return to continue. \n";
    print_string "> ";
    match read_line () with
    | _ ->
        if hit then play final_state "computer" false
        else play final_state "player" true)

and normal_player_turn input s =
  let new_state = Game.guess_against_computer input s in
  Game.print_computer_board s;
  print_endline "";
  let computer_sunk = Game.check_computer_sunk new_state in
  let final_state = Game.computer_sink computer_sunk new_state in
  let hit = Game.check_for_player_hit input s in
  if computer_sunk = "" && hit then
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "Hit! Guess again! \nYou sunk no new ships with that guess."
  else if computer_sunk <> "" then
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      ("Hit! Guess again! \nCongrats! You sunk your opponent's " ^ computer_sunk
     ^ " ship with that guess!")
  else
    ANSITerminal.print_string [ ANSITerminal.yellow ] "Miss! Your turn is over.";
  ANSITerminal.print_string
    [ ANSITerminal.black; ANSITerminal.Bold ]
    "\n\
     _________________________________________________________________________\n";
  if Game.get_num_computer_sunk final_state < 4 then
    if hit then play final_state "player" false
    else (
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\nPlease press return to continue. \n";
      print_string "> ";
      match read_line () with
      | _ -> play final_state "computer" true)
  else (final_state, "player")

and main () : unit =
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "\n\nThese are the rules of Battleship:\n\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "To begin the game, you will choose an opponent board to play against. \
     Choose \n\
     a number between 1 and 15 (inclusive).\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "Then, you will choose where to place the following four battleships, with \
     each\n\
     battleship's respective length followed by it:\n";
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "\nCarrier (5), Battleship (4), Cruiser (3), and Destroyer (2).\n\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "Once you place your ships the game begins. You will take turns with the \n\
     opponent guessing the coordinates of each other's battleships. \n\n\
     If you hit a ship, you can guess again.\n\
     If you miss, it is your opponent's turn.\n\
     When all of the coordinates of a ship have been guessed, that ship is \
     sunk. \n\
     The game is over when all of the battleships of a player have been sunk.\n";
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "If at any point in the game you choose to leave the game, type \"Quit\".\n\n";
  let init_state = initialize_game () in
  if init_state <> quit_state then (
    ANSITerminal.print_string
      [ ANSITerminal.red; ANSITerminal.Bold ]
      "\n\
       You've chosen an opponent board. Now it's time to set up your own board.\n";
    let player_set_up_state = set_up init_state true in
    if player_set_up_state <> quit_state then (
      ANSITerminal.print_string
        [ ANSITerminal.black; ANSITerminal.Bold ]
        "\n\
         _________________________________________________________________________\n";
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nHere is your final board: \n\n";
      Game.print_player_board player_set_up_state;
      ANSITerminal.print_string
        [ ANSITerminal.black; ANSITerminal.Bold ]
        "\n\
         _________________________________________________________________________\n";
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\nNow that your board is set up it is time to start playing!\n";
      ANSITerminal.print_string
        [ ANSITerminal.yellow; ANSITerminal.Bold ]
        "\n\
         You will be able to track your hits and misses against your opponent\n\
         on this board.\n\n";
      Game.print_computer_board player_set_up_state;
      ANSITerminal.print_string
        [ ANSITerminal.black; ANSITerminal.Bold ]
        "\n\
         _________________________________________________________________________\n";
      let played_state = play player_set_up_state "player" false in
      if played_state <> quit_play_state then
        match played_state with
        | _, "player" ->
            ANSITerminal.print_string
              [ ANSITerminal.red; ANSITerminal.Bold ]
              "\nGAME OVER!! Congratualations, YOU WON! \n\n"
        | _, "computer" ->
            ANSITerminal.print_string
              [ ANSITerminal.red; ANSITerminal.Bold ]
              "\nGAME OVER!! You lost. Better luck next time. \n\n"
        | _ -> raise (Failure "impossible")
      else quit_message ())
    else quit_message ())
  else quit_message ()

(* Execute *)
let () = main ()