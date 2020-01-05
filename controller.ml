open Model
open View

exception Bad_Command of string

type move_command = Model.idx * Model.idx

type command =
  | Move of (move_command*Model.color)
  | Surrender of Model.color

type move_result =
  | Left of Model.t
  | Victory of string
  | Right of string*Model.t

let get_idx str =
  if String.length str != 2 
  then raise (Bad_Command "The index you provided is not of length two.")
  else 
    try 
      Model.make_idx (String.get str 0) (String.sub str 1 1
                                         |> int_of_string) 
    with
    | _ -> raise 
             (Bad_Command "The index you provided is not of the correct form. ")

let parse_move color str =
  let error_msg = "The syntax for moving a piece is move [source] to [target]\
                   as in 'move H5 to H6'."
  in
  match str with
  | source::_::target::[] -> (
      try
        Move ((get_idx source, get_idx target),color)
      with
      | Bad_Command s -> raise (Bad_Command (s^error_msg))
    )
  | _ -> raise (Bad_Command error_msg)

let parse_command color input =
  input
  |> String.trim
  |> (fun x -> if x = ""
       then raise (Bad_Command "Please type a command.") 
       else x)
  |> String.split_on_char ' '
  |> List.map String.trim
  |> List.filter (fun x -> not (x = ""))
  |> (fun str -> match str with
      | key::rest -> (
          match key with
          | "move" -> parse_move color rest
          | "surrender" -> Surrender color
          | _ -> raise (Bad_Command "That command is not recognized.")
        )
      |  _ -> raise (Bad_Command "This command is not recognized.")
    )

let rec get_command c _ =
  let turn_string =
    match c with
    | Black -> "Black's turn"
    | White -> "White's turn"
  in
  print_string (turn_string^"> ");
  try parse_command c (read_line ()) with
  | Bad_Command s -> print_endline s; get_command c ()
  | _ -> failwith "unexpected runtime error"

let opposite_color = function
  | White ->  Black
  | Black -> White

let rec get_upgrade_request fst c =
  let _ = if fst then
      (print_endline "You have advanced a pawn to the final row.";
       print_endline "What piece would you like to upgrade to?";
       print_endline "Type bishop, rook, queen, or knight";
       print_string "> ")
    else ()
  in
  let input = read_line () in
  String.trim input
  |> (fun x ->
      match x with
      | "bishop" -> Piece (Bishop,c)
      | "rook" -> Piece (Rook,c)
      | "queen" -> Piece (Queen,c)
      | "knight" -> Piece (Knight,c)
      | _ ->
        print_endline "That is not one of your options.";
        print_string "> ";
        get_upgrade_request false c)

let string_of_color = function
  | White -> "White"
  | Black -> "Black"


let white_win verbose unicode b = 
  begin
    if verbose then
      View.print_board false unicode b
    else ()
  end ;
  Victory "Checkmate. White wins."


let black_win verbose unicode b = 
  ( 
    if verbose then
      View.print_board false unicode b
    else ()
  ) ;
  Victory "Checkmate. Black wins."

let pawn_upgrade (b, c, idx) verbose unicode auto_request = 
  let new_piece =
    match auto_request with
    | None -> get_upgrade_request true c
    | Some p -> p
  in
  let new_board = Model.set_piece b idx new_piece in
  (if achieved_mate new_board c then
     ((if verbose then
         View.print_board false unicode new_board
       else ()) ;
      Victory ("Checkmate. "^(string_of_color c)^" wins."))
   else (Left new_board)
  )


let run_command board unicode verbose auto_request cmd =
  match cmd with
  | Move move_cmd ->
    begin
      try
        let new_board = Model.make_move board move_cmd in
        Left new_board
      with
      | Bad_Move msg -> Right (msg,board)
      | Win (b,White) -> white_win verbose unicode b
      | Win (b,Black) -> black_win verbose unicode b
      | Pawn_Upgrade (b,c,idx) -> 
        pawn_upgrade (b, c, idx) verbose unicode auto_request
    end
  | Surrender color ->
    begin 
      let alt_color = match color with
        | Black -> "White"
        | White -> "Black"
      in Victory (alt_color^" wins.")
    end

let handle_result = function
  | Left board -> board
  | Right (error,board) ->
    print_endline error;
    board
  | Victory winner ->
    print_endline winner;
    exit 0

let rec get_yes_no _ =
  print_endline "Please input (yes/no)";
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with
  | "yes" -> true
  | "no" -> false
  | _ ->
    print_endline "That input was not recognized.";
    get_yes_no ()

let welcome _ =
    print_endline "";
    print_endline "";
    print_endline "Welcome to chess!";
    print_endline "The basic commands are:";
    print_endline
      "move E2 to E4 --  where you can replace E2 and E4 with the start and \
      finish squares for the move you want to do, and";
    print_endline "surrender -- which forfeits the game"

let main _ = 
  let rec main_aux (board:Model.t) (turn:Model.color) (unicode:bool) =
    let handle_result = function
      | Left board -> main_aux board (opposite_color turn) unicode
      | Right (error,board) ->
        print_endline error;
        main_aux board turn unicode
      | Victory winner ->
        print_endline winner;
        exit 0
    in
    board
    |> View.print_board (in_check board turn) unicode
    |> get_command turn
    |> run_command board unicode true None
    |> handle_result
  in
  let unicode =
    print_endline "We have found that sometimes unicode does not display \
                   properly on \ncertain operating systems. \
                   \nBelow you should see some chess pieces. \
                   \nIf you do, answer yes, if not, \
                   answer no.";
    test_unicode ();
    get_yes_no ()
  in
  welcome ();
  main_aux Model.start_board White unicode

exception Stop_Early of string*bool

type test_move =
  | Upgrade of command*piece
  | Command of command

let test_main (moves: test_move list) =
  let rec main_aux (board:Model.t) (turn:Model.color) move =
    let handle_result = function
      | Left board -> board
      | Right (error,board) ->
        raise (Stop_Early (error,false))
      | Victory winner ->
        raise (Stop_Early (winner,true))
    in
    match move with
    | Upgrade (cmd,upgrade) ->
      (run_command board true false (Some upgrade) cmd
       |> handle_result
      )
    | Command cmd ->
      (run_command board true false None cmd 
       |> handle_result)
  in
  List.fold_left
    (fun board x -> main_aux board White x)
    Model.start_board moves 
