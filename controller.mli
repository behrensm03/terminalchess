(** The module [Controller] uses the modules [Model] and [View]
    to execute the chess game.*)
open Model

(** An abstract type representing a basic move that is a helper type
  for defining the type [command]*)
type move_command = Model.idx * Model.idx

(** type [command] represents all commands a player can give, either they
   surrender or submit a [move_command].*)
type command =
  | Move of (move_command*Model.color)
  | Surrender of  Model.color

(** [move_result] is an abstract type that encapsulates the result of
   executing a move command.*)
type move_result

(*type moveset = move_command -> Model.t -> move_result*)
(** [parse_command c str] parses the input [str] into a command given
   that the
  current player is [c]*)
val parse_command : Model.color->string -> command

(** [run_command board unicode verbose auto_requrest cmd] 
    returns the result of executing [cmd] on the
    board [board].
    If the board needs to displayed it uses unicode if [unicode] is
    [true] and ascii if it is instead [false].
    If [verbose] is [true] then the function may print out
    requests for the user to input more data, otherwise if [verbose] is
    [false] then it will not print anything ever.
    If [auto_request] is [Some p] then, if a pawn is advanced to the final
    row, that pawn will be automatically upgraded to piece [p].
    If [auto_requrest] is instead [None] then the function will ask
    the user what piece they would like to upgrade to.*)
val run_command : Model.t -> bool -> bool ->
  piece option -> command -> move_result 

(** [main ()] runs the game*)
val main : unit -> unit

(** [Stop_Early (s,b)] is raised by [test_main] when an exception is raised
    while attempting to execute the list of commands, where [s] is the
    string attached to the error and [b] is [true] if the exception was that
    someone won the game and [b] is [false] if the exception was anything
    else.*)
exception Stop_Early of string*bool

(** [test_move] is a type used to run [test_main].
    [Upgrade (cmd,piece)] is used to indicate you would like to use move
    [cmd], which will move a pawn to the last row, and you would like to
    upgrade that pawn immediately to [piece].
    [Command cmd] is used when you just want to execute [cmd]*)
type test_move =
  | Upgrade of command*piece
  | Command of command

(** [test_main move_list] executes the list of moves represented by the
    [move_list] and returns the board at the end of executing those moves.
    Raises [Stop_Early]. See the description of [Stop_Early] for when this
    occurs.
    It is intended for testing purposes.*)
val test_main : test_move list -> Model.t 

