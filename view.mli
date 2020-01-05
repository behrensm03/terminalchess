(** The module [View] is used solely for displaying a board to the terminal.*)
open Model
(** [print_board in_check unicode board] will print the board [board] to the terminal in
     a human-readable format.
     It will display an in check warning if [in_check] is true and it will print in
     unicode if [unicode] is [true] and ascii if [unicode] is [false].*)
val print_board : bool -> bool -> Model.t -> unit 

(** [test_unicode ()] prints out a test of the unicode to see if it displays
    properly on the users machine *)
val test_unicode : unit -> unit
