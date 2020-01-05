(**The module [Model] is used to model a chessboard and support moving
  pieces with rule-checking.*)

(** type [t] is an abstract type representing a board state*)
type t

(** type [idx] is a type synonym.
  A pair [(a,b)] represents the square in file [a] and row [b].*)
type idx = char*int

(** type [color] is used to keep track of which pieces belong to which players,
   color the board, and name the players.*) 
type color = Black | White 

(** type [piece'] is a helper type used to define [piece]*)
type piece' = King | Queen | Rook | Bishop | Knight | Pawn

(** type [piece] is a type used to represent a piece*)
type piece = Piece of piece'*color | Nothing

(** [((idx1,idx2),c)] represents a move from [i1 -> i2] where the player
  attempting to execute the move is color [c]*)
type move = (idx*idx)*color
        
(**This constant gives the starting state of chess.*)
val  start_board : t

(**This helper function verifies that an index is valid.
   Note this is usually unecessary since [make_idx] will throw an error if
   you attempt to construct an invalid [idx], and you should always use this
   function.*)
val  valid_idx : idx -> bool

(**[get_piece board idx] returns the piece at the square [idx] on the board
  [board]*)
val  get_piece : t -> idx -> piece

(** [set_piece board idx piece] manually resets the piece at position [idx]
  to [piece], overwriting whatever piece was there, and returns the result.
  Outside of internal [Model] module use, this is only intended to be used
    when allowing a player to upgrade their advanced pawn and does
    no rule-checking whatsoever.*)
val  set_piece : t -> idx -> piece -> t

(** [make_move board move] attempts to make move [move] on the board [board]
  and returns the board that results after this move has been executed.
  If [move] is not a valid move for [board] then this will raise:
  [Bad_Move err] where [err] is an error message to be displayed to the user.
  If [move] requires more information or needs to execute things not accessible
  in model it can also raise
  [Pawn_Upgrade (board,color,idx)] if player [color] successfully moved their
  pawn to the final row and we need to ask them what they would like to
    upgrade to, where [board] is the board with the pawn moved, and [idx] is
    the square where that pawn currently is, or
  [Win (board,color)] if player [color] has won the game and [board] is the
  final state of the board.*)
val  make_move : t -> move -> t 

(** type [pos_rep] is used as a type to communicate with the [View] module
  and records the information necessary to display a square.
  It allows calculation of the color of a square to be done in this module where
  this is done more easily.*)
type pos_rep = Taken of piece | Empty of color

(** [board_view board] gives a 2-dimensional list of [pos_rep] representations
   for each square, where the item at position [(i,j)] in the 2-d list
   represents the square in row [i] and file [j].*)
val  board_view : t -> pos_rep list list * piece list

(**[make_idx c i] tries to create the index [(c,i)] and fails if [(c,i)] is not
   a valid index.*)
val  make_idx : char -> int -> idx

(** [in_check board c] returns whether player [c] is in check on the board
  [board]*)
val in_check : t -> color -> bool

(** [achieved_mate board color] returns true if [color] is in checkmate
    on [board]*)
val achieved_mate : t -> color -> bool


(**exception [Pawn_Upgrade (board,c,idx)] is thrown by [make_move] when the
  move will require asking the player how they would like to upgrade their pawn.
  [board] is the state of the board with the piece in question still represented
  as a pawn and [idx] points to this pawn in question.*)
exception Pawn_Upgrade of t*color*idx

(**exception [Bad_Move str] is thrown when a player inputs a move that they
  are not allowed to execute. [str] is a message that should be displayed to the
  player.*)
exception Bad_Move of string

(**exception [Win (board,c)] is thrown by [make_move] when the move results in
  a win with final board state [board], and [c] is the winner.*)
exception Win of t*color

