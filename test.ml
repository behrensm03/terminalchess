(** Test Plan:
    The majority of the system was tested manually due to the nature
    of the game of chess. For example, en passant and 
    castling were tested manually. This is primarily due to the fact that
    writing out an entire state of the board is very difficult and prone to
    error while writing the test cases while visually inspecting the board
    before and after the move makes it apparent if an error occurred.
    Testing manually also allows us to check the model, view, and controller
    modules simulatenously as the controller links to the model module and 
    allows us to see errors following a mistyped input, the model checks the
    logic behind a move and figures whether or not it is valid, and the 
    view displays such changes to the board and state of the game following
    a commands input registered from controller to model. Certain aspects
    such as the display for taken pieces also cannot be effectively tested
    in any other manner because the whole point is to have it appear visually.

    In addition to testing manually, we have tested Controller and Model with
    automatic tests that run through a pre-determined sequence of commands
    beginning from the start board. We note that these tests do not test the
    function main defined in controller since they actually call the test_main
    function. This is necessary because calling the generic main function
    would require entering strings from StdIn which we cannot do automatically
    in any easy way, as well as printing out a bunch of stuff. Therefore the
    function main is tested manually.

    The first functions written in this test suite were written as black box
    testing, and they were written even before functionality was completed. 
    Following this set of tests were more robust testing on sequences of moves
    in which glass box testing was utilized to test execution paths in both
    the Controller and Model modules. Finally,there was not clear property 
    testing done, hence we could not use randomized testing.

    The primary purpose of the OUnit tests is for regression testing, and
    final manual testing is also performed.

    More specifically, these tests check for move validity for specific pieces, 
    valid board locations, taking your own piece, moving the opponent's pieces,
    moving pieces that are not present at a given board location, etc.
    Additionally, these tests only test the model module as it core
    functionality and has the most complicated logic and code to ensure
    that our version of chess works.

    This testing approach demonstrates the correctness of the system because
    in order to test the actual functionality and core of the game, such as 
    putting opponents into check, we found it much more efficient to use manual
    testing whereas the following OUnit tests were used primarily for 
    regression testing and ensuring that anything added did not generate new 
    errors with respect to these basic tests. *)

open OUnit2
open Model
open View
open Controller

let make_idx_test_failure
    (name : string)
    (c: char)
    (v: int) : test = 
  name >:: (fun _ ->
      assert_raises (Failure "invalid idx") (fun() -> make_idx c v))

let make_idx_test
    (name : string)
    (c : char)
    (v : int)
    (expected_output: Model.idx) : test = 
  name >:: (fun _ ->
      assert_equal (make_idx c v) expected_output)

let make_move_no_piece_failure 
    (name: string)
    (board: Model.t)
    (move: Model.move) : test =
  name >:: (fun _ ->
      assert_raises (Bad_Move "There is no piece at that location.")
        (fun () -> make_move board move))

let make_move_own_piece_failure 
    (name: string)
    (board: Model.t)
    (move: Model.move) : test =
  name >:: (fun _ ->
      assert_raises (Bad_Move "You cannot take  your own pieces.")
        (fun () -> make_move board move))

let make_move_opp_piece_failure 
    (name: string)
    (board: Model.t)
    (move: Model.move) : test =
  name >:: (fun _ ->
      assert_raises (Bad_Move "You cannot move the opponent's pieces.")
        (fun () -> make_move board move))

let make_move_rook_failure
    (name: string)
    (board: Model.t)
    (move: Model.move) : test =
  name >:: (fun _ ->
      assert_raises (Bad_Move "Sorry, that is not how rooks work.")
        (fun () -> make_move board move))

let make_move_bishop_failure
    (name: string)
    (board: Model.t)
    (move: Model.move) : test =
  name >:: (fun _ ->
      assert_raises (Bad_Move "That ain't how bishops work, son.")
        (fun () -> make_move board move))

let make_move_knight_failure
    (name: string)
    (board: Model.t)
    (move: Model.move) : test =
  name >:: (fun _ ->
      assert_raises (Bad_Move "Knight's definitely don't work like that.")
        (fun () -> make_move board move))

let make_move_queen_failure
    (name: string)
    (board: Model.t)
    (move: Model.move) : test =
  name >:: (fun _ ->
      assert_raises (Bad_Move "The queen can do a lot of things. \
                               Unfortunately, that is not one of them.")
        (fun () -> make_move board move))

let make_move_king_failure
    (name: string)
    (board: Model.t)
    (move: Model.move) : test =
  name >:: (fun _ ->
      assert_raises (Bad_Move "The king cannot do that.")
        (fun () -> make_move board move))

let make_move_pawn_failure
    (name: string)
    (board: Model.t)
    (move: Model.move) : test =
  name >:: (fun _ ->
      assert_raises (Bad_Move "Pawns don't work like that (I'm pretty sure).")
        (fun () -> make_move board move))


let model_tests =[
  make_idx_test_failure "invalid idx char" 'R' 7;
  make_idx_test_failure "invalid idx num" 'A' 0;
  make_idx_test_failure "invalid idx lower char" 'a' 1;
  make_idx_test_failure "invalid idx neg num and lower char" 'a' (-1); 
  make_idx_test_failure "invalid idx num and char" 'R' 0;
  make_idx_test "valid idx" 'A' 1 ('A',1);
  make_idx_test "valid idx2" 'H' 8 ('H',8);
  make_idx_test "valid idx3" 'A' 8 ('A',8);
  make_idx_test "valid idx4" 'H' 1 ('H',1);
  make_idx_test "valid idx" 'D' 4 ('D',4);

  make_move_no_piece_failure "invalid piece loc" (Model.start_board)
    ((('A',4),('A',5)),White);
  make_move_no_piece_failure "invalid piece loc2" (Model.start_board)
    ((('D',3),('A',1)),White);
  make_move_no_piece_failure "invalid piece loc3" (Model.start_board)
    ((('F',3),('A',1)),White);
  make_move_opp_piece_failure "invalid piece opponent" (Model.start_board)
    ((('A',8),('A',1)),White);
  make_move_opp_piece_failure "invalid piece opponent2" (Model.start_board)
    ((('A',1),('A',1)),Black);
  make_move_own_piece_failure "invalid can't take own piece" (Model.start_board)
    ((('A',1),('A',2)),White);
  make_move_own_piece_failure "invalid can't take own piece2" 
    (Model.start_board) ((('A',2),('A',1)),White);
  make_move_own_piece_failure "invalid can't take own piece3" 
    (Model.start_board) ((('A',8),('A',8)),Black);
  make_move_own_piece_failure "invalid can't take own piece4" 
    (Model.start_board) ((('A',7),('A',8)),Black);
  make_move_own_piece_failure "invalid can't take own piece5" 
    (Model.start_board) ((('A',8),('A',7)),Black);
  make_move_own_piece_failure "invalid can't take own piece6" 
    (Model.start_board) ((('A',1),('A',1)),White);

  make_move_rook_failure "invalid rook move1" (Model.start_board) 
    ((('A',1),('A',6)),White);
  make_move_rook_failure "invalid rook move2" (Model.start_board) 
    ((('A',1),('A',7)),White);
  make_move_rook_failure "invalid rook move3" (Model.start_board) 
    ((('H',1),('H',7)),White);
  make_move_rook_failure "invalid rook move4" (Model.start_board) 
    ((('A',8),('A',3)),Black);
  make_move_rook_failure "invalid rook move5" (Model.start_board) 
    ((('A',8),('A',1)),Black);
  make_move_rook_failure "invalid rook move6" (Model.start_board) 
    ((('H',8),('H',1)),Black);

  make_move_bishop_failure "invalid bishop-w move1" (Model.start_board)
    ((('C',1),('H',7)),White);
  make_move_bishop_failure "invalid bishop-w move2" (Model.start_board)
    ((('C',1),('H',3)),White);
  make_move_bishop_failure "invalid bishop-w move3" (Model.start_board)
    ((('C',1),('A',7)),White);
  make_move_bishop_failure "invalid bishop-w move4" (Model.start_board)
    ((('F',1),('H',7)),White);
  make_move_bishop_failure "invalid bishop-w move5" (Model.start_board)
    ((('F',1),('H',3)),White);

  make_move_bishop_failure "invalid bishop-b move6" (Model.start_board)
    ((('F',8),('A',2)),Black);
  make_move_bishop_failure "invalid bishop-b move1" (Model.start_board)
    ((('C',8),('H',2)),Black);
  make_move_bishop_failure "invalid bishop-b move2" (Model.start_board)
    ((('C',8),('H',6)),Black);
  make_move_bishop_failure "invalid bishop-b move3" (Model.start_board)
    ((('C',8),('A',2)),Black);
  make_move_bishop_failure "invalid bishop-b move4" (Model.start_board)
    ((('F',8),('H',2)),Black);
  make_move_bishop_failure "invalid bishop-b move5" (Model.start_board)
    ((('F',8),('H',6)),Black);
  make_move_bishop_failure "invalid bishop-b move6" (Model.start_board)
    ((('F',8),('A',2)),Black);

  make_move_knight_failure "invalid knight-w move1" (Model.start_board)
    ((('B',1),('A',4)),White);
  make_move_knight_failure "invalid knight-w move2" (Model.start_board)
    ((('B',1),('H',8)),White);
  make_move_knight_failure "invalid knight-w move3" (Model.start_board)
    ((('G',1),('H',4)),White);
  make_move_knight_failure "invalid knight-w move4" (Model.start_board)
    ((('G',1),('A',8)),White);

  make_move_knight_failure "invalid knight-b move1" (Model.start_board)
    ((('B',8),('A',5)),Black);
  make_move_knight_failure "invalid knight-b move2" (Model.start_board)
    ((('B',8),('H',1)),Black);
  make_move_knight_failure "invalid knight-b move1" (Model.start_board)
    ((('G',8),('H',5)),Black);
  make_move_knight_failure "invalid knight-b move2" (Model.start_board)
    ((('G',8),('A',1)),Black);

  (*still need to test failures for pawns, king, and queen*)
  make_move_pawn_failure "invalid pawn-w move1" (Model.start_board)
    ((('A',2),('A',5)),White);
  make_move_pawn_failure "invalid pawn-w move2" (Model.start_board)
    ((('A',2),('B',3)),White);
  make_move_pawn_failure "invalid pawn-b move1" (Model.start_board)
    ((('A',7),('A',4)),Black);
  make_move_pawn_failure "invalid pawn-b move2" (Model.start_board)
    ((('A',7),('B',6)),Black);

  make_move_king_failure "invalid king-w move1" (Model.start_board)
    ((('E',1),('B',6)),White);
  make_move_king_failure "invalid king-w move2" (Model.start_board)
    ((('E',1),('E',3)),White);
  make_move_king_failure "invalid king-b move1" (Model.start_board)
    ((('E',8),('B',3)),Black);
  make_move_king_failure "invalid king-b move2" (Model.start_board)
    ((('E',8),('E',6)),Black);

  make_move_queen_failure "invalid queen-w move1" (Model.start_board)
    ((('D',1),('C',3)),White);
  make_move_queen_failure "invalid queen-w move2" (Model.start_board)
    ((('D',1),('B',3)),White);
  make_move_queen_failure "invalid queen-w move3" (Model.start_board)
    ((('D',1),('D',3)),White);
  make_move_queen_failure "invalid queen-b move1" (Model.start_board)
    ((('D',8),('C',6)),Black);
  make_move_queen_failure "invalid queen-b move2" (Model.start_board)
    ((('D',8),('B',6)),Black);
  make_move_queen_failure "invalid queen-b move3" (Model.start_board)
    ((('D',8),('D',6)),Black);

  (*make_move_checkmate "four move checkmate" st4 ((('D',8),('H',4)),Black) *)
]

let make_move r1 c1 r2 c2 c =
  Command (Move ((make_idx r1 c1, make_idx r2 c2),c))

let run_manual_test moves _ =
  View.print_board false false (test_main moves)

let result_manual_test result moves name =
  name >:: (fun _ ->
      assert_raises result (fun _ -> test_main moves))

let no_fail_manual_test moves name =
  name >:: (fun _ ->
      (assert_equal true (try let _ = test_main moves in true with _ -> false)))


let opposite_color = function 
  | White -> Black
  | Black -> White

let string_of_color = function
  | White -> "White"
  | Black -> "Black"

let str_make_move (str_mv:string) c = 
  (* print_endline ("\n\n\n\n\n"^(string_of_color c)^str_mv); *)
  let r1 = String.get str_mv 0 in
  let r2 = String.get str_mv 3 in
  let c1 = int_of_string (String.sub str_mv 1 1) in
  let c2 = int_of_string (String.sub str_mv 4 1) in
  make_move r1 c1 r2 c2 c


let lazy_make_move_sequence (seq: string list) = 
  let rec aux c s = 
    match s with
    | h::t -> (str_make_move h c)::(aux (opposite_color c) t)
    | [] -> []
  in 
  aux White seq


let rare_bug_seq = ["F2 F4"; "D7 D5"; "F4 F5"; "E7 E6"; "F5 E6"; "D8 D7";
                    "H2 H3"; "E8 D8"; "H1 H2"; "B7 B6"; "H2 H1"; "C8 A6"; "H1 H2"; "D8 C8"; 
                    "E6 E7"; "C7 C5"; "H2 H1"; "C8 C7"; "H1 H2"; "C7 C6"; "H2 H1";
                    "C6 D6"; "H1 H2"; "D6 E6"; "H2 H1"; "F7 F5"; "H1 H2"; "G8 H6";
                    "H2 H1"; "H6 F7"; "H1 H2"; "A6 D3"; "H2 H1"; "B8 A6"; "H1 H2";
                    "D7 A4"; "H2 H1"; "A6 B8";"H1 H2"; "B8 D7"; "H2 H1"; "A8 C8"; 
                    "H1 H2"; "C8 C6"; "H2 H1"; "C6 D6"; "H1 H2"; "G7 G5"; "H2 H1";
                    "H8 G8"; "H1 H2"; "G8 G6"; "H2 H1"; "G6 F6"; "H1 H2"; "G5 G4";
                    "H2 H1"; "F8 H6"; "H1 H2"; "H6 F4"; "H2 H1"; "F4 E5"]

let last_move = 
  Upgrade(
    Move((('E', 7), ('E', 8)), White),  
    Piece(Queen, White))

let manual_no_exn =
  [no_fail_manual_test [make_move 'A' 2 'A' 3 White] "Simple pawn move.";
   no_fail_manual_test [make_move 'F' 2 'F' 4 White;
                        make_move 'E' 7 'E' 5 Black;
                        make_move 'F' 4 'E' 5 White;
                        make_move 'F' 8 'A' 3 Black;
                        make_move 'E' 2 'E' 3 White;
                        make_move 'F' 7 'F' 6 Black;
                        make_move 'E' 5 'F' 6 White;
                        make_move 'G' 7 'G' 6 Black;
                        make_move 'F' 6 'F' 7 White]
     "Weird bug in which checking for a check mate caused an uncaught \
      pawn upgrade request, which could then be accepted, bypassing opponents \
      move and taking their king, leading to fatal error.";
   no_fail_manual_test (lazy_make_move_sequence
                          ["G1 H3"; "A7 A5"; "G2 G3"; "B7 B6"; "F1 G2";
                           "C7 C5"; "E1 G1"])
     "Short castle in lower right corner.";
   no_fail_manual_test (lazy_make_move_sequence
                          ["A2 A3"; "B7 B6"; "B2 B3"; "C8 B7"; "C2 C4";
                           "B8 A6"; "D2 D3"; "E7 E6"; "G1 H3"; "D8 E7";
                           "E2 E3"; "E8 C8"]
                       ) "Long castle in upper left corner.";
   no_fail_manual_test (lazy_make_move_sequence
                          ["E2 E4"; "B7 B6"; "E4 E5"; "F7 F5"; "E5 F6"])
     "En passant"
  ]

let result_manual_tests =
  [
    (result_manual_test (Stop_Early ("Checkmate. Black wins.",true))  
       [make_move 'F' 2 'F' 3 White;
        make_move 'E' 7 'E' 5 Black;
        make_move 'G' 2 'G' 4 White;
        make_move 'D' 8 'H' 4 Black]
       "Four move checkmate.");
    (result_manual_test (Stop_Early ("Checkmate. White wins.",true))
       ((lazy_make_move_sequence rare_bug_seq)@[last_move])
       "Extremely rare bug. A pawn is advanced to the final row and is upgraded\
        to a queen which puts the other king in checkmate.");
    (result_manual_test (Stop_Early ("Black wins.", true))
       [Command(Surrender White)]
       "White surrenders.");
    (result_manual_test (Stop_Early ("White wins.", true))
       [Command(Surrender Black)]
       "Black surrenders.");
    (result_manual_test (Stop_Early ("You cannot move into check!", false))
       (lazy_make_move_sequence ["E2 E4";"A7 A6";"D1 H5";"F7 F6"])
       "Black attempts to move pawn forward, but this will put them into \
        check so they should be stopped.");
    (result_manual_test (Stop_Early ("You may not castle out of check.", false))
       (lazy_make_move_sequence ["F2 F4"; "E7 E6"; "G2 G4"; "G7 G6";
                                 "G1 H3"; "A7 A6"; "F1 G2"; "D8 H4"; "E1 G1"])
       "White attempts to castle out of check, and should be stopped.");
    (result_manual_test (Stop_Early ("You cannot move into check!", false))
       (lazy_make_move_sequence ["E2 E4"; "F7 F5"; "E4 F5"; "E7 E5";
                                 "F2 F4"; "F8 C5"; "F5 F6"; "B7 B6"; "F6 F7"; "E8 E7";
                                 "A2 A3"; "E7 D6"; "A3 A4"; "D8 H4"; "F7 F8"])
          "White tries to advance pawn to final row but this will put them in check")
  ]

let tests  =  model_tests@result_manual_tests@manual_no_exn

let suite = "search  test suite" >::: tests

let _ = run_test_tt_main suite
