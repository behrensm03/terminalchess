open Model
open Uchar

let string_of_piece = function
  | Piece (King,c) -> ("K", c)
  | Piece (Queen,c) -> ("Q",c)
  | Piece (Bishop,c) -> ("B",c)
  | Piece (Rook,c) -> ("R",c)
  | Piece (Knight,c) -> ("N",c)
  | Piece (Pawn,c) -> ("P",c)
  | _ -> failwith "unexpected run time error"

let empty_token = "-"

let get_mod_board b = 
  List.map (List.map 
              (fun x -> match x with
                 | Taken p -> string_of_piece p
                 | Empty c -> (empty_token, c))
           ) (Stdlib.fst (board_view b))

let get_taken_piece b c =
  List.map string_of_piece (Stdlib.snd (board_view b))
  |> List.filter (fun x ->
      match x with
      | (s,c') -> c = c')

let print_chess p unicode =
  if unicode then
    begin
      match p with
      | "K" -> "♔ "
      | "Q" -> "♕ "
      | "B" -> "♗ "
      | "R" -> "♖ "
      | "N" -> "♘ "
      | "P" -> "♙ "
      | _ -> "- "
    end
  else p^" "


let test_unicode _ =
  let _ =
    print_string "♔ ";
    List.map (fun x ->
        print_string ", ";
        print_string x
      )
      ["♕";"♗";"♖";"♘";"♙"]
  in
  print_endline ""; ()

let f color board ucode = List.map (fun (s,c) ->
    match c with 
    | Model.Black ->
      ANSITerminal.(print_string [red] (print_chess s ucode))
    | Model.White ->
      ANSITerminal.(print_string [white] (print_chess s ucode))
  ) (get_taken_piece board color)

let g board counter ucode = 
  List.map (fun x -> 
      print_string(string_of_int (!counter) ^ " ");
      counter := !counter - 1;
      (let _ = List.map (fun (s,c) ->
           match c with 
           | Model.Black ->
             ANSITerminal.(print_string [red] (print_chess s ucode))
           | Model.White ->
             ANSITerminal.(print_string [white] (print_chess s ucode))
         ) x 
       in ());
      print_endline "")
    (get_mod_board board)

let print_board curr_player_in_check unicode board = 
  print_endline("Pieces taken:");
  let _ = f Black board unicode in
  print_endline "";
  let _ = f White board unicode in
  print_endline "";
  print_endline "-----------------";
  let _ =
    let counter = ref 8 in
    g board counter unicode 
  in 
  print_endline("  A B C D E F G H");
  print_endline("");
  if curr_player_in_check
  then print_endline("Warning: You are currently in check.")
  else ()

