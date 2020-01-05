type color = Black | White
type piece' = King | Queen | Rook | Bishop | Knight | Pawn
type piece = Piece of piece'*color| Nothing
type pos_rep = Taken of piece | Empty of color

(** type [column] represents one file of the board*)
type column = piece*piece*piece*piece*piece*piece*piece*piece

(** the type [moves_that_matter] is used to keep track of whether
    certain pieces have moved so far, as this is necessary when determining
    whether a castle or en passant move is allowed. Specifically, [wk]
    (respectively [bk]) stores whether the white (respectively black) king
    has moved so far.
    [llr], [lrr], [ulr], and [urr] refer to the lower left rook, lower right
    rook, upper left rook, and upper right rook, and store whether each of these
    pieces has moved so far if the board is viewed as the white player sees it.
    In all cases the variable is set to [true] at the beginning of the game and
    becomes [false] the first time the piece moves.
    [w2p] (respectively [b2p]) is [None] if white (respectively black) has not
    moved a pawn two spaces in the last move, and [Some f] if the pawn that
    of the associated color was moved two spaces in file [f].
*)
type moves_that_matter =
  {wk : bool; bk : bool; llr : bool; lrr : bool;
   ulr : bool; urr : bool; w2p:char option; b2p:char option}

type taken = piece list

type t = column*column*column*column*column*column*column*
         column*moves_that_matter*taken
type idx = char*int
type move = (idx*idx)*color

(** [comp_color c] is [Black] if [c] is [White] and [White] if [c] is [Black]*)
let comp_color = function
  | White->Black
  | Black->White

exception Pawn_Upgrade of t*color*idx
exception Win of t*color
exception Bad_Move of string

let valid_idx = function
  | (c,r) -> let c_num = Char.code c in
    c_num >= 65 && c_num <= 72 && r > 0 && r <= 8

let make_idx c i : idx =
  if valid_idx (c,i)
  then (c,i)
  else failwith "invalid idx"

(** [get_mtm board] gives the data stored in the [moves_that_matter] type
    for the board [board]*)
let get_mtm : (t -> moves_that_matter) = function
  | (_,_,_,_,_,_,_,_,mtm,taken) -> mtm

(** A helper function for [get_piece], [get_piece_from_col col r] gives
    the piece in position [r] of the column [col].*)
let get_piece_from_col (col:column) r =
  match col with
  | (a,b,c,d,e,f,g,h) ->
    match r with
    | 1 -> a
    | 2 -> b
    | 3 -> c
    | 4 -> d
    | 5 -> e
    | 6 -> f
    | 7 -> g
    | 8 -> h
    | _ -> failwith "failed to detect invalid idx"

let get_piece board idx =
  match board with
  | (a, b, c, d, e, f, g, h, mtm,taken) ->
    begin
      match idx with
      | (col,r) -> match col with
        |'A' -> get_piece_from_col a r
        |'B' -> get_piece_from_col b r
        |'C' -> get_piece_from_col c r
        |'D' -> get_piece_from_col d r
        |'E' -> get_piece_from_col e r
        |'F' -> get_piece_from_col f r
        |'G' -> get_piece_from_col g r
        |'H' -> get_piece_from_col h r
        |_ -> failwith "failed to detect invalid idx"
    end

(** [update_col col row piece] returns a copy of [col] but with entry
    [row] replaced with [piece].*)
let update_col (col:column) row piece : column = 
  match col with
  | (p1, p2, p3, p4, p5, p6, p7, p8) ->
    begin
      match row with
      | 1 -> (piece, p2, p3, p4, p5, p6, p7, p8)
      | 2 -> (p1, piece, p3, p4, p5, p6, p7, p8)
      | 3 -> (p1, p2, piece, p4, p5, p6, p7, p8)
      | 4 -> (p1, p2, p3, piece, p5, p6, p7, p8)
      | 5 -> (p1, p2, p3, p4, piece, p6, p7, p8)
      | 6 -> (p1, p2, p3, p4, p5, piece, p7, p8)
      | 7 -> (p1, p2, p3, p4, p5, p6, piece, p8)
      | 8 -> (p1, p2, p3, p4, p5, p6, p7, piece)
      | _ -> failwith "Failed to detect valid idx"
    end

(** [set_piece board idx piece] gives a copy of [board] with the piece
    at position [idx] replaced by [piece].*)
let set_piece (board:t) idx piece =
  match board with
  | (a, b, c, d, e, f, g, h,mtm,taken) ->
    begin
      match idx with
      | (col, r) ->
        begin
          match col with
          | 'A' -> (update_col a r piece, b, c, d, e, f, g, h)
          | 'B' -> (a, update_col b r piece, c, d, e, f, g, h)
          | 'C' -> (a, b, update_col c r piece, d, e, f, g, h)
          | 'D' -> (a, b, c, update_col d r piece, e, f, g, h)
          | 'E' -> (a, b, c, d, update_col e r piece, f, g, h)
          | 'F' -> (a, b, c, d, e, update_col f r piece, g, h)
          | 'G' -> (a, b, c, d, e, f, update_col g r piece, h)
          | 'H' -> (a, b, c, d, e, f, g, update_col h r piece)
          | _ -> failwith "unexpected runtime error"
        end
        |> (fun x -> match x with
              (a,b,c,d,e,f,g,h) -> (a,b,c,d,e,f,g,h,mtm,taken))
    end

(** [empty_square board idx] returns [true] if there is no piece at position
    [idx] on the board [board].*)
let empty_square (board:t) idx =
  match get_piece board idx with
  | Nothing -> true
  | _ -> false

let add_taken (board:t) piece =
  match board with
  | (a,b,c,d,e,f',g,h,mtm,taken) ->
    (a,b,c,d,e,f',g,h,mtm, piece::taken)

let find_and_add_taken idx board =
  if not (empty_square board idx)
  then add_taken board (get_piece board idx)
  else board

(** [naive_move board idx1 idx2] moves the piece currently at [idx1] to
    [idx2], overwriting any piece that was at [idx2]. It has no rule-checking.*)
let naive_move (board:t) idx1 idx2 : t =
  let board = find_and_add_taken idx2 board in
  let init_change = (set_piece board idx1 Nothing) in
  let start_piece = (get_piece board idx1) in
  set_piece init_change idx2 start_piece

(** [file_char_to_num] maps the ASCII characters A-H -> 1-8.
    For example [file_char_to_num 'A'] is [1]*)
let file_char_to_num chr = (Char.code chr) - 64

(** [num_to_char] is the inverse to [file_char_to_num].*)
let num_to_char num = Char.chr (num + 64)

(**[all_clear board squares] returns [true] iff all the squares in the board
   [board] referenced by an index in the list of indices [squares] are empty.*)
let all_clear board squares =
  List.fold_left
    (fun acc x ->
       if not acc then acc else empty_square board x)
    true squares

(** [seq s t] gives an inclusive list of numbers from [s] to [t],
    even if [s > t]*)
let seq s t = 
  let rec aux_seq_asc acc curr t =
    if curr >= t
    then acc
    else let curr'=curr+1 in
      aux_seq_asc (curr'::acc) curr' t
  in
  let rec aux_seq_desc acc curr s =
    if curr <= s then acc
    else let curr'=curr-1 in
      aux_seq_desc (curr'::acc) curr' s
  in
  if s <= t
  then aux_seq_asc [] (s-1) t
  else aux_seq_desc [] (s+1) t

(** [zip lst1 lst2] gives a list [lst] of tuples where the kth element
    of [lst] is the pair [(k1, k2)] where [k1] is the kth element of [lst1]
    and [k2] is the kth element of [lst2]. If the length of [lst1] and [lst2]
    is not equal an error is thrown.*)
let rec zip lst1 = function
  | [] ->
    begin
      match lst1 with
      | [] -> []
      | _ -> failwith "lists must be of equal length"
    end
  | h2::t2 ->
    begin
      match lst1 with
      | [] -> failwith "lists must be  of equal length"
      | h1::t1-> (h1,h2)::(zip t1 t2)
    end

(** [repeat n v] gives a list of length [n] where every element is [v]*)
let rec repeat n v =
  if n = 0 then [] else v::(repeat (n-1) v)

(** [zip_constant lst1 v]*) 
let zip_constant lst1 v =
  zip lst1 (repeat (List.length lst1) v)

(*given a start and end point marked by idx1 and ix2 determines if
  they form a diagonal line which contains no pieces
  except it does not check idx1 or idx2;
  also rejects if [idx1] is the same as [idx2].*)
let clear_diag board idx1 idx2 =
  match idx1 with
    (sf',sr) -> 
    match idx2 with
    |(tf',tr) ->
      let sf = file_char_to_num sf' in
      let tf = file_char_to_num tf' in
      let is_diag = (abs (sf - tf) = abs (sr - tr)) in
      if (not is_diag) || idx1 = idx2 then false
      else
        let make_diag_to_check tf sf tr sr =
          (zip (seq sf tf) (seq sr tr))
          |> List.filter
            (fun x -> (not (x = (sf,sr))) && (not (x = (tf,tr))))
        in
        let make_check tf sf tr sr =
          List.map (fun (n,i) -> (num_to_char n, i))
            (make_diag_to_check tf sf tr sr)
          |> all_clear board
        in make_check tf sf tr sr

(**[clear_line board idx1 idx2]  *)
let clear_line board idx1 idx2  =
  match idx1 with
    (sf',sr) ->
    match idx2 with
    |(tf',tr) ->
      let sf = file_char_to_num sf' in
      let tf = file_char_to_num tf' in
      let make_row s t v = zip_constant (seq s t) v in
      let make_check s t v switch =
        let to_check' = make_row s t v in
        let to_check =
          if not switch
          then to_check'
          else List.map (fun x -> match x with (u,v) -> (v,u)) to_check'
        in
        List.map (fun (n,i) -> (num_to_char n, i)) to_check
        |> List.filter (fun x -> (not (x = idx1)) && (not (x = idx2)))
        |> all_clear board
      in
      if sf = tf then make_check sr tr sf true
      else if sr = tr then make_check sf tf sr false 
      else false

let knight_move board idx1 idx2 =
  match idx1 with
    (sf',sr) ->
    begin
      match idx2 with
      |(tf',tr) ->
        let sf = file_char_to_num sf' in
        let tf = file_char_to_num tf' in
        let is_type_1 = abs (tf - sf) = 2 && abs (tr - sr) = 1 in
        let is_type_2 = abs (tf - sf) = 1 && abs (tr - sr) = 2 in
        if is_type_1 || is_type_2
        then naive_move board idx1 idx2
        else raise (Bad_Move "Knight's definitely don't work like that.")
    end

let bishop_move board idx1 idx2 =
  if clear_diag board idx1 idx2
  then naive_move board idx1 idx2
  else raise (Bad_Move "That ain't how bishops work, son.")

let change_mtm (board:t) f =
  match board with
  | (a,b,c,d,e,f',g,h,mtm,taken) ->
    (a,b,c,d,e,f',g,h,f mtm,taken)

let get_taken : t -> taken = function
  | (a,b,c,d,e,f',g,h,mtm,taken) -> taken

let moved_white_king = function
  | {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p} ->
    {wk=false;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p}

let moved_black_king = function
  | {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p} ->
    {wk=wk;bk=false;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p}

let moved_llr = function
  | {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p} ->
    {wk=wk;bk=bk;llr=false;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p}

let moved_ulr = function
  | {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p} ->
    {wk=wk;bk=bk;llr=llr;ulr=false;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p}

let moved_urr = function
  | {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p} ->
    {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=false;w2p=w2p;b2p=b2p}

let moved_lrr = function
  | {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p} ->
    {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=false;urr=urr;w2p=w2p;b2p=b2p}

let moved_pawn_two_white file = function
  | {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p} ->
    {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=Some file;b2p=b2p}

let moved_pawn_two_black file = function
  | {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p} ->
    {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=Some file}

let clear_w2p = function
  | {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p} ->
    {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=None;b2p=b2p}

let clear_b2p = function
  | {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=b2p} ->
    {wk=wk;bk=bk;llr=llr;ulr=ulr;lrr=lrr;urr=urr;w2p=w2p;b2p=None}

let rook_move board idx1 idx2 = 
  if clear_line board idx1 idx2
  then naive_move board idx1 idx2
       |> begin
         fun b ->
           match idx1 with
           | ('H',8) -> change_mtm b moved_urr
           | ('A',8) -> change_mtm b moved_ulr
           | ('A',1) -> change_mtm b moved_llr
           | ('H',1) -> change_mtm b moved_lrr
           | _ -> b
       end
  else raise (Bad_Move "Sorry, that is not how rooks work.")

exception Unexpected_move

let get_w2p = function
  | (_,_,_,_,_,_,_,_,{w2p=w2p},_) -> w2p

let get_b2p = function
  | (_,_,_,_,_,_,_,_,{b2p=b2p},_) -> b2p


let black_en_passant color (board:t) idx1 idx2 r2 f2= 
  if r2 = 3 then match get_w2p board with
    | None -> raise (Bad_Move "")
    | Some f ->
      if not (f2 = f)
      then raise (Bad_Move "")
      else
        let board = find_and_add_taken (f,4) board in
        naive_move board idx1 idx2
        |> (fun b -> set_piece b (f,4) Nothing)
  else raise (Bad_Move "")


let white_en_passant color (board:t) idx1 idx2 r2 f2 = 
  if r2 = 6
  then
    match get_b2p board with
    | None -> raise (Bad_Move "")
    | Some f ->
      if not (f2 = f)
      then raise (Bad_Move "")
      else
        let board = find_and_add_taken (f,5) board in
        naive_move board idx1 idx2
        |> (fun b -> set_piece b (f,5) Nothing)
  else raise (Bad_Move "")

let en_passant color (board:t) idx1 idx2 =
  match idx2 with (f2,r2) ->
  match color with
  | Black -> black_en_passant color board idx1 idx2 r2 f2
  | White -> white_en_passant color board idx1 idx2 r2 f2

let fail_msg_pawn _ =
  raise (Bad_Move "Pawns don't work like that (I'm pretty sure).")


let basic_pawn_move board idx1 idx2 =
  if clear_line board idx1 idx2 && empty_square board idx2
  then naive_move board idx1 idx2
  else fail_msg_pawn ()


let move_forward_one color f1 f2 t s =
  match color with
  | White -> f1=f2 && (t - s = 1)
  | Black -> f1=f2 && (s - t = 1)


let move_two color f1 f2 t s =
  match color with
  | White -> f1=f2 && (t - s = 2 && s = 2) 
  | Black -> f1=f2 && (s - t  = 2 && s = 7)


let attack_move color f1 f2 t s =
  match color with
  | White ->
    ((t - s = 1) && abs((file_char_to_num f1)-(file_char_to_num f2)) = 1)
  | Black ->
    ((s - t = 1) && abs((file_char_to_num f1)-(file_char_to_num f2)) = 1)


let pawn_attack_move board color idx1 idx2 = 
  (if not (empty_square board idx2)
   then naive_move board idx1 idx2
   else try en_passant color board idx1 idx2 with
     | Bad_Move _ -> fail_msg_pawn ())

let pawn_move' color board idx1 idx2 =
  match idx1 with
  | (f1,s) ->
    match idx2 with
      (f2,t) ->
      match color with
      | White when move_forward_one White f1 f2 t s -> 
        basic_pawn_move board idx1 idx2
      | White when move_two White f1 f2 t s -> 
        basic_pawn_move board idx1 idx2 |>
        (fun b -> change_mtm b (moved_pawn_two_white f1))
      | Black when move_forward_one Black f1 f2 t s -> 
        basic_pawn_move board idx1 idx2
      | Black when move_two Black f1 f2 t s -> 
        basic_pawn_move board idx1 idx2 |> (
          fun b -> change_mtm b (moved_pawn_two_black f1))
      | White when attack_move White f1 f2 t s -> 
        pawn_attack_move board color idx1 idx2
      | Black when attack_move Black f1 f2 t s -> 
        pawn_attack_move board color idx1 idx2
      |  _ -> fail_msg_pawn ()


let queen_move board idx1 idx2 =
  let has_line = clear_line board idx1 idx2 in
  let has_diag = clear_diag board idx1 idx2 in
  if has_line || has_diag
  then naive_move board idx1 idx2
  else raise
      (Bad_Move "The queen can do a lot of things. \
                 Unfortunately, that is not one of them.")

let check_target board color idx =
  if empty_square board idx then true
  else match get_piece board idx with
    | Piece (_,c) -> if c = color then false else true
    | Nothing  -> failwith "unexpected runtime error"

let color_of_idx = function
  | (c,i) -> let row = (Char.code c - 64) in
    if (row + i) mod 2 = 0
    then Black
    else White

let indices =
  let f x = List.fold_left (fun acc y -> (x,y)::acc) [] [1;2;3;4;5;6;7;8] in
  (List.map f ['A';'B';'C';'D';'E';'F';'G';'H'])
  |> (List.fold_left (fun acc x -> x@acc) [])

let castle_out_of_check _ =
  raise (Bad_Move "You may not castle out of check.") 

let castle_blocked _ =
  raise (Bad_Move "There cannot be any pieces in the way to castle.")

let castle_with_moved_king _ =
  raise (Bad_Move "You have already moved your king")

(* the mutual recursion is required because certain moves need to check if
   the player is currently in check, but our functions for this require
   being able to try moves*)
let rec make_move' (board:t) = function
  | ((idx1,idx2),current_color) ->
    match (get_piece board idx1) with
    | Nothing -> raise (Bad_Move "There is no piece at that location.")
    | Piece (piece,piece_color) ->
      let aux_move _ =
        if current_color = piece_color then
          if check_target board current_color idx2 then
            match piece with
            | Pawn -> pawn_move current_color board idx1 idx2 
            | Knight -> knight_move board idx1 idx2
            | Bishop -> bishop_move board idx1 idx2
            | Rook -> rook_move board idx1 idx2
            | Queen -> queen_move board idx1 idx2
            | King -> king_move board idx1 idx2 current_color
          else raise (Bad_Move "You cannot take  your own pieces.")
        else raise (Bad_Move "You cannot move the opponent's pieces.")
      in
      aux_move ()
      |> (fun b -> match current_color with
          | White -> change_mtm b clear_b2p
          | Black -> change_mtm b clear_w2p)

and pawn_move c b i1 = function
  | (f2,r2) as i2 ->
    let result = pawn_move' c b i1 i2 in
    let final_row : color -> bool = function
      | White -> r2 = 8
      | Black -> r2 = 1
    in
    if final_row c then
      if not (in_check result c) then 
      raise (Pawn_Upgrade (result,c,i2))
      else result
    else
      result

and find_white_king board =
  let find_idx_white = List.fold_left 
      (fun acc x -> match get_piece board x with 
         | Piece (King,White) -> Some x
         | _ -> acc)
      None indices
  in
  match find_idx_white with
  | Some x -> x
  | _ -> failwith "no white king on board"

and find_black_king board =
  let find_idx_black = List.fold_left 
      (fun acc x -> match get_piece board x with
         | Piece (King,Black) -> Some x
         | _ -> acc)
      None indices
  in
  match find_idx_black with
  | Some x -> x
  | _ -> failwith "no black king on board"

and in_check (board:t) color =
  let check_indices = List.filter
      (fun x -> not (get_piece board x = Piece (King,color))) indices
  in
  let target_idx =
    if color = White then find_white_king board
    else find_black_king board
  in
  List.fold_left (fun acc x ->
      try let _ = make_move' board ((x,target_idx),comp_color color) in true
      with
      | Pawn_Upgrade _ -> true
      | Bad_Move _ -> acc
      | _ -> failwith "unexpected runtime error")
    false check_indices

and black_castle_help_urr board idx1 idx2 color urr =
  if urr then
    if clear_line board idx1 ('H',8) then
      if not (in_check board color) then
        naive_move board idx1 idx2
        |> (fun b -> naive_move b ('H',8) ('F',8))
      else castle_out_of_check ()
    else castle_blocked ()
  else raise (Bad_Move "You have already moved your left rook.")

and black_castle_help_ulr board idx1 idx2 color ulr =
  if ulr then
    if clear_line board idx1 ('A',8) then
      if not (in_check board color) then
        naive_move board idx1 idx2
        |> (fun b -> naive_move b ('A',8) ('D',8))
      else castle_out_of_check ()
    else castle_blocked ()
  else raise (Bad_Move "You have already moved your right rook.")

and black_castle_help board idx1 idx2 color =
  match get_mtm board with
  | {bk=bk;ulr=ulr;urr=urr} ->
    if bk then
      if idx2 = ('G',8) then
        black_castle_help_urr board idx1 idx2 color urr
      else
      if idx2 = ('C',8) then
        black_castle_help_ulr board idx1 idx2 color ulr
      else raise Unexpected_move
    else castle_with_moved_king ()

and white_castle_help_lrr board idx1 idx2 color lrr =
  if lrr then
    if clear_line board idx1 ('H',1) then
      if not (in_check board color) then
        naive_move board idx1 idx2
        |> (fun b -> naive_move b ('H',1) ('F',1))
      else castle_out_of_check ()
    else castle_blocked ()
  else raise (Bad_Move "You have already moved your right rook.")

and white_castle_help_llr board idx1 idx2 color llr =
  if llr then
    if clear_line board idx1 ('A',1) then
      if not (in_check board color) then
        naive_move board idx1 idx2
        |> (fun b -> naive_move b ('A',1) ('D',1))
      else castle_out_of_check ()
    else castle_blocked ()
  else raise (Bad_Move "You have already moved your left rook.")

and white_castle_help board idx1 idx2 color =
  match get_mtm board with
  | {wk=wk;llr=llr;lrr=lrr} ->
    if wk then
      if idx2 = ('G',1) then
        white_castle_help_lrr board idx1 idx2 color lrr
      else
      if idx2 = ('C',1) then
        white_castle_help_llr board idx1 idx2 color llr
      else raise Unexpected_move
    else castle_with_moved_king ()

(**Executes a castle move if valid.
   Requires: [idx1] is the index of the current position of a king*)
and castle_move board idx1 idx2 color =
  match get_piece board idx1 with
  | Piece (King,c) -> (
      if c = White then
        white_castle_help board idx1 idx2 color
      else black_castle_help board idx1 idx2 color
    )
  | _ -> failwith "unexpected runtime error"

and king_move board idx1 idx2 color =
  let fail_msg _ = raise (Bad_Move "The king cannot do that.") in
  match idx1 with
  | (sf',sr) ->
    match idx2 with
    |(tf',tr) ->
      let sf = file_char_to_num sf' in
      let tf = file_char_to_num tf' in
      if idx1 = idx2 then fail_msg ()
      else if abs(sf-tf) <= 1 && abs(sr-tr) <= 1 then
        naive_move board idx1 idx2
      else try castle_move board idx1 idx2 color with
        | Unexpected_move -> fail_msg ()

let square_lst lst =
  List.fold_left (fun acc x -> (zip_constant lst x)@acc) [] lst

let achieved_mate (board:t) color =
  let other_color = comp_color color in
  if in_check board (other_color) then
    (List.fold_left (fun acc test_mv ->
         if not acc then false else
           try (in_check (make_move' board (test_mv, comp_color color))
                  (other_color))
           with
           | Bad_Move _ -> true)
        true (square_lst indices))
  else false


let make_move board cmd =
  match cmd with (_,color) ->
    make_move' board cmd
    |> (fun b -> if in_check b (match cmd with (_,c) -> c) then
          raise (Bad_Move "You cannot move into check!")
        else b)
    |> (fun b -> if achieved_mate b color then raise (Win (b,color)) else b)

let color_of_idx = function
  | (c,i) -> let row = (Char.code c - 64) in
    if (row + i) mod 2 = 0
    then Black
    else White

let start_board = (
  (Piece(Rook,White),Piece(Pawn,White),
   Nothing,Nothing,Nothing,Nothing,
   Piece(Pawn,Black),Piece(Rook,Black)),
  (Piece(Knight,White),Piece(Pawn,White),
   Nothing,Nothing,Nothing,Nothing,
   Piece(Pawn,Black),Piece(Knight,Black)),
  (Piece(Bishop,White),Piece(Pawn,White),
   Nothing,Nothing,Nothing,Nothing,
   Piece(Pawn,Black),Piece(Bishop,Black)),
  (Piece(Queen,White),Piece(Pawn,White),
   Nothing,Nothing,Nothing,Nothing,
   Piece(Pawn,Black),Piece(Queen,Black)),
  (Piece(King,White),Piece(Pawn,White),
   Nothing,Nothing,Nothing,Nothing,
   Piece(Pawn,Black),Piece(King,Black)),
  (Piece(Bishop,White),Piece(Pawn,White),
   Nothing,Nothing,Nothing,Nothing,
   Piece(Pawn,Black),Piece(Bishop,Black)),
  (Piece(Knight,White),Piece(Pawn,White),
   Nothing,Nothing,Nothing,Nothing,
   Piece(Pawn,Black),Piece(Knight,Black)),
  (Piece(Rook,White),Piece(Pawn,White),
   Nothing,Nothing,Nothing,Nothing,
   Piece(Pawn,Black),Piece(Rook,Black)),
  {wk=true;bk=true;llr=true;lrr=true;ulr=true;urr=true;w2p=None;b2p=None},
  []
)

let board_view (board:t) =
  let rep_of_pos idx = function
    | Piece (p',c) -> Taken (Piece (p',c))
    | Nothing -> Empty (color_of_idx idx)
  in
  let string_of_row r =
    let idxs = List.fold_left (fun acc x -> (x,r)::acc) []
        ['A';'B';'C';'D';'E';'F';'G';'H']
    in
    List.fold_left
      (fun acc x -> (get_piece board x |> rep_of_pos x)::acc) [] idxs
  in
  (List.fold_left (fun acc x -> (string_of_row x)::acc) 
     [] [1;2;3;4;5;6;7;8], get_taken board)

