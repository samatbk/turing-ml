type state = Start | Accept | Reject

type alphabet = One | Zero | Empty

type direction = Left | Right

type turing_machine = {pointer: int; tape: alphabet array; current_state: state}

(* Negate bit *)
let transition_fn state cell =
  match (state, cell) with
  | Start, One ->
      (Start, Zero, Right)
  | Start, Zero ->
      (Start, One, Right)
  | _ ->
      (Accept, cell, Left)

let turing =
  { pointer= 0
  ; tape= [|One; Zero; Zero; Zero; Empty; Empty; Empty; Empty; Empty; Empty|]
  ; current_state= Start }

let cell_to_char = function One -> '1' | Zero -> '0' | Empty -> ' '

let get_tape_content tape =
  Array.fold_left
    (fun acc cell -> acc ^ String.make 1 (cell_to_char cell))
    "" tape

let get_offset = function Left -> -1 | Right -> 1

let rec run tm =
  match tm.current_state with
  | Accept ->
      print_endline ("Accepted: " ^ get_tape_content tm.tape)
  | Reject ->
      print_endline "Rejected"
  | _ ->
      print_endline (get_tape_content tm.tape) ;
      let state, cell, direction =
        transition_fn tm.current_state tm.tape.(tm.pointer)
      in
      Array.set tm.tape tm.pointer cell ;
      let new_pointer = tm.pointer + get_offset direction in
      let bounded_pointer =
        if new_pointer < 0 then 0
        else if new_pointer >= Array.length tm.tape then
          Array.length tm.tape - 1
        else new_pointer
      in
      let updated_tm =
        {tm with pointer= bounded_pointer; current_state= state}
      in
      run updated_tm

let () = run turing
