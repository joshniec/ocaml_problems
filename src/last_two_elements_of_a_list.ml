(* https://ocaml.org/problems#2 *)
let rec last_two (l : 'a list) : ('a * 'a) option =
  match l with
  | [] | [ _ ] -> None
  | [x;y] -> Some (x, y)
  | _ :: rest -> last_two rest

let%test "empty list" = last_two [] = None
let%test "single item list" = last_two [1] = None
let%test "three item list" = last_two [ 1; 2; 3 ] = Some (2, 3)
