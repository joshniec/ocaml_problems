(* https://ocaml.org/problems#1 *)
let rec last (l : 'a list) : 'a option =
  match l with [] -> None | [ x ] -> Some x | _ :: rest -> last rest

let%test "empty list" = last [] = None
let%test "populated list" = last [ 1; 2; 3 ] = Some 3
