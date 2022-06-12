(* https://ocaml.org/problems#3 *)
let rec elem (l : 'a list) (n : int) : 'a =
  match n with
  | 0 -> ( match l with [] -> failwith "Index out of bounds" | h :: _ -> h)
  | _ -> (
      match l with
      | [] -> failwith "Index out of bounds"
      | _ :: rest -> elem rest (n - 1))

(* How to test for failure? *)
(* let%test "empty list" = elem [] 2 = Failure "Index out of bounds" *)
let%test "valid list & idx" = elem [ 6; 5; 4; 3; 2; 1; 0 ] 2 = 4
