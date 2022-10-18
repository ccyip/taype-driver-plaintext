open Sexplib

type obliv_array = int array

let print_obliv_array a =
  Conv.sexp_of_array Conv.sexp_of_int a |> Sexp.to_string |> print_endline

let count = ref 0

let is_verbose = ref false

(* All parties are public in plaintext driver. *)
let party_public = 0
let party_alice = 0
let party_bob = 0

let setup_driver ?(verbose=false) _ _ party =
  is_verbose := verbose;
  if party <> party_public
  then raise (Failure "Unknown party: this driver only supports \
                       public party (0)")

let finalize_driver _ = ()

let collect_stat _ = count := 0

let report_stat _ = !count

let obliv_array_new n = Array.make n 0

let obliv_array_new_from n _ = obliv_array_new n

let private_obliv_array_new = obliv_array_new

let obliv_array_concat a1 a2 = Array.append a1 a2

let obliv_array_slice a pos len = Array.sub a pos len

let obliv_array_mux len b a1 a2 =
  if !is_verbose
  then begin
    print_string "MUX "; print_int len; print_string " ";
    print_int b.(0); print_newline ();
    print_string "  "; print_obliv_array a1;
    print_string "  "; print_obliv_array a2
  end;
  count := !count + len;
  if b.(0) == 0
  then Array.sub a2 0 len
  else Array.sub a1 0 len

let obliv_int_add m n = m.(0) + n.(0) |> Array.make 1

let obliv_int_sub m n = m.(0) - n.(0) |> Array.make 1

let obliv_int_mul m n = m.(0) * n.(0) |> Array.make 1

let obliv_int_div m n =
  (if n.(0) = 0 then (-1) else m.(0) / n.(0)) |> Array.make 1

let obliv_int_le m n = m.(0) <= n.(0) |> Bool.to_int |> Array.make 1

let obliv_int_eq m n = m.(0) = n.(0) |> Bool.to_int |> Array.make 1

let bool_of_int b = b <> 0

let obliv_bool_not a = not (bool_of_int a.(0)) |> Bool.to_int |> Array.make 1

let obliv_bool_and a b =
  (bool_of_int a.(0) && bool_of_int b.(0)) |> Bool.to_int |> Array.make 1

let obliv_bool_or a b =
  (bool_of_int a.(0) || bool_of_int b.(0)) |> Bool.to_int |> Array.make 1

let s_int n = Array.make 1 n

let s_int_from n _ = s_int n

let private_s_int = s_int

let unsafe_r_int a = a.(0)

let unsafe_r_bool a = unsafe_r_int a |> bool_of_int

let unsafe_if a t e = if unsafe_r_bool a then t else e
