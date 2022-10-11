type obliv_array = int array

let count = ref 0

(* All parties are public in plaintext driver. *)
let party_public = 0
let party_alice = 0
let party_bob = 0

let setup_driver _ _ party =
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
  count := !count + len;
  if b.(0) == 0
  then Array.sub a2 0 len
  else Array.sub a1 0 len

let obliv_int_add m n = Array.make 1 (m.(0) + n.(0))

let obliv_int_sub m n = Array.make 1 (m.(0) - n.(0))

let obliv_int_mul m n = Array.make 1 (m.(0) * n.(0))

let obliv_int_div m n = Array.make 1 (m.(0) / n.(0))

let obliv_int_le m n = Array.make 1 (Bool.to_int (m.(0) <= n.(0)))

let obliv_int_eq m n = Array.make 1 (Bool.to_int (m.(0) == n.(0)))

let s_int n = Array.make 1 n

let s_int_from n _ = s_int n

let private_s_int = s_int

let unsafe_r_int a = a.(0)

let unsafe_r_bool a = unsafe_r_int a <> 0

let unsafe_if a t e = if unsafe_r_bool a then t else e
