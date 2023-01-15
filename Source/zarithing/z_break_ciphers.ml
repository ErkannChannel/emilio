(** Factoring big integers into primes *)

open Z

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)



let keys p q =
  let rec key_rec keys =
    match q with
    | q when rem q keys = zero -> keys
    | _ -> key_rec (sub keys (of_int 2))
  in if rem p (of_int 2) = zero then
       key_rec (pred p)
     else
       key_rec p;;



let break key =
  match key with
    (a,b) -> let x = keys (sqrt a) a  in
             (x, (div a x));;
