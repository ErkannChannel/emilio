(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)

let keys p q =
  let rec key_rec keys =
    match q with
    | q when mod_b q keys = [0] -> keys
    | _ -> key_rec (diff_b keys [0;0;1])
  in if mod_b p [0;0;1] = [0] then
       key_rec (diff_b p [0;1])
     else
       key_rec p;;

let break key =
  match key with
    (a,b) -> let x = keys (from_int(int_of_float(sqrt (float_of_int (to_int a))))) a  in
             (x,(quot_b a x));;
