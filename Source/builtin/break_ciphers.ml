(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(*#use "basic_arithmetics.ml";;*)

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)

let keys p q =
  let rec key_rec keys =
    match q with
    | q when q mod keys = 0 -> keys
    | _ -> key_rec (keys-2)
  in if p mod 2 = 0 then
       key_rec (p-1)
     else
       key_rec p;;



let break key =
  match key with
    (a,b) -> let x = keys (int_of_float(sqrt (float_of_int a))) a  in
             (x,a/x);;


