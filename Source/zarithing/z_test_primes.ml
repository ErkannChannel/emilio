(** Testing for primality *)

open Z
open Z_power

(** Deterministic primality test
    @param n a big integer bigger or equal to 2.
 *)

let is_prime n =
  let rec is_prime_rec n m =
    if n = m then
      true
    else
      if rem m n = zero then
        false
      else
        is_prime_rec (succ n) m
    in is_prime_rec (of_int 2) n;;
