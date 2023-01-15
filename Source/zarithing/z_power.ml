(** Power function implementations for big integers *)

open Z


(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
    let result = (rem x m) in
    let rec mod_power_rec n result =
      if n = one then
        result
      else
        mod_power_rec (sub n one) (rem (mul x result) m)
    in
    if n = zero then
      one
    else
      mod_power_rec n result;;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  let result = (rem x p) in
    let rec mod_power_rec n result =
      if n = one then
        result
      else
        mod_power_rec (sub n one) (rem (mul x result) p)
    in
    if n = zero then
      one
    else
      mod_power_rec n result;;


