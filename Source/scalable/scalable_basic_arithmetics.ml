(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)

open Scalable

(*#use "scalable.ml";;*)
(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
**)



let rec gcd_b bA bB =
  if bA = [0] then
    bB
  else if bB = [0] then
    bA
  else
    let r = mod_b bA bB  in
       gcd_b r bA;;



(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)


let rec bezout_b bA bB =
  let rec bezout_rec bA bB u v u' v' =
    if bB = [0] then
      (u,v,bA)
    else
      let q = quot_b bA bB in
      bezout_rec bB (diff_b bA (mult_b q bB)) u' v' (diff_b u (mult_b q u')) (diff_b v (mult_b q v'))
  in bezout_rec bA bB [0;1] [0;0] [0;0] [0;1];;




