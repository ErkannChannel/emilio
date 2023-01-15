(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(*#use "scalable.ml";;
#use "scalable_power.ml";;*)


(** Deterministic primality test *)
let is_prime n =
  let rec is_prime_rec m n =
    if compare_b n m = 0 then
      true
    else
      if mod_b n m = [0] then
        false
      else
        is_prime_rec (add_b m [0;1]) n
    in is_prime_rec [0;0;1] n;;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarrayx
    @param testSeq sequence of bitarrays againt which to test
 *)

let is_pseudo_prime p test_seq =
  let rec is_pseudo_prime_rec test_seq =
    match test_seq with
      [] -> true
    | a::b -> if mod_power a p p = a then
                is_pseudo_prime_rec b
              else if mod_power a (diff_b p [0;1]) p = [0;1] then
                is_pseudo_prime_rec b
              else if mod_power a p p = [0] then
                is_pseudo_prime_rec b
              else
                false
  in is_pseudo_prime_rec test_seq;;


(*[0, 1, 1, 0, 1]  [[0, 0, 1], [0, 0, 0, 1], [0, 1, 0, 1], [0, 0, 0, 1, 0, 1]]*)




mod_b [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1] [0;1;1;0;1];;

