
open Builtin
open Basic_arithmetics
open Power




let is_prime n =
  let rec is_prime_rec n m =
    if n = m then
      true
    else
      if m mod n = 0 then
        false
      else
        is_prime_rec (n+1) m
    in is_prime_rec 2 n;;






let is_pseudo_prime p test_seq =
  let rec is_pseudo_prime_rec test_seq =
    match test_seq with
      [] -> true
    | a::b -> if mod_power a p p = a then
                is_pseudo_prime_rec b
              else if mod_power a (p-1) p = 1 then
                is_pseudo_prime_rec b
              else if mod_power a p p = 0 then
                is_pseudo_prime_rec b
              else
                false
  in is_pseudo_prime_rec test_seq;;






