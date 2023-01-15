(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)

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



let init_eratosthenes n =
 let rec init_eratosthenes x =
    if (>>) x n then
      []
    else
      x::init_eratosthenes (add_b x [0;0;1])
  in
  [0;0;2]::init_eratosthenes [0;1;1];;
(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  let rec init_eratosthenes x =
    if x = [0;0;1] then
      x::init_eratosthenes (add_b x [0;1])
    else if x = (add_b n [0;1]) then
      []
    else
      if (is_prime x) then
        x::(init_eratosthenes (add_b x [0;1]))
            else
              init_eratosthenes (add_b x [0;1])
           in init_eratosthenes [0;0;1];;

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file =
  match li with
    [] -> ()
  | a::b -> print_int (a+1);
            print_string (file);;


(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file =
  let oc = open_out file in
  let rec aux = function
      [] -> close_out oc
    | e::l -> Printf.fprintf oc "%d" e ;
              aux l
  in aux n;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c;;

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = [[1]](*
  let ic = open_in file in
  let try_read() =
    try Some(input_line ic) with End_of_file -> None in
  let rec loop() =
    match try_read() with
      Some s -> int_of_string s::(loop())
    | None -> close_in ic; []
  in loop();;*)

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)



let double_primes limit isprime =
  let rec doubles_primes_rec list =
    match list with
      [] -> []
    | a::b -> if is_prime (add_b (mult_b a [0;0;1]) [0;1]) then
                (a, (add_b (mult_b a [0;0;1]) [0;1]))::doubles_primes_rec b
              else
                doubles_primes_rec b
            in doubles_primes_rec (eratosthenes limit);;



(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec doubles_primes_rec list =
    match list with
      [] -> []
    | a::b -> if is_prime (add_b a [0;0;1]) then
                (a,(add_b a [0;0;1]))::doubles_primes_rec b
              else
                doubles_primes_rec b
  in doubles_primes_rec (eratosthenes limit);;
