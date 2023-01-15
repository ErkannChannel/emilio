(** Generating primes *)

open Z

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
   A light version done in-class.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n upper limit of elements in the list of big integers.
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



let init_eratosthenes n =
  let rec init_eratosthenes x=
    if gt x n then
      []
    else
      x::init_eratosthenes (add x (of_int 2))
  in
  ((of_int 2)::init_eratosthenes (of_int 3));;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
  let rec init_eratosthenes x =
    if x = (of_int 2) then
      x::init_eratosthenes (succ x)
    else if x = (succ n) then
      []
    else
      if (is_prime x) then
        x::(init_eratosthenes (succ x))
            else
              init_eratosthenes (succ x)
           in init_eratosthenes (of_int 2);;



(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file in
  let rec aux = function
      [] -> close_out oc
    | e::l -> output oc e;
              Printf.fprintf oc "\n";
              aux l
  in aux li;;



(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file =
 let oc = open_out file in
  let rec aux = function
      [] -> close_out oc
    | e::l -> output oc e;
              Printf.fprintf oc "\n";
              aux l
  in aux (eratosthenes n);;


(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c;;



(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let ic = open_in file in
  let try_read() =
    try Some(input_line ic) with End_of_file -> None in
  let rec loop() =
    match try_read() with
      Some s -> of_string(s)::(loop())
    | None -> close_in ic; []
  in loop();;







(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(* Generating couples of primes numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive big integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec doubles_primes_rec list =
    match list with
      [] -> []
    | a::b -> if is_prime (add (mul a (of_int 2)) one) then
                (a, add (mul a (of_int 2)) one)::doubles_primes_rec b
              else
                doubles_primes_rec b
            in doubles_primes_rec (eratosthenes limit);;
