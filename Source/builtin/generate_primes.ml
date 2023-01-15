(** Generating primes *)



open Builtin
open Basic_arithmetics




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




let init_eratosthenes n =
  let rec init_eratosthenes x=
    if x > n then
      []
    else
      x::init_eratosthenes (x+2)
  in
  2::init_eratosthenes 3;;


let eratosthenes n =
  let rec init_eratosthenes x =
    if x = 2 then
      x::init_eratosthenes (x+1)
    else if x = (n+1) then
      []
    else
      if (is_prime x) then
        x::(init_eratosthenes (x+1))
            else
              init_eratosthenes (x+1)
           in init_eratosthenes 2;;


let write_list_primes n file =
  let oc = open_out file in
  let rec aux = function
      [] -> close_out oc
    | e::l -> Printf.fprintf oc "%d" e ;
              aux l
  in aux (eratosthenes n);;




let write_list list str =
  match list with
    [] -> ()
  | a::b -> print_int (a+1);
            print_string (str);;






let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None;;

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




let read_list_primes file =
  let ic = open_in file in
  let try_read() =
    try Some(input_line ic) with End_of_file -> None in
  let rec loop() =
    match try_read() with
      Some s -> int_of_string s::(loop())
    | None -> close_in ic; []
  in loop();;






(** Get biggest prime.
    @param l list of prime numbers.
 *)


let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t;;

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t;;




let double_primes limit isprime =
  let rec doubles_primes_rec list =
    match list with
      [] -> []
    | a::b -> if is_prime (a*2+1) then
                (a, a*2+1)::doubles_primes_rec b
              else
                doubles_primes_rec b
            in doubles_primes_rec (eratosthenes limit);;




let twin_primes limit isprime =
  let rec doubles_primes_rec list =
    match list with
      [] -> []
    | a::b -> if is_prime (a+2) then
                (a, a+2)::doubles_primes_rec b
              else
                doubles_primes_rec b
  in doubles_primes_rec (eratosthenes limit);;



