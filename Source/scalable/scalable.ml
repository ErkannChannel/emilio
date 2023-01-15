(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

*)

(*-------------------------------------------------*)

let invert list =
  let rec invert_list list nlist=
    match list with
      [] -> nlist
    | a::b -> (invert_list b (a::nlist))
  in invert_list list [];;




let from_int x =
  let rec from_int_rec x y list =
    match x with
      0 -> if y>=0 then
          0::(invert list)
        else
          1::(invert list)
    | x-> from_int_rec (x/2) y ((x mod 2)::list)
  in from_int_rec (abs(x)) (x) [];;





(*-------------------------------------------------*)


let pow x n =
  let rec pow_rec n =
    if n <> 0 then
      x*(pow_rec (n-1))
    else
      1
  in pow_rec n;;




let rec to_int_rec lim list d x =
  match list with
    [] -> d*x
  | a::b -> if a = 1 then
        to_int_rec (lim+1) b (d+(pow 2 lim)) x
      else
        to_int_rec (lim+1) b d x;;



let to_int bA =
  let lim = 0 in
  match bA with
    [] -> 0
  | a::b -> if a = 1 then
        to_int_rec lim b 0 (-1)
      else
        to_int_rec lim b 0 1;;




(*-------------------------------------------------*)



let print_b bA =
  print_int(to_int bA);;




(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
*)

let rec length x =
  match x with
    [] -> 0
  | a::b -> length b + 1;;


(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
*)

let compare_n nA nB =
  let taille_nA = length nA in
  let taille_nB = length nB in
  let nA = invert nA in
  let nB = invert nB in
  let rec compare_n_rec nA nB =
    match (nA, nB) with
      ([],[]) -> 0
    | ([],_) -> -1
    | (_,[]) -> 1
    | (a::b,c::d) when taille_nA = taille_nB -> if a=1 && c=1 || a = 0 && c = 0 then
                                                  compare_n_rec b d
                                                else if a=0 && c=1 then
                                                  -1
                                                else if a=1 && c=0 then
                                                  1
                                                else
                                                  0
    | (a::b,c::d) -> compare_n_rec b d
                   in compare_n_rec nA nB;;



(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)



let (>>!) nA nB =
  if compare_n nA nB = 1 then
    true
  else
    false;;







(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<<!) nA nB =
  if compare_n nA nB = -1 then
    true
  else
    false;;


(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let (>=!) nA nB =
  let x = compare_n nA nB in
  if x = 1 || x = 0 then
    true
  else
    false;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<=!) nA nB =
  let x = compare_n nA nB in
  if x = -1 || x = 0 then
    true
  else
    false;;







(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB =
  match (bA,bB) with
    ([],[]) | ([0],[]) | ([],[0]) -> 0
  | (_,[]) -> 1
  | ([],_) -> -1
  | (a::b,c::d) -> if a=0 && c=0 then
        compare_n b d
      else if a=1 && c=0 then
        -1
      else if a=0 && c=1 then
        1
      else
        (-1)*compare_n b d;;



(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)



let (>>) bA bB =
  if compare_b bA bB = 1 then
    true
  else
    false;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<<) bA bB =
  if compare_b bA bB = -1 then
    true
  else
    false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let (>>=) bA bB =
  let x = compare_b bA bB in
  if x = 1 || x = 0  then
    true
  else
    false;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<<=) bA bB =
  let x = compare_b bA bB in
  if x = -1 || x = 0  then
    true
  else
    false;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA =
  match bA with
    [] -> 0
  | a::b -> if a=0 then
        0
      else
        1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
  match bA with
    [] -> []
  | a::b -> if a=0 then
        bA
      else
        (a-1)::b;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a =
  if a < 2 then
    0
  else
    1;;

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a =
  if a = 1 || a = 3 then
    1
  else
    0;;

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a =
  (_quot_t a, _mod_t a);;

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
 *)

let enlev0 nA =
  let rec en0_rec list =
    match list with
      [] -> []
    | a::b -> if a = 0 then
                en0_rec b
              else
                invert (a::b)
  in en0_rec (invert nA);;





let add_n_supp nA nB =
  let rec add_n_rec nAb nBb k =
    match (nAb, nBb) with
      ([],[]) -> if k = 1 then
          [1]
        else
          []
    | ([],a::b) when a = 1 -> if k = 1 then
          0::add_n_rec [] b 1
        else
          1::add_n_rec [] b 0
    | ([],a::b) when a = 0 -> if k = 1 then
          1::add_n_rec [] b 0
        else
          0::add_n_rec [] b 0
    | (a::b,[]) when a = 1 -> if k = 1 then
          0::add_n_rec [] b 1
        else
          1::add_n_rec [] b 0
    | (a::b,[]) when a = 0 -> if k = 1 then
          1::add_n_rec [] b 0
        else
          0::add_n_rec [] b 0
    | (a::b, c::d) when (a = 1 && c = 0) ||  (a = 0 && c = 1) -> if k = 1 then
          0::add_n_rec b d 1
        else
          1::add_n_rec b d k
    | (a::b, c::d) when a = 0 && c = 0-> if k = 1 then
          1::add_n_rec b d 0
        else
          0::add_n_rec b d k
    | (a::b, c::d) when a = 1 && c = 1-> if k = 1 then
          1::add_n_rec b d 1
        else
          0::add_n_rec b d 1
    | _ -> []
  in add_n_rec nA nB 0;;

let add_n nA nB =
  enlev0 (add_n_supp nA nB);;



(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n_supp nA nB =
  let rec diff_n_rec nAb nBb k =
    match (nAb, nBb) with
      ([],[]) -> []
    | ([],a::b) -> []
    | (a::b,[]) when a = 1 -> if k = 1 then
                                0::diff_n_rec b [] 0
                              else
                                nAb
    | (a::b,[]) when a = 0 -> if k = 1 then
                                1::diff_n_rec b [] 1
                              else
                                nAb
    | (a::b,[]) when a = 0 -> if k = 1 then
                                1::diff_n_rec [] b 1
                              else
                                nAb
    | (a::b, c::d) when a = 1 && c = 0 -> if k = 1 then
                                            0::diff_n_rec b d 0
                                          else
                                            1::diff_n_rec b d 0
    | (a::b, c::d) when a = 0 && c = 1 -> if k = 1 then
                                            0::diff_n_rec b d 1
                                          else
                                            1::diff_n_rec b d 1
    | (a::b, c::d) when a = c -> if k = 1 then
                                   1::diff_n_rec b d 1
                                 else
                                   0::diff_n_rec b d k
    | _ -> []
  in diff_n_rec nA nB 0;;

let diff_n nA nB =
  enlev0 (diff_n_supp nA nB);;


(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let add_b bA bB =
  let xA = sign_b bA in
  let xB = sign_b bB in
  match (bA, bB) with
    ([],[]) -> []
  | ([],_) -> bB
  | (_,[]) -> bA
  | (a::b, c::d) when xA = 0 && xB = 0 -> 0::add_n b d
  | (a::b, c::d) when xA = 1 && xB = 1 -> 1::add_n b d
  | (a::b, c::d) when xA = 0 && xB = 1 && compare_n b d = 1-> 0::diff_n b d
  | (a::b, c::d) when xA = 0 && xB = 1 && compare_n b d = (-1) -> 1::diff_n d b
  | (a::b, c::d) when xA = 1 && xB = 0 && compare_n b d = 1 -> 1::diff_n b d
  | (a::b, c::d) when xA = 1 && xB = 0 && compare_n b d = (-1) -> 0::diff_n d b
  | _ -> [] ;;





(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)

let diff_b bA bB =
  let xA = sign_b bA in
  let xB = sign_b bB in
  match (bA, bB) with
    ([],[]) -> []
  | (a::b, c::d) when xA = 0 && xB = 0 && compare_b bB bA = 1 -> 1::diff_n d b
  | (a::b, c::d) when xA = 0 && xB = 0 -> 0::diff_n b d
  | (a::b, c::d) when xA = 1 && xB = 0 -> 1::add_n b d
  | (a::b, c::d) when xA <> xB -> xA::add_n b d
  | (a::b, c::d) when xA = 1 && xB = 1 -> if (>>=) bA bB = false then
                                            1::diff_n b d
                                          else
                                            0::diff_n d b
  | _ -> [] ;;



(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let shift bA d =
  match bA with
    [] -> bA
  | a::b -> a::(let rec shift_rec b d =
                  if d>0 then
                    0::shift_rec b (d-1)
                  else
                    bA
                in shift_rec b d);;


(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)

let rec mult_b_rec bA bB result =
  match bB with
    [] -> result
  | (a::b) -> if a = 1 then
        mult_b_rec (0::bA) b (add_n result bA)
      else
        mult_b_rec (0::bA) b result;;


let mult_b bA bB =
  if bA = [0] || bB = [0] then
    [0]
  else
    let xA = sign_b bA in
    let xB = sign_b bB in
    match (bA,bB) with
      (a::b, c::d) when xA <> xB -> 1::mult_b_rec b d []
    | (a::b, c::d) when xA = xB-> 0::mult_b_rec b d []
    | _ -> [];;





(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let rec quot_b_rec bA bB result =
  let x = diff_b bA bB in
  if ((>>=) bA bB) then
    quot_b_rec x bB (add_n result [1])
  else
    result;;

let quot_b bA bB =
  let xA = sign_b bA in
  let xB = sign_b bB in
  if bA = [0] then
    [0]
  else
    match (bA,bB) with
      (b, d) when  compare_n bB [0] = 0 -> invalid_arg "quotb : bB must be != 0"
    | (b, d) when xA = 0 && xB = 0 -> 0::quot_b_rec bA bB [0]
    | (b, d) when xA = 1 && xB = 0 -> 1::quot_b_rec (abs_b bA) bB [1]
    | (b, d) when xA = 0 && xB = 1 -> 1::quot_b_rec bA (abs_b bB) [0]
    | (b, d) when xA = 1 && xB = 1 -> 0::quot_b_rec (abs_b bA) (abs_b bB) [1]
    | _ -> [];;




(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)


let mod_b bA bB =
  if bA = [0] then
    [0]
  else let r = diff_b bA (mult_b bB (quot_b bA bB)) in
       if r = bB then
         diff_b r (abs_b bB)
       else
         r;;



(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
 *)
let div_b bA bB =
  (quot_b bA bB, mod_b bA bB);;

