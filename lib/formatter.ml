module Peano = struct
  type zero = Z
  type 'n succ = S of 'n
end

module IdList = struct
  type (_, _) t =
    | [] : (_,Peano.zero) t
    | ( :: ) : 'a * (_, 'n) t -> (_, 'n Peano.succ) t
end

module FList = struct
  type _ t =
    | [] : Peano.zero t
    | Literal : string * 'n t -> 'n t
    | Hole : ('a -> string) * 'n t -> 'n Peano.succ t

  let ( ^ ) str fmt = Literal (str, fmt)
  let string fmt = Hole ((fun x -> x),fmt)
  let int fmt = Hole (string_of_int,fmt)
  let bool fmt = Hole (string_of_bool, fmt)
  let ( ^^ ) f x = f x
end

let mprintf fmt args =
  let rec aux : type n a. n FList.t * (a, n) IdList.t -> _ = function
    | FList.[], IdList.[] -> ""
    | FList.Literal (x, xs), args -> x ^ aux (xs, args)
    | FList.Hole (to_string,xs), IdList.(x :: args) -> (to_string x) ^ aux (xs, args)

  in
  aux (fmt, args)