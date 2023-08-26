module Peano = struct
  type zero = Z
  type 'n succ = S of 'n
end

module IdList = struct
  type ('a, _) t =
    | [] : (_,Peano.zero) t
    | ( :: ) : 'a * ('a, 'n) t -> ('a, 'n Peano.succ) t
end

module FList = struct
  type _ t =
    | [] : Peano.zero t
    | Literal : string * 'n t -> 'n t
    | Hole : 'n t -> 'n Peano.succ t

  let ( ^ ) str fmt = Literal (str, fmt)
  let hole fmt = Hole fmt
  let ( ^^ ) f x = f x
end

module Printable = struct
  type t = P : ('a * ('a -> string)) -> t
  let pack e f = P (e,f)
  let unpack (P(e,to_string)) = to_string e
end

let mprintf fmt args =
  let rec aux : type n. n FList.t * (Printable.t, n) IdList.t -> _ = function
    | FList.[], IdList.[] -> ""
    | FList.Literal (x, xs), args -> x ^ aux (xs, args)
    | FList.Hole xs, IdList.(x :: args) ->(Printable.unpack x) ^ aux (xs, args)

  in
  aux (fmt, args)