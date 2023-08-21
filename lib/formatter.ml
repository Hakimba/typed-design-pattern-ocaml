module Peano = struct
  type zero = Z
  type 'n succ = S of 'n
end


module IdList = struct
  type _ element =
    | Str : string -> string element
    | Num : int -> int element
  
  type (_, _) t =
    | [] : (_, Peano.zero) t
    | ( :: ) : _ element * (_ element, 'n) t -> (_ element, 'n Peano.succ) t

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

let mprintf fmt args =
  let rec aux : type n a. n FList.t * (a IdList.element, n) IdList.t -> _ = function
    | FList.[], IdList.[] -> ""
    | FList.Literal (x, xs), args -> x ^ aux (xs, args)
    | FList.Hole xs, IdList.(x :: args) ->
      match x with
      | Str v -> v ^ aux (xs, args)
      | Num n -> (string_of_int n) ^ aux (xs, args)

  in
  aux (fmt, args)