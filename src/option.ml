open Monad

module Option : Monad with type 'a t = 'a option = struct
  type 'a t = 'a option

  let return (x : 'a) : 'a t = Some x

  let ( >>= ) (x : 'a t) (f : 'a -> 'b t) : 'b t =
    match x with
    | None -> None
    | Some x -> f x
end
