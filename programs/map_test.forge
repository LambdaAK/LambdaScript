type Option<a> =
  | None
  | Some of a

let map f o =
  switch o =>
  | None -> None
  | Some v -> Some (f v)
