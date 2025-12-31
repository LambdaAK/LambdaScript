type Point = {x: int, y: int}
  type rec Shape =
    | Circle of {center: Point, radius: int}
    | Rectangle of {topLeft: Point, bottomRight: Point}
    | Group of {shapes: [Shape]}

  let rec count_shapes = fn s ->
    case s do
    | Circle _ -> 1
    | Rectangle _ -> 1
    | Group g ->
      let rec count_list = fn lst ->
        case lst do
        | [] -> 0
        | h :: t -> count_shapes h + count_list t
      in count_list g.shapes

  let scene = Group {
    shapes: [
      Circle {center: {x: 0, y: 0}, radius: 5},
      Rectangle {topLeft: {x: 0, y: 0}, bottomRight: {x: 10, y: 10}},
      Group {shapes: [
        Circle {center: {x: 5, y: 5}, radius: 3},
        Circle {center: {x: 10, y: 10}, radius: 2}
      ]}
    ]
  }