module Solution

type Ship = {
    location    : int * int
    waypoint    : int * int }

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map (fun x -> x.Trim())
    |> List.map (fun s -> s.[0], s.Substring(1) |> int)

let rec rotateVector90 (x0, y0) n =
    if n = 0 then (x0, y0)
    else rotateVector90 (y0, -x0) (n-1)

let rec moveVector (x0, y0) (dx, dy) = function
    | 0            -> x0, y0
    | n when n > 0 -> moveVector (x0+dx, y0+dy) (dx, dy) (n-1)
    | x            -> failwithf "unexpected: %A" x

let tryParseOrientation = function
    | 'N' -> Some ( 0,  1)
    | 'E' -> Some ( 1,  0)
    | 'S' -> Some ( 0, -1)
    | 'W' -> Some (-1,  0)
    | _   -> None

let tryParseRotation = function
    | 'R', degree -> Some (degree / 90)
    | 'L', degree -> Some (4 - degree / 90)
    | _           -> None

let applyShipOffset (cmd, n) ship =
    match tryParseOrientation cmd with
    | Some speed -> { ship with location = moveVector ship.location speed n }
    | None       -> ship

let applyRotation cmd ship =
    match tryParseRotation cmd with
    | Some r -> { ship with waypoint = rotateVector90 ship.waypoint r }
    | None   -> ship

let applyForward (cmd, n) ship =
    match cmd with
    | 'F' -> { ship with location = moveVector ship.location ship.waypoint n }
    | _   -> ship

let applyWaypointOffset (cmd, n) ship =
    match tryParseOrientation cmd with
    | Some speed -> { ship with waypoint = moveVector ship.waypoint speed n }
    | None       -> ship

let part1Actions ship a =
    ship
    |> applyShipOffset a
    |> applyForward a
    |> applyRotation a

let part2Actions ship a =
    ship
    |> applyWaypointOffset a
    |> applyForward a
    |> applyRotation a

let genericSolve actions wp =
    parse
    >> List.fold actions { waypoint = wp; location = 0, 0 }
    >> fun { location = x, y } -> x, y, abs x + abs y

let solve  = genericSolve part1Actions  ( 1, 0)
let solve2 = genericSolve part2Actions (10, 1)

