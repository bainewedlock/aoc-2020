module Solution

type Ship = {
    location    : int * int
    waypoint    : int * int }

module Ship =
    let create = {
        waypoint    = 1, 0
        location    = 0, 0 }

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map (fun x -> x.Trim())
    |> List.map (fun s -> s.[0], s.Substring(1) |> int)

//let moveWaypoint orientation n ship = 
//    let dx, dy =
//        match orientation with
//        | North -> +0, +n
//        | East  -> +n, +0
//        | South -> +0, -n
//        | West  -> -n, +0
//    let x0, y0 = ship.location
//    { ship with location = x0+dx, y0+dy }

let rec rotateVector90 n (x, y) =
    if n = 0 then (x, y)
    else rotateVector90 (n-1) (y, -x)

let rotateShip degree ship =
    { ship with waypoint = rotateVector90 (degree/90) ship.waypoint }

let rec move n (dx, dy) ship =
    if n = 0 then ship else
    let x0, y0 = ship.location
    move (n-1) (dx, dy) { ship with location = x0+dx, y0+dy }

let action ship (cmd, n) =
    match cmd with
    | 'E' -> move n ( 1, 0) ship
    | 'N' -> move n ( 0, 1) ship
    | 'S' -> move n ( 0,-1) ship
    | 'W' -> move n (-1, 0) ship
    | 'R' -> rotateShip n ship
    | 'L' -> rotateShip (360-n) ship
    | 'F' -> move n ship.waypoint ship
    | _   -> failwith "unexpected"

let solve =
    parse
    >> List.fold action Ship.create
    >> fun {location = x, y} -> x, y, abs x + abs y

