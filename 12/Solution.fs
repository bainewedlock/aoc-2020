module Solution

type Orientation =
    |    North
    | West | East
    |    South

type Ship = {
    orientation : Orientation
    location    : int * int }

module Ship =
    let create = {
        orientation = East
        location    = 0, 0 }


let rotate o deg =
    let os = [| North; East; South; West |]
    let i = os |> Array.findIndex ((=) o)
    let d = (4 + i + deg / 90) % 4
    os.[d]

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map (fun x -> x.Trim())
    |> List.map (fun s -> s.[0], s.Substring(1) |> int)

let move orientation n ship = 
    let dx, dy =
        match orientation with
        | North -> +0, +n
        | East  -> +n, +0
        | South -> +0, -n
        | West  -> -n, +0
    let x0, y0 = ship.location
    { ship with location = x0+dx, y0+dy }

let rotateShip n ship =
    { ship with orientation = rotate ship.orientation n }

let action ship (cmd, n) =
    match cmd with
    | 'E' -> move East  n ship
    | 'N' -> move North n ship
    | 'S' -> move South n ship
    | 'W' -> move West  n ship
    | 'R' -> rotateShip n ship
    | 'L' -> rotateShip -n ship
    | 'F' -> move ship.orientation n ship
    | _   -> failwith "unexpected"

let solve =
    parse
    >> List.fold action Ship.create
    >> fun {location = x, y} -> x, y, abs x + abs y

