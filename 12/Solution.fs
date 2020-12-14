module Solution

type Ship = {
    location    : int * int
    waypoint    : int * int }

type Transformation =
    | ChangeLocation of (int * int)
    | ChangeWaypoint of (int * int)

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map (fun x -> x.Trim())
    |> List.map (fun s -> s.[0], s.Substring(1) |> int)

let rec rotateVector90 (x0, y0) n =
    if n = 0 then (x0, y0)
    else rotateVector90 (y0, -x0) (n-1)

let moveVector (x0, y0) factor (dx, dy) = x0+dx*factor, y0+dy*factor

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

let tryParseForward = function
    | 'F', speed -> Some speed
    | _          -> None

let offsetShip (cmd, n) ship =
    tryParseOrientation cmd
    |> Option.map (moveVector ship.location n)
    |> Option.map ChangeLocation

let offsetWaypoint (cmd, n) ship =
    tryParseOrientation cmd
    |> Option.map (moveVector ship.waypoint n)
    |> Option.map ChangeWaypoint

let rotateWaypoint cmd ship =
    tryParseRotation cmd
    |> Option.map (rotateVector90 ship.waypoint)
    |> Option.map ChangeWaypoint

let moveForward cmd ship =
    tryParseForward cmd
    |> Option.map (fun n -> moveVector ship.location n ship.waypoint)
    |> Option.map ChangeLocation

let private apply cmd ship f = 
    match f cmd ship with
    | None -> ship
    | Some (ChangeLocation x) -> { ship with location = x }
    | Some (ChangeWaypoint x) -> { ship with waypoint = x }

let private genericSolve handlers wp =
    parse
    >> List.fold handlers { waypoint = wp; location = 0, 0 }
    >> fun { location = x, y } -> x, y, abs x + abs y

let genericExecute actions ship cmd = List.fold (apply cmd) ship actions
let execute1 = genericExecute [ moveForward; rotateWaypoint; offsetShip ]
let execute2 = genericExecute [ moveForward; rotateWaypoint; offsetWaypoint ]

let solve  = genericSolve execute1 ( 1, 0)
let solve2 = genericSolve execute2 (10, 1)

