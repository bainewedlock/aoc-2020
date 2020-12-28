module Solution

let parseLine (y:int, line:string) =
    line
    |> Seq.toList
    |> List.indexed
    |> List.filter (snd >> ((=)'#'))
    |> List.map (fst >> fun x -> x,y,0,0)

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.indexed
    |> List.collect parseLine
    |> Set

type Cubes = Set<int * int * int * int>

let neighbours dws (x0,y0,z0,w0) = [
    for dx in [-1..1] do
    for dy in [-1..1] do
    for dz in [-1..1] do
    for dw in dws do
        if dx <> 0 || dy <> 0 || dz <> 0 || dw <> 0
        then yield (x0+dx,y0+dy,z0+dz,w0+dw) ]

let update (cs:Cubes) neighbours c =
    match cs.Contains c, Map.tryFind c neighbours with
    | true,  Some 2
    | true,  Some 3
    | false, Some 3 -> true
    | _             -> false

let genericStep fNeighbours cs =
    let ns =
        cs
        |> Seq.collect fNeighbours
        |> Seq.countBy id 
        |> Map
    let relevantPositions = 
        ns |> Map.toList |> List.map fst
        |> Set
        |> Set.union cs
    relevantPositions
    |> Set.filter (update cs ns)

let genericSolve fStep input =
    [1..6]
    |> List.fold (fun cs _ -> fStep cs) (parse input)
    |> Set.count

let step  = genericStep (neighbours [0])
let solve = genericSolve step

let step2  = genericStep (neighbours [-1..1])
let solve2 = genericSolve step2
