module Solution

open System

let parseLine (y:int, line:string) =
    line
    |> Seq.toList
    |> List.indexed
    |> List.filter (snd >> ((=)'#'))
    |> List.map (fst >> fun x -> x,y,0)

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.indexed
    |> List.collect parseLine
    |> Set

type Cubes = Set<int * int * int>

let printlayer (cubes:Cubes) s z = [
    yield (sprintf "z=%d" z)
    for y in [-s..s] do
        yield (
            [-s..s]
            |> List.map (fun x -> if cubes.Contains(x,y,z) then '#' else '.')
            |> List.toArray
            |> String) ]

let prettyprint s zs cubes =
    zs
    |> List.map (printlayer cubes s) 
    |> List.reduce (fun a b -> a @ [""] @ b)

let neighbours (x0,y0,z0) = [
    for dx in [-1..1] do
    for dy in [-1..1] do
    for dz in [-1..1] do
        if dx <> 0 || dy <> 0 || dz <> 0
        then yield (x0+dx,y0+dy,z0+dz) ]

let update (cs:Cubes) neighbours c =
    match cs.Contains c, Map.tryFind c neighbours with
    | true,  Some 2
    | true,  Some 3
    | false, Some 3 -> true
    | _             -> false

let step (cs:Cubes) : Cubes =
    let neighbours =
        cs
        |> Seq.collect neighbours
        |> Seq.countBy id 
        |> Map
    let relevantPositions = 
        neighbours |> Map.toList |> List.map fst
        |> Set
        |> Set.union cs
    relevantPositions
    |> Set.filter (update cs neighbours)


let solve input =
    [1..6]
    |> List.fold (fun cs _ -> step cs) (parse input)
    |> Set.count
