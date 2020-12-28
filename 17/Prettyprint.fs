module Prettyprint

open Solution
open System

let printlayer (cubes:Cubes) s z = [
    yield (sprintf "z=%d" z)
    for y in [-s..s] do
        yield (
            [-s..s]
            |> List.map (fun x -> if cubes.Contains(x,y,z,0) then '#' else '.')
            |> List.toArray
            |> String) ]

let prettyprint s zs cubes =
    zs
    |> List.map (printlayer cubes s) 
    |> List.reduce (fun a b -> a @ [""] @ b)
