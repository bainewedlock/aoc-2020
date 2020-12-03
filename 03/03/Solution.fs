module Solution

module String = let trim (x:string) = x.Trim()
 
let trimStr (x:string) = x.Trim()

let parse (x:string) =
    x.Split '\n'
    |> Array.toList
    |> List.map trimStr
    |> List.skip 1

let findTrees dx input =
    input
    |> List.filter (fun x -> x <> "")
    |> List.indexed
    |> List.choose (fun (y, line) ->
        let x = (y+1) * dx % line.Length
        if line.[x] = '#' then Some (y+1) else None)
    |> Set

let solve input =
    input
    |> parse
    |> findTrees 3 
    |> Set.count

let isOdd x =
    x % 2 = 1

let filterodd xs = 
    xs |> List.indexed |> List.filter (fst >> isOdd) |> List.map snd

let solve2 input =
    let lines = input |> parse
    [   findTrees 1 lines
        findTrees 3 lines
        findTrees 5 lines
        findTrees 7 lines
        findTrees 1 (filterodd lines) ]
    |> Seq.map Set.count
    |> Seq.map uint32
    |> Seq.reduce (*)
