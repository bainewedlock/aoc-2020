module Solution

let offset = Map [
    "e",  ( 1,  0, -1)
    "w",  (-1,  0,  1)
    "se", ( 0,  1, -1)
    "nw", ( 0, -1,  1)
    "sw", (-1,  1,  0)
    "ne", ( 1, -1,  0) ]

let parseLine (x:string) =
    let rec loop s = [
        if s = "" then () else
        let d = match s.[0] with
                | 'e' -> "e"
                | 'w' -> "w"
                | _   -> s.Substring(0, 2)
        yield d
        yield! loop (s.Substring d.Length) ]
    loop x

let walk xs = 
    xs
    |> List.fold (fun (a,b,c) (da,db,dc) -> (a+da,b+db,c+dc)) (0,0,0)

let toOffset x = offset.Item(x)

let trimStr (s:string) = s.Trim()

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map (trimStr >> parseLine >> List.map toOffset)

let odd x = x % 2 = 1

let solve =
    parse 
    >> List.countBy walk 
    >> List.filter (snd >> odd) 
    >> List.length

