module Solution

let offset = Map [
    "e",  ( 1,  0, -1)
    "w",  (-1,  0,  1)
    "se", ( 0,  1, -1)
    "nw", ( 0, -1,  1)
    "sw", (-1,  1,  0)
    "ne", ( 1, -1,  0) ]

let toOffset x = offset.Item(x)

let trimStr (s:string) = s.Trim()

let odd x = x % 2 = 1

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

let walk = List.reduce (fun (a,b,c) (e,f,g) -> (a+e,b+f,c+g))

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map (trimStr >> parseLine >> List.map toOffset)

let solve =
    parse 
    >> List.countBy walk 
    >> List.filter (snd >> odd) 
    >> List.length

