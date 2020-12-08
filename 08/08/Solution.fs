module Solution

type Instruction =
    | Nop of int
    | Acc of int
    | Jmp of int

type Code = Map<int, Instruction>

type State = {
    nextLine     : int
    acc          : int 
    visitedLines : int Set }

module State =
    let initial = { nextLine = 0; acc = 0; visitedLines = Set.empty }
    let getAcc s = s.acc

let toTuple = function
    | a::b::[] -> a, b
    | x        -> failwithf "cant make tuple of %A" x

let parseLine (line:string) =
    let a, b = line.Split() |> Array.toList |> toTuple
    match a with
    | "nop" -> Nop (int b)
    | "acc" -> Acc (int b)
    | "jmp" -> Jmp (int b)
    | x     -> failwithf "unexpected operation: %s" x

let parse (input:string) =
    input.Trim().Split '\n'
    |> Array.toList
    |> List.map ((fun x -> x.Trim()) >> parseLine)
    |> List.indexed
    |> Map

let step code s =
    let dLine, dAcc =
        match code |> Map.find s.nextLine with
        | Nop _ -> 1, 0
        | Acc x -> 1, x
        | Jmp x -> x, 0
    { s with
        visitedLines = s.visitedLines.Add s.nextLine
        nextLine = s.nextLine + dLine
        acc = s.acc + dAcc }

let loopOccured s = s.visitedLines.Contains s.nextLine

let private tryStep code s =
    if loopOccured s then None
    else Some (s, step code s)

let rawSolve code = Seq.unfold (tryStep code) State.initial |> Seq.last

let lastLine (code:Code) = (code |> Seq.map (fun k -> k.Key) |> Seq.max)

let appendLoop (code:Code) = code.Add(lastLine code + 1, Jmp 0)

let terminates (code:Code) = (rawSolve code).nextLine = lastLine code

let variants (code:Code) : Code seq =
    code |> Seq.choose (fun kvp ->
        match kvp.Key, kvp.Value with
        | l, Jmp x -> Some (code.Add(l, Nop x))
        | l, Nop x -> Some (code.Add(l, Jmp x))
        | _        -> None)

let solve =
    parse
    >> rawSolve
    >> State.getAcc

let solve2 =
    parse
    >> variants
    >> Seq.map appendLoop
    >> Seq.find terminates
    >> rawSolve
    >> State.getAcc

