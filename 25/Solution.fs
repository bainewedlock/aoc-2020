module Solution

open System

let initialSubjectNumber = 7L

let transform (subjectNumber:int64) =
    Seq.unfold (fun v -> Some (v, v * subjectNumber % 20201227L)) 1L

let determineLoopSize publicKey =
    transform initialSubjectNumber
    |> Seq.findIndex ((=)publicKey)

let parse (input:string) =
    match input.Split '\n' |> Array.map Int64.Parse with
    | [|card;door|] -> card, door
    | x -> failwithf "unexpected input: %A" x

let solve input =
    let card, door = parse input
    transform door |> Seq.item (determineLoopSize card)
