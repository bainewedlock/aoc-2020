module Solution
open System.Text.RegularExpressions

type Password = {
    lo : int
    hi : int
    letter : string
    text : string }

let parseLine (line:string) =
    let m = Regex.Match(line, @"^(\d+)\-(\d+) (\w): (\w+)$")
    {   lo = m.Groups.[1].Value |> int
        hi = m.Groups.[2].Value |> int
        letter = m.Groups.[3].Value
        text   = m.Groups.[4].Value }

let eval pw =
    let found = Regex.Matches(pw.text, pw.letter).Count
    pw.lo <= found && found <= pw.hi

let genericSolve evalFunc (input:string) =
    Regex.Split(input.Trim(), "\n")
    |> Array.toList
    |> List.map (fun x -> x.Trim())
    |> List.map (fun x -> x, x |> parseLine |> evalFunc)

let solve = genericSolve eval

let eval2 (pw:Password) =
    let checkAt i = pw.text.[i] = pw.letter.[0]
    [pw.lo - 1; pw.hi - 1]
    |> List.filter checkAt
    |> List.length
    |> ((=) 1)

let solve2 = genericSolve eval2
