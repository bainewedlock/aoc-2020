module Solution

open System
open System.Text.RegularExpressions

let tileSize = 10
type Edge = Right | Left | Top | Bottom
type Tile = { No: int; Edges : Map<Edge, string> }

let regexReplace (pattern:string) (replacement:string) (input:string) =
    Regex.Replace(input, pattern, replacement)

let verticalLine x (lines:string list) =
    lines
    |> List.map (fun l -> l.Substring(x, 1))
    |> String.concat ""

let parseEdges (lines:string list) = Map [
    Top, lines.Head
    Left, verticalLine 0 lines
    Right, verticalLine (tileSize-1) lines
    Bottom, lines |> List.last ]

let parseTile = function
    | header::lines ->
        let no = regexReplace "\D" "" header |> int
        { No = no |> int; Edges = parseEdges lines }
    | x             -> failwithf "unexpected: %A" x

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map (fun s -> s.Trim())
    |> List.chunkBySize (tileSize+2)
    |> List.map (List.take (tileSize+1) >> parseTile)

let reverseString (s:string) =
    s
    |> Seq.rev
    |> Seq.toArray
    |> String

let matchingEdges t1 t2 =
    let es1 = t1.Edges |> Map.toList |> List.map snd |> Set
    let es2 =
        [   t2.Edges |> Map.toList |> List.map snd
            t2.Edges |> Map.toList |> List.map snd |> List.map reverseString]
        |> List.concat
        |> Set
    Set.intersect es1 es2

let matchingTiles t (tiles:Tile List) =
    tiles
    |> List.filter (fun t' ->
        t'.No <> t.No && (matchingEdges t t').Count > 0)
    |> List.map (fun t -> t.No)
    |> Set

let analyze tiles =
    tiles
    |> Seq.map (fun t -> t.No, matchingTiles t tiles)
    |> Map

let solve =
    parse
    >> analyze
    >> Map.toSeq
    >> Seq.filter (snd >> Set.count >> (=)2)
    >> Seq.map (fst >> int64)
    >> Seq.reduce (*)
