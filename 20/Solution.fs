module Solution

open System
open System.Text.RegularExpressions

let tileSize = 10
type Tile = { No: int; Edges : string Set }

let regexReplace (pattern:string) (replacement:string) (input:string) =
    Regex.Replace(input, pattern, replacement)

let verticalLine x (lines:string list) =
    lines
    |> List.map (fun l -> l.Substring(x, 1))
    |> String.concat ""

let parseEdges (lines:string list) = Set [
    lines.Head
    verticalLine (tileSize-1) lines
    verticalLine 0 lines
    lines |> List.last ]

let parseTile = function
    | header::lines ->
        let no = regexReplace "\D" "" header |> int
        { No = no |> int; Edges = parseEdges lines }
    | x             -> failwithf "unexpected: %A" x

let parse (input:string) : Tile list =
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

let matchingEdges es1 es2 =
    let es2rev = es2 |> Set.map reverseString
    Set.intersect es1 (Set.union es2 es2rev)

let matchingTiles t (tiles:Tile List) =
    let edges = t.Edges
    tiles
    |> List.filter (fun t' ->
        t'.No <> t.No && (matchingEdges edges t'.Edges).Count > 0)
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
