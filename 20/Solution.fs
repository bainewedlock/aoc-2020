module Solution

open System
open System.Text.RegularExpressions

let tileSize = 10
type Tile = { Edges : string Set }

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
        regexReplace "\D" "" header |> int, { Edges = parseEdges lines }
    | x             -> failwithf "unexpected: %A" x

let parse (input:string) : Map<int,Tile> =
    input.Split '\n'
    |> Array.toList
    |> List.map (fun s -> s.Trim())
    |> List.chunkBySize (tileSize+2)
    |> List.map (List.take (tileSize+1) >> parseTile)
    |> Map

let reverseString (s:string) =
    s
    |> Seq.rev
    |> Seq.toArray
    |> String

let matchingEdges es1 es2 =
    let es2rev = es2 |> Set.map reverseString
    Set.intersect es1 (Set.union es2 es2rev)

let matchingTiles n (tiles:Map<int, Tile>) =
    let edges = tiles.[n].Edges
    tiles
    |> Map.filter (fun k v ->
        k <> n && (matchingEdges edges v.Edges).Count > 0)
    |> Map.toList
    |> List.map fst
    |> Set

let analyze tiles =
    tiles
    |> Map.toSeq
    |> Seq.map (fst >> (fun t -> t, matchingTiles t tiles))
    |> Map

let solve =
    parse
    >> analyze
    >> Map.toSeq
    >> Seq.filter (snd >> Set.count >> (=)2)
    >> Seq.map (fst >> int64)
    >> Seq.reduce (*)
