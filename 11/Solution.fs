module Solution

open System

type Grid = {
    data : char[,]
    width  : int
    height : int }

module Grid =
    let ys g = seq { 0..g.height-1}
    let xs g = seq { 0..g.width-1}
    let private printLine y g =
        xs g
        |> Seq.map (fun x -> g.data.[x, y])
        |> Seq.toArray
        |> fun cs -> String(cs)
    let print g = 
        ys g
        |> Seq.toList
        |> List.map (fun y -> printLine y g )

let parse (input:string) =
    let lines  = input.Trim().Split '\n' |> Array.map (fun x -> x.Trim())
    let width  = lines |> Seq.map (fun x -> x.Length) |> Seq.max
    let height = lines.Length 
    {   width  = width
        height = height
        data   = Array2D.init width height (fun x y -> lines.[y].[x]) }

type Cell = {
    content            : char
    occupiedNeighbours : int }

let nextCellState c =
    match c.content with
    | 'L' when c.occupiedNeighbours =  0 -> '#'
    | '#' when c.occupiedNeighbours >= 4 -> 'L'
    | x   -> x

let adjacent (x,y) grid =
    [   for dx in [-1; 0; 1] do
        for dy in [-1; 0; 1] do
            let x, y = x+dx, y+dy
            if  x >= 0 && y >= 0 &&
                x < grid.width && y < grid.height &&
                (dx <> 0 || dy <> 0)
                then yield (x, y) ]

let analyzeCell (x,y) grid =
    let o =
        adjacent (x,y) grid
        |> Seq.filter (fun (x,y) -> grid.data.[x,y] = '#')
        |> Seq.length
    { content=grid.data.[x,y]; occupiedNeighbours=o }

let step grid =
    let calc x y = analyzeCell (x, y) grid |> nextCellState
    { grid with data = Array2D.init grid.width grid.height calc }

let countSeats grid =
    seq {   for x in Grid.xs grid do
            for y in Grid.ys grid do
                if grid.data.[x,y] = '#' then yield 1 }
    |> Seq.length

let solve input =
    let rec loop n grid =
        let grid' = step grid
        if Grid.print grid = Grid.print grid' 
        then grid
        else loop (n+1) grid'
    parse input |> loop 0 |> countSeats

