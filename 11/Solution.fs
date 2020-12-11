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
    let inBounds (x, y) g =
        x >= 0      && y >= 0 &&
        x < g.width && y < g.height

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

let nextCellState threshold c =
    match c.content with
    | 'L' when c.occupiedNeighbours =  0 -> '#'
    | '#' when c.occupiedNeighbours >= threshold -> 'L'
    | x   -> x

let adjacent (x,y) grid =
    let inBounds x y = grid |> Grid.inBounds (x, y)
    [   for dx in [-1; 0; 1] do
        for dy in [-1; 0; 1] do
            let x, y = x+dx, y+dy
            if inBounds x y && (dx <> 0 || dy <> 0) then yield (x, y) ]

let analyzeCell (x,y) grid =
    let o =
        adjacent (x,y) grid
        |> Seq.filter (fun (x,y) -> grid.data.[x,y] = '#')
        |> Seq.length
    { content=grid.data.[x,y]; occupiedNeighbours=o }

let customStep fAnalyze fCell grid =
    let calc x y = fAnalyze (x, y) grid |> fCell
    { grid with data = Array2D.init grid.width grid.height calc }

let step = customStep analyzeCell (nextCellState 4)

let countSeats grid =
    seq {   for x in Grid.xs grid do
            for y in Grid.ys grid do
                if grid.data.[x,y] = '#' then yield 1 }
    |> Seq.length

let customSolve fStep input =
    let rec loop n grid =
        let grid' = fStep grid
        if Grid.print grid = Grid.print grid' 
        then grid
        else loop (n+1) grid'
    parse input |> loop 0 |> countSeats

let solve = customSolve step

let raytrace (x, y) grid =
    let invalid x y = grid |> Grid.inBounds (x, y) |> not

    let rec traceDir (x0, y0) (dx, dy) = [
        let x, y = x0+dx, y0+dy
        if invalid x y then () else
        match grid.data.[x, y] with
        | '#' -> yield (x, y, '#')
        | 'L' -> yield (x, y, 'L')
        | _   -> yield! traceDir (x, y) (dx, dy) ]

    [   for h in [-1; 0; 1] do
        for v in [-1; 0; 1] do
        if h <> 0 || v <> 0 then yield h, v ]
    |> List.collect (traceDir (x, y))

let analyzeCell2 (x, y) grid =
    let o =
        raytrace (x, y) grid
        |> Seq.filter (fun (_, _, c) -> c = '#')
        |> Seq.length
    { content = grid.data.[x, y]; occupiedNeighbours = o }

let step2 = customStep analyzeCell2 (nextCellState 5)

let solve2 = customSolve step2
