module Solution2

let rec arrange (tiles:Map<int, int Set>)  =
    let size = tiles.Count |> float |> sqrt |> int
    let rec loop (tiles:Map<int, int Set>) (acc:Map<int * int, int>) =
        if tiles.IsEmpty then Some acc else
        let x = acc.Count % size
        let y = acc.Count / size
        let refTile = 
            if x = 0 then acc.[0, y-1]
            else          acc.[x-1, y]
        tiles
        |> Map.toSeq
        |> Seq.tryPick (fun (no, others) ->
            if not <| others.Contains refTile then None else
            let acc' = acc.Add((x,y), no)
            let tiles' = tiles.Remove(no) 
            loop tiles' acc')
    let someCorner =
        tiles |> Map.toSeq |> Seq.find (snd >> Set.count >> (=)2) |> fst
    let tilesWithoutCorner = tiles.Remove someCorner
    let initialArrangement = Map [(0,0), someCorner]
    loop tilesWithoutCorner initialArrangement
    |> Option.get
