module Solution

let offset =
    Map [ "e", (1, 0, -1)
          "w", (-1, 0, 1)
          "se", (0, 1, -1)
          "nw", (0, -1, 1)
          "sw", (-1, 1, 0)
          "ne", (1, -1, 0) ]

let toOffset x = offset.Item(x)

let trimStr (s: string) = s.Trim()

let odd x = x % 2 = 1

let parseLine (x: string) =
    let rec loop s =
        [ if s = "" then
            ()
          else
              let d =
                  match s.[0] with
                  | 'e' -> "e"
                  | 'w' -> "w"
                  | _ -> s.Substring(0, 2)

              yield d
              yield! loop (s.Substring d.Length) ]

    loop x

let walk =
    List.reduce (fun (a, b, c) (e, f, g) -> (a + e, b + f, c + g))

let parse (input: string) =
    input.Split '\n'
    |> Array.toList
    |> List.map (trimStr >> parseLine >> List.map toOffset)

let parseAndWalk =
    parse
    >> List.countBy walk
    >> List.filter (snd >> odd)
    >> List.map fst
    >> Set

let solve = parseAndWalk >> Set.count

let neighbours (a, b, c) =
    offset
    |> Map.toSeq
    |> Seq.map (snd >> fun (da, db, dc) -> a + da, b + db, c + dc)

let cellLivesIn grid cell =
    let ns =
        grid
        |> Set.intersect (neighbours cell |> Set)
        |> Set.count

    match grid.Contains cell, ns with
    | true, 0 -> false
    | true, x when x > 2 -> false
    | false, 2 -> true
    | x, _ -> x

let step grid =
    let ns = grid |> Seq.collect neighbours |> Set
    Set.union grid ns |> Set.filter (cellLivesIn grid)

let rec steps grid =
    seq {
        yield grid
        yield! steps (step grid)
    }

let solve2 =
    parseAndWalk >> steps >> Seq.item 100 >> Set.count
