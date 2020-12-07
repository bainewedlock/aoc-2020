open System.IO

// copied from day 04
let splitLines predicate xs =
    List.foldBack (fun x acc ->
        match acc with 
        | []     -> failwith "unexpected"
        | hd::tl ->
            if predicate x
            then ([]::(hd::tl))
            else ((x::hd)::tl)) (Seq.toList xs) [[]]

[<EntryPoint>]
let main argv =

    let groupedLines = File.ReadAllLines "input.txt" |> splitLines ((=) "")

    groupedLines
    |> Seq.sumBy (String.concat "" >> Seq.distinct >> Seq.length)
    |> printfn "solution: %d"
    printfn "(must be 6782)"

    groupedLines
    |> Seq.sumBy (List.map Set
                  >> Seq.reduce Set.intersect
                  >> Seq.distinct
                  >> Seq.length)
    |> printfn "solution: %d"
    printfn "(must be 3596)"

    0
