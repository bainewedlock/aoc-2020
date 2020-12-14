module Solution

let parseNumberlist (line:string) =
    line.Split ','
    |> Array.toList
    |> List.filter ((<>) "x")
    |> List.map int

let parse (input:string) =
    match input.Split '\n' with
    | [|a;b|] -> int a, parseNumberlist b
    | _       -> failwith "unexpected input"

let solve input =
    let earliestDeparture, bus_ids = parse input
    let findBusAt time = bus_ids |> Seq.tryFind (fun b -> time % b = 0)
    let rec loop time =
        match findBusAt time with
        | Some bus_id -> bus_id, time - earliestDeparture
        | None        -> loop (time + 1)
    let bus_id, wait = loop earliestDeparture
    sprintf "%d * %d = %d" bus_id wait (bus_id * wait)
