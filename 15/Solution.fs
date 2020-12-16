module Solution

type NumberLog =
    | SpokenOnce of index:int
    | SpokenMultipleTimes of indexBeforeLastTime:int * indexOfLastTime:int

module NumberLog =
    let create index = SpokenOnce index
    let add index nl =
        match nl with
        | SpokenOnce i'               -> i'
        | SpokenMultipleTimes (_, i') -> i'
        |> fun i' -> SpokenMultipleTimes (i', index)

type History = {
    log              : Map<int, NumberLog>
    nextIndex        : int
    lastNumberSpoken : int }

module History =
    let empty = { log = Map.empty; nextIndex = 0; lastNumberSpoken = -1 }
    let add number history =
        let logEntry =
            history.log.TryFind number
            |> Option.map (NumberLog.add history.nextIndex)
            |> Option.defaultValue (NumberLog.create history.nextIndex)
        { history with
            lastNumberSpoken = number
            nextIndex        = history.nextIndex + 1
            log              = history.log.Add(number, logEntry) }
    let nextNumber history =
        match history.log.Item history.lastNumberSpoken with
        | SpokenOnce _                 -> 0
        | SpokenMultipleTimes (i0, i1) -> i1 - i0

let spokenNumbers startingNumbers = seq {
    let rec loop h = seq {
        let n = History.nextNumber h
        yield n
        yield! loop (History.add n h) }

    let history =
        startingNumbers
        |> List.fold (fun h n -> History.add n h) History.empty

    yield! startingNumbers
    yield! loop history }

let genericSolve count (input:string) =
    input.Split ','
    |> Array.toList
    |> List.map int
    |> spokenNumbers
    |> Seq.skip (count-1)
    |> Seq.head

let solve  = genericSolve 2020

let solve2 = genericSolve 30000000

