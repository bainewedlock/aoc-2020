module Solution

type Value = int64

type ValidPair = {
    sum : Value
    firstPreambleIndex  : int
    secondPreambleIndex : int }

module ValidPair =
    let getSum vp = vp.sum
    let create sum a b =
        { sum = sum; firstPreambleIndex = a; secondPreambleIndex = b }

type Preamble = {
    lowestIndex   : int
    highestIndex  : int
    indexedValues : Map<int, Value>
    validPairs    : ValidPair list }

module Preamble =
    let checkValue x p =
        p.validPairs |> List.exists (ValidPair.getSum >> ((=) x))

    let fromTuple (a, b) = {
        lowestIndex   = 0
        highestIndex  = 1
        indexedValues = Map [ 0,a; 1,b ]
        validPairs    = [ ValidPair.create (a+b) 0 1 ] }

    let removeFirstValue p = 
        let newLowest = p.lowestIndex + 1
        { p with
            lowestIndex   = newLowest
            indexedValues = p.indexedValues.Remove p.lowestIndex
            validPairs    =
                p.validPairs
                |> List.filter (fun p -> p.firstPreambleIndex >= newLowest
                                      && p.secondPreambleIndex >= newLowest) }

    let appendValue x p =
        let h = p.highestIndex
        let newPairs =
            p.indexedValues
            |> Seq.map (fun k -> ValidPair.create (k.Value + x) (k.Key) (h+1))
            |> Seq.toList
        { p with
            highestIndex  = p.highestIndex + 1
            validPairs    = p.validPairs @ newPairs
            indexedValues = p.indexedValues.Add(h+1, x) }

    let fromList = function
        | a::b::rest ->
            rest
            |> Seq.fold (fun acc x -> appendValue x acc) (fromTuple (a, b))
        | x -> failwithf "unexpected: %A" x

    let cycle x = removeFirstValue >> appendValue x

let parse (input:string) =
    input.Trim().Split '\n' |> Array.toList |> List.map int64

let solve preambleSize input =
    let rec loop p = function
        | []    -> failwithf "nothing found"
        | x::tl -> if Preamble.checkValue x p
                   then loop (Preamble.cycle x p) tl else x
    let values   = parse input
    let preamble = values |> List.take preambleSize |> Preamble.fromList
    loop preamble (values |> List.skip preambleSize)

