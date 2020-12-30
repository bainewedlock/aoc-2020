module Solution

let pickup (input:string) =
    input.Substring(0, 3), input.Substring(3)

let digits (input:string) =
    input
    |> Seq.map (string >> int)
    |> Seq.toList

let calcTarget (current:string) (picked:string) =
    [1..9] @ [1..9]
    |> List.rev
    |> List.skip (10 - int current)
    |> List.except (digits picked)
    |> List.head
    |> string

let rotateLeft n (input:string) =
    let n = (n + input.Length) % input.Length
    input.Substring(n) + input.Substring(0, n)

let rotateToEnd (target:string) (input:string) =
    rotateLeft (input.IndexOf target + 1) input

let move (input:string) =
    let current = input.Substring(0, 1)
    let picked, remaining = input |> rotateToEnd current |> pickup
    let target = calcTarget current picked
    rotateToEnd current (picked + (rotateToEnd target remaining))

let solve =
    Seq.unfold (fun s -> Some (s, move s)) 
    >> Seq.item 100
    >> rotateToEnd "1"
    >> fun s -> s.Replace("1","")
