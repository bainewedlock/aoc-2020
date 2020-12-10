module Solution

let solve (input:string) =
    input.Split '\n'
    |> Seq.map ((fun x -> x.Trim()) >> int)
    |> Seq.sort
    |> Seq.fold (fun (prev, history:Map<int,int>) x -> 
        let d = x - prev
        let sum = history.TryFind d |> Option.defaultValue 0
        (x, history.Add(d, sum+1)))
            (0, Map.empty)
    |> snd
    |> fun m -> m.[1], m.[3] + 1
