module internal MultiSet

// For more information see https://aka.ms/fsharp-console-apps
//printfn "Hello from F#"

    type MultiSet<'a when 'a: comparison> = MSet of Map<'a, uint32>

    let empty = MSet Map.empty

    let isEmpty (MSet a) = Map.isEmpty a 

    let size (MSet a)  =  Map.fold (fun acc k v -> acc + v) 0u a

    let contains k (MSet a) = Map.containsKey k a

    let numItems a (MSet s) = Map.tryFind a s |> Option.defaultValue 0u

    let add a n (MSet s) = MSet (Map.add a ((numItems a (MSet s)) + n) s) 

    let addSingle a (MSet s) = MSet (Map.add a ((numItems a (MSet s)) + 1u) s)

    let remove a n (MSet s) = 
        if (numItems a (MSet s) - uint32(n)) < 0u then MSet (Map.remove a s) 
        else MSet (Map.add a (((numItems a (MSet s)) - n)) s)

    let fold : ('b -> 'a ->uint32 ->'b) -> 'b-> MultiSet<'a> -> 'b = fun _ _ _ -> failwith "not implemented"
    