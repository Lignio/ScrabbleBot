module MultiSet

    type MultiSet<'a when 'a : comparison> = M of (Map<'a, uint32>)

    let empty: MultiSet<'a>  = M Map.empty<'a, uint32>

    let isEmpty (M s) = Map.isEmpty s

    let size (M s) = Map.fold (fun acc key value-> acc + value) 0u s

    let contains a (M s) = Map.containsKey a s

    let numItems a (M s) =
        match Map.tryFind a s with
        | None -> 0u
        | Some(value) -> value
    
    let add a n (M s) =M  (Map.add a ((numItems a (M s)) + n) s)

    let addSingle a (M s) = M (Map.add a ((numItems a (M s) + 1u)) s)

    let remove a n (M s) = if (numItems a (M s)) > n then (M (Map.add a (numItems a (M s) - n) s)) else (M (Map.remove a s))

    let removeSingle a (M s) = if (numItems a (M s)) > 0u then (M (Map.add a (numItems a (M s) - 1u) s)) else (M (Map.remove a s))

    let fold (f: 'a -> 'b -> uint32 -> 'a) acc (M s) = Map.fold(f) acc s

    let foldBack (f: 'a -> 'uint32 -> 'b -> 'b) (M s) acc  = Map.foldBack(f) s acc