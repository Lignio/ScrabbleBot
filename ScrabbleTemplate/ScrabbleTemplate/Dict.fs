module internal TriDictionary

type Dict = 
    | Leaf of bool
    | Node of bool * Map<char, Dict>



let empty () = Leaf false

let rec insert (a:string) = function
    | Leaf b when a = "" -> Leaf true
    | Node (b, m) when a = "" -> Node (true, m)
    | Leaf b when a.Length >=1 -> 
        Node (b, Map.add a.[0] (insert (a.Remove(0,1)) (empty())) Map.empty)
    | Node (b, m) when a.Length >=1 ->
                match Map.tryFind a.[0] m with
                | None -> Node (b, Map.add a.[0] (insert (a.Remove(0,1)) (empty())) m)
                | Some x -> Node (b, Map.add a.[0] (insert (a.Remove(0,1)) x) m)
        

let rec lookup a = function
    | Leaf b when a = "" -> b
    | Leaf b when a.Length >=1 -> false
    | Node (b, m) when a = "" -> b 
    | Node (b, m) when a.Length >=1 -> 
        match Map.tryFind a.[0] m with 
        | None -> false
        | Some x -> lookup (a.Remove(0,1)) x
 

let step (a:char) = function
        |Leaf b -> None
        |Node (b, m)->
            match Map.tryFind a m with
            | None ->   None
            | Some x ->
                match x with
                |Leaf b -> Some (b, x)
                |Node (b, _) -> Some(b, x)
            
            