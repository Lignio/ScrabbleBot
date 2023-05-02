namespace ScrapBot

open System.Collections.Generic
open Microsoft.FSharp.Collections
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()


module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

 
    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        boardMap      : Map<coord, char>
    }

    let mkState b d pn h m = {board = b; dict = d;  playerNumber = pn; hand = h; boardMap = m}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let boardMap st     = st.boardMap


module Bot =
    type originDirection =
        | right = 0
        | down = 1
        | left = 2
        | up = 3
   
    // Takes a coord and goes through each neighbor, returning coords and a bool of wheter or not there is a letter there 
    let neighborHasValue (boardMap: Map<coord, char>) (location :coord) (neighbor : originDirection) =
        match neighbor with
            | originDirection.right ->
                match boardMap.TryFind ((fst location) + 1 , snd location) with
                    | None -> (false, ((fst location) + 1 , snd location))
                    | Some x -> (true, ((fst location) + 1 , snd location))
            | originDirection.left ->
               match boardMap.TryFind ((fst location) - 1 , snd location) with
                    | None -> (false, ((fst location) - 1 , snd location))
                    | Some x-> (true, ((fst location) - 1 , snd location))
            | originDirection.up ->
               match boardMap.TryFind (fst location , (snd location) + 1) with
                    | None -> (false, (fst location , (snd location) + 1))
                    | Some x -> (true, (fst location , (snd location) + 1))
            | originDirection.down ->
               match boardMap.TryFind (fst location,(snd location) - 1) with
                    | None -> (false, (fst location , (snd location) - 1))
                    | Some x -> (true, (fst location , (snd location) - 1))
    
    // Actually does the thing                                                                                                                     
    let trueNeighborList (boardMap: Map<coord, char>) (location : coord) (neighborMap : Map<originDirection,(bool*coord)>) = neighborMap |> Map.add originDirection.right (neighborHasValue boardMap location originDirection.right) |>
                                                                                                                             Map.add originDirection.down (neighborHasValue boardMap location originDirection.down) |>
                                                                                                                             Map.add originDirection.left (neighborHasValue boardMap location originDirection.left) |>
                                                                                                                             Map.add originDirection.up (neighborHasValue boardMap location originDirection.up)
    // Takes a location and returns a list of the four neighbors
    let neighborList (boardMap: Map<coord, char>) (location : coord) = trueNeighborList boardMap location Map.empty
    
    // Makes state after initial word has then stepped
    let getInitialDict (st: State.state) (word: char list) =
        List.fold (fun dictAcc value ->
            match Dictionary.step (value) dictAcc with
            | None -> st.dict
            | Some (_, dict) -> dict
            ) st.dict word
    
    // Finds all possible words given our state and hand
    let rec findWordReal (st: State.state) (word: char list) (wordList : (char list) list) =
        MultiSet.fold (fun (wordAcc, wordListAcc) e x ->
            match Dictionary.step (Util.IdToChar e) st.dict with
            | None -> (word, wordListAcc)
            | Some (false, dict) ->
                let wordAcc1 = wordAcc@[Util.IdToChar e]
                (wordAcc, snd (findWordReal (State.mkState st.board dict st.playerNumber (MultiSet.removeSingle e st.hand) st.boardMap) wordAcc1 wordListAcc)) 
            | Some (true, dict) ->
                let wordAcc1 = wordAcc@[Util.IdToChar e]
                let wordListAcc1 = (wordAcc1 :: wordListAcc)
                (wordAcc, (snd (findWordReal (State.mkState st.board dict st.playerNumber (MultiSet.removeSingle e st.hand) st.boardMap) wordAcc1 wordListAcc1)))
        ) (word, wordList) st.hand
   
    // Takes a list of words, and returns the longest
    let wordListToBestWord (wordList : char list list) = List.fold (fun acc elem -> if (List.length elem) > (List.length acc) then elem else acc) List.Empty wordList
    
    // Combines the two previous functions to make a single function that can find a word
    let findWord (st: State.state) = wordListToBestWord(snd (findWordReal st List.Empty List.Empty))
    
    // Uses findWord and getInitialDict, to get a word using the given word as its start. 
    let findWordFromExisting (st: State.state) (word: char list) =
        let result =(findWord (State.mkState st.board (getInitialDict st word) st.playerNumber st.hand st.boardMap))
        debugPrint(sprintf "findWordFromExistingDebug: %A" result)
        result
         
    // Uses the starting letter and a direction to return a coord i in that direction
    let getPlacement (i : int) (direction : originDirection) (start : coord) =
        match direction with
            | originDirection.right -> coord(fst start + i , snd start)
            | originDirection.down -> coord(fst start, snd start + i)
            
    // Takes a startLetter and state, and runs find word with that letter as start letter. It then gives each char in the word a coord and return a list of char coord, each letter and their coord. And a int, length of word
    let wordWithPlacements (startLetter : (coord*char) * originDirection) (st: State.state) =
        let result = List.fold (fun (accList, accCount) elem -> ((accList@[(getPlacement (accCount+1) (snd startLetter) (fst startLetter |> fst), elem)]), accCount + 1)) (List.Empty, 0) (findWordFromExisting st ((fst startLetter |> snd ) :: List.Empty))
        debugPrint(sprintf "CoordsResultForMove: %A" result)
        result
    // takes a word in form of coord and char list, and makes it into a prober command list we can give to server
    let wordToCommand (word : (coord*char) list) = List.fold (fun acc elem -> (coord ((fst elem |> fst) ,(fst elem |> snd)), ((Util.CharToId (snd elem)), (snd elem, Util.CharToPoint (snd elem)))) :: acc) List.Empty word
    
    // Takes a place and looks at each neighbor. Returns right if no neighbors left or right, return up if no neighbor up or down. None if neither
    let getPossibleDirection (place : coord) (st : State.state) =
        let neighborMap = neighborList st.boardMap place
        match fst (Map.find originDirection.right neighborMap) with
            | true ->
                match fst (Map.find originDirection.up neighborMap) with
                    | true -> None
                    | false ->
                        match fst (Map.find originDirection.down neighborMap) with
                            | true -> None
                            | false -> Some originDirection.down
            | false -> 
                match fst (Map.find originDirection.left neighborMap) with
                    |true -> None
                    |false -> Some originDirection.right
                                          
    // Goes through our boardMap and returns each letter and a direction (if any) we can write in  + coords                                                              
    let getPossibleStartingLetters (st: State.state) =
      Map.fold (fun acc key value ->
        match getPossibleDirection key st with
        | None -> acc
        | Some x -> ((key,value) ,x) :: acc
        ) List.Empty st.boardMap
    
    // Goes through each possible starting letter, and generates a word (the best word) with coords using wordWithPlacements for each of them 
    let wordToEachStartingLetter (st: State.state) = List.fold (fun acc elem -> (wordWithPlacements elem st) :: acc) List.Empty (getPossibleStartingLetters st)
    
    // Checks if the new word has neighbors true if it does not
    let checkWordNeighbor (placementList: (coord*char) list) (st: State.state) =
        List.fold (fun (boolAcc, countAcc) value ->
            match countAcc with
            | 0 -> if (Map.fold (fun acc key value -> if (fst value) then acc+1 else acc) 0 (neighborList st.boardMap (fst value))) > 1
                   then (false, countAcc+1)
                   else (true, countAcc+1) 
            | n -> if (Map.fold (fun acc key value -> if (fst value) then acc+1 else acc) 0 (neighborList st.boardMap (fst value))) > 0
                   then (false, countAcc+1)
                   else (true, countAcc+1)
            ) (false, 0) placementList       
   
    // Takes the longest word from wordToEachStartingLetter 
    let bestWord (st : State.state) = List.fold (fun acc elem -> if (snd elem) > List.length acc && (fst (checkWordNeighbor (fst elem) st)) then fst elem else acc) List.Empty (wordToEachStartingLetter st)
    
    // Takes the longest word in findWord
    let bestWordNoStart (st : State.state) = List.fold (fun (accList, accNr) elem -> (coord(accNr,0) ,elem) :: accList, accNr+1 ) (List.Empty,0) (findWord st)
    
    // uses the correct of the two previous and wordToCommand to give a move we can send straight to the server   
    let myMove (st : State.state) = if st.boardMap.IsEmpty then wordToCommand (fst (bestWordNoStart st)) else wordToCommand (bestWord st)
        
module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            //let input =  System.Console.ReadLine()
            let myMove = Bot.myMove st
            debugPrint (sprintf "Attempted move: %A" myMove)
            let move = myMove

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let removeFromHand (letters: (coord * (uint32 * (char*int))) list) (st: State.state) =
                    List.fold (fun acc value -> MultiSet.removeSingle (snd value |> fst) acc) st.hand letters
                let addToHand (a : (uint32*uint32) list) (b : State.state) (letters: (coord * (uint32 * (char*int))) list) = a |> List.fold (fun acc value -> MultiSet.add (fst value) (snd value) acc) (removeFromHand letters b)
                let addToMap (a : (coord * (uint32 * (char * int))) list) (b : State.state) = List.fold (fun acc value -> Map.add (fst value) (snd value |> snd |> fst) acc) b.boardMap a
                let st' = State.mkState st.board st.dict st.playerNumber (addToHand newPieces st ms) (addToMap ms st) // This state needs to be update
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let addToMap (a : (coord * (uint32 * (char * int))) list) (b : State.state) = List.fold (fun acc value -> Map.add (fst value) (snd value |> snd |> fst) acc) b.boardMap a
                let st' = State.mkState st.board st.dict st.playerNumber st.hand (addToMap ms st) // This state needs to be update
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        
        //let customHand = [(9u, 1u); (18u, 1u); (1u, 1u)]

        //let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty customHand

        //debugPrint (sprintf "words = %A" (snd (Bot.findWordReal (State.mkState board dict playerNumber handSet Map.empty) List.Empty List.Empty)))

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty)
    