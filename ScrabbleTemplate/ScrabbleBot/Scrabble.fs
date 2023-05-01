﻿namespace ScrapBot

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
        
    let addToPossiblePlacements (possiblePlacement : Map<coord,originDirection>) (neighborList : Map<originDirection, (bool*coord)>) = Map.fold (fun acc key value -> if fst value = false then Map.add (snd value) key acc else acc) possiblePlacement neighborList
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
                                                                                                                        
    let trueNeighborList (boardMap: Map<coord, char>) (location : coord) (neighborMap : Map<originDirection,(bool*coord)>) = neighborMap |> Map.add originDirection.right (neighborHasValue boardMap location originDirection.right) |>
                                                                                                                             Map.add originDirection.down (neighborHasValue boardMap location originDirection.down) |>
                                                                                                                             Map.add originDirection.left (neighborHasValue boardMap location originDirection.left) |>
                                                                                                                             Map.add originDirection.up (neighborHasValue boardMap location originDirection.up)
    
    let neighborList (boardMap: Map<coord, char>) (location : coord) = trueNeighborList boardMap location Map.empty
     
    let possiblePlacements (boardMap: Map<coord, char>) = Map.fold (fun acc key value -> addToPossiblePlacements acc (neighborList boardMap key)) Map.empty boardMap
    
    let getParent (possiblePlacement: (coord*originDirection)) (boardMap : Map<coord, char>) =
        match snd possiblePlacement with
        | originDirection.left -> (coord((fst possiblePlacement|> fst)-1 , (fst possiblePlacement |> snd)), Map.find ((fst possiblePlacement|> fst)-1 , (fst possiblePlacement |> snd)) boardMap)
        | originDirection.right -> (coord((fst possiblePlacement|> fst)+1 , (fst possiblePlacement |> snd)),Map.find ((fst possiblePlacement|> fst)+1 , (fst possiblePlacement |> snd)) boardMap)
        | originDirection.up -> (coord((fst possiblePlacement|> fst) , (fst possiblePlacement |> snd)+1) ,Map.find ((fst possiblePlacement|> fst) , (fst possiblePlacement |> snd)+1) boardMap)
        | originDirection.down -> (coord((fst possiblePlacement|> fst) , (fst possiblePlacement |> snd)-1),Map.find ((fst possiblePlacement|> fst) , (fst possiblePlacement |> snd)-1) boardMap)
    let rec findMoveReal (possiblePlacement : (coord*originDirection)) (st : State.state) (myWord : (coord*char) list) =
        match myWord with
            | myWord when Dictionary.lookup (myWord |> List.map snd |> List.toArray |> System.String) st.dict = true -> myWord
            | myWord when myWord = List.empty -> 
                match Dictionary.step (getParent possiblePlacement st.boardMap |> snd) st.dict with
                    | None -> List.empty 
                    | Some x -> findMoveReal possiblePlacement (State.mkState st.board (snd x) st.playerNumber st.hand st.boardMap) ((getParent possiblePlacement st.boardMap) :: myWord)
            | myWord -> MultiSet.fold (fun acc e x ->
                match Dictionary.step (Util.IdToChar e) st.dict with
                    | None -> List.empty
                    | Some (false, x) -> findMoveReal possiblePlacement (State.mkState st.board x st.playerNumber (MultiSet.remove (getParent possiblePlacement st.boardMap |> snd |> Util.CharToId) 1u st.hand) st.boardMap) ((getParent possiblePlacement st.boardMap) :: myWord)
                    | Some (true, x) -> myWord //findMoveReal possiblePlacement (State.mkState st.board x st.playerNumber (MultiSet.remove (getParent possiblePlacement st.boardMap |> snd |> Util.CharToId) 1u st.hand) st.boardMap) ((getParent possiblePlacement st.boardMap) :: myWord)
                        ) myWord st.hand
    let findMove (possiblePlacement : (coord*originDirection)) (st : State.state) = findMoveReal possiblePlacement st List.empty
    
    // Makes state from initial word
    let getInitialDict (st: State.state) (word: char list) =
        List.fold (fun dictAcc value ->
            match Dictionary.step (value) dictAcc with
            | None -> st.dict
            | Some (_, dict) -> dict
            ) st.dict word
    
    // can take empty word and wordlist as well as state to return worList with all words using letters in hand
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
   
    let wordListToBestWord (wordList : char list list) = List.fold (fun acc elem -> if (List.length elem) > (List.length acc) then elem else acc) List.Empty wordList
    let findWord (st: State.state) = wordListToBestWord(snd (findWordReal st List.Empty List.Empty))
    let findWordFromExisting (st: State.state) (word: char list) =(findWord (State.mkState st.board (getInitialDict st word) st.playerNumber st.hand st.boardMap))
    
    let findMoves (possiblePlacements : Map<coord,originDirection>) (st : State.state) =
       Map.fold (fun acc key value -> (findMove (key,value) st) :: acc) List.empty possiblePlacements
    
    let chooseMove (moves : (coord * char) list list) = List.head moves 
    
    let getPlacement (i : int) (direction : originDirection) (start : coord) =
        match direction with
            | originDirection.right -> coord(fst start + i , snd start)
            | originDirection.down -> coord(fst start, snd start + i)
            
    // take a startLetter and state, and runs find word with that letter as start letter. It then gives each char in the word a coord and return a list of char coord, each letter and their coord. And a int, length of word
    // DO NOT DELETE
    //let wordWithPlacements (startLetter : (coord*char) * originDirection) (st: State.state) =  List.fold (fun (accList, accCount) elem -> if accCount = 0 then (((fst startLetter) :: accList), (accCount + 1)) else (((getPlacement accCount (snd startLetter) (fst startLetter |> fst), elem) :: accList), accCount + 1)) (list.Empty, 0) (findWord st ((fst startLetter |> snd ) :: List.empty))
    
    let wordToCommand (word : (coord*char) list) = List.fold (fun acc elem -> (((fst elem |> fst) ,(fst elem |> snd)), ((Util.CharToId (snd elem)), (snd elem, 1))) :: acc) List.Empty word
    
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
                                                                    
    let getPossibleStartingLetters (st: State.state) = Map.fold (fun acc key value -> ((key,value) ,getPossibleDirection key st) :: acc ) List.empty st.boardMap
    // DO NOT DELETE
   // let allPossibleMoves (st: State.state) =
     //   List.fold (fun acc elem ->
       //     match snd elem with
         //       | None -> acc
           //     | Some x -> (((wordWithPlacements (fst elem, x) st) |> fst) |> wordToCommand) :: acc
        //) List.empty (getPossibleStartingLetters st)
        
module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let addToHand (a : (uint32 * uint32) list) (b : State.state) = a |> List.fold (fun acc value -> MultiSet.add (fst value) (snd value) acc) b.hand
                let addToMap (a : (coord * (uint32 * (char * int))) list) (b : State.state) = List.fold (fun acc value -> Map.add (fst value) (snd value |> snd |> fst) acc) b.boardMap a
                let st' = State.mkState st.board st.dict st.playerNumber (addToHand newPieces st) (addToMap ms st) // This state needs to be update
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
                  
        //let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        
        let customHand = [(9u, 1u); (18u, 1u); (1u, 1u)]

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty customHand

        debugPrint (sprintf "words = %A" (snd (Bot.findWordReal (State.mkState board dict playerNumber handSet Map.empty) List.Empty List.Empty)))

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty)
    