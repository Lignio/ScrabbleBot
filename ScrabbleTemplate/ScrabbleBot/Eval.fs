module internal Eval

    open StateMonad
    open ScrabbleUtil

    (* Code for testing *)

    let hello = ['H', 4; 'E', 1; 'L', 1; 'L', 1; 'O', 1;] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b = a >>= fun (x:int) -> b >>= fun (y:int) -> ret (x+y)  
    let sub a b = a >>= fun (x:int) -> b >>= fun (y:int) -> ret (x-y)
    let mul a b = a >>= fun (x:int) -> b >>= fun (y:int) -> ret (x*y)    
    let div a b = a >>= fun x -> b >>= fun y -> if y <> 0 then ret (x/y) else fail DivisionByZero
    let modu a b = a >>= fun x -> b >>= fun y -> if y <> 0 then ret (x/y) else fail DivisionByZero    
    let equal a b = a >>= fun (x:int) -> b >>= fun (y:int) -> ret (x=y)
    let lessThan a b = a >>= fun (x:int) -> b >>= fun (y:int) -> ret (x<y)
    let conj a b = a >>= fun x -> b >>= fun y -> ret (x&&y)
    let vowels = ['a'; 'A'; 'e'; 'E'; 'i'; 'I'; 'o'; 'O'; 'u'; 'U'; 'y'; 'Y'; 'æ'; 'Æ'; 'ø'; 'Ø'; 'å'; 'Å';]
    let isVowel a =  List.contains a vowels

    type aExp = 
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> = 
      match a with 
        | N c -> ret c
        | V c -> lookup c 
        | WL -> wordLength
        | PV c -> arithEval c >>= fun x -> pointValue x
        | Add (c,d) -> add (arithEval c) (arithEval d)
        | Sub (c,d) -> sub (arithEval c) (arithEval d)
        | Mul (c,d) -> mul (arithEval c) (arithEval d)
        | Div (c, d) -> div (arithEval c) (arithEval d)
        | Mod (c, d) -> modu (arithEval c) (arithEval d)
        | CharToInt c -> charEval c >>= fun x -> int x |> ret

    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | ToUpper c -> charEval c >>= fun x -> System.Char.ToUpper x |> ret 
        | ToLower c -> charEval c >>= fun x -> System.Char.ToLower x |> ret
        | CV c -> arithEval c >>= fun x -> characterValue x 
        | IntToChar a -> arithEval a >>= fun x -> char x |> ret

    and boolEval b : SM<bool> =  
        match b with 
        | TT -> ret true
        | FF -> ret false
        | AEq (c, d) -> equal (arithEval c) (arithEval d)
        | ALt (c, d) -> lessThan (arithEval c) (arithEval d)
        | Not c -> boolEval c >>= fun x -> not x |> ret
        | Conj (c, d) -> conj (boolEval c) (boolEval d)
        | IsDigit c -> charEval c >>= fun x -> System.Char.IsDigit x |> ret
        | IsLetter c -> charEval c >>= fun x -> System.Char.IsLetter x |> ret
        | IsVowel c -> charEval c >>= fun x -> isVowel x |> ret

    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    