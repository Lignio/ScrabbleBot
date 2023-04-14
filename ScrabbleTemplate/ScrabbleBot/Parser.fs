module internal Parser

    open Eval
    open ScrabbleUtil
    open StateMonad

    
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open FParsecLight.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) a b =  a .>>. (spaces>>.b) <?> "combine"
    let (.>*>) a b = (a.>>spaces) .>> b <?> "combine1"
    let (>*>.) a b  = a >>. (spaces>>.b) <?> "combine2"

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' <?> "paranthesis"

    let quote p = pchar ''' >>. p .>> pchar ''' <?> "paranthesis"

    let pid: Parser<string> = pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_') |>> fun (x, y) -> System.String.Concat(Array.ofList(x::y))  

    let unop op a = op >*>. a <?> "unop"
    let binop op p1 p2 = p1 .>*> op .>*>. p2 <?> "binop"

    //p1 .>*> op >*>. p2 |>> fun x y -> (x,y) <?> "binop" incorrect (not implemented)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"

    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"

    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"

    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse;  DivParse; ModParse; AtomParse]

    let NegationParse = unop (pchar '-') AtomParse |>> (fun a -> Mul (N(-1), a))  <?> "Mul"

    let PVParse =  unop (pPointValue) (parenthesise TermParse) |>> PV <?> "PV"

    let NParse   = pint32 |>> N <?> "Int"

    let VarParse = pid |>> V <?> "Var"
    let ParParse = parenthesise TermParse

    let CharToIntParse = unop(pCharToInt) (parenthesise CharParse) |>> CharToInt <?> "CharToInt"
    do aref := choice [NegationParse; PVParse; ParParse; CharToIntParse; VarParse; NParse]

    let AexpParse = TermParse 

    let CParse = quote anyChar |>> C <?> "Char"

    let CVParse = unop (pCharValue) (parenthesise TermParse) |>> CV <?> "CV" 

    let ToUpperParse = unop (pToUpper) (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
 
    let ToLowerParse = unop (pToLower) (parenthesise CharParse) |>> ToLower <?> "ToLower"

    let IntToCharParse = unop (pIntToChar) (parenthesise TermParse) |>> IntToChar <?> "IntToChar"

    do cref := choice [CParse; CVParse; ToUpperParse; ToLowerParse; IntToCharParse]

    let CexpParse = CharParse 

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board = failwith "not implemented" 
