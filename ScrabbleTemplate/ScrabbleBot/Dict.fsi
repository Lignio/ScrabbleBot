module internal Dictionary

type Dict


val empty : unit -> Dict

val insert : string -> Dict -> Dict

val  lookup : string -> Dict -> bool

//val step : char -> Dict<'a> -> (bool * Dict<'a>) option 