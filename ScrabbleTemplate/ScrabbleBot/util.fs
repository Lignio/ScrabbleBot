module Util
let CharToId (c : char) = (uint32)(c - 'a')+1u
   

let IdToChar (i : uint32) =
    match i with
    |1u -> 'A'
    |2u -> 'B'
    |3u -> 'C'
    |4u -> 'D'
    |5u -> 'E'
    |6u -> 'F'
    |7u -> 'G'
    |8u -> 'H'
    |9u -> 'I'
    |10u -> 'J'
    |11u -> 'K'
    |12u -> 'L'
    |13u -> 'M'
    |14u -> 'N'
    |15u -> 'O'
    |16u -> 'P'
    |17u -> 'Q'
    |18u -> 'R'
    |19u -> 'S'
    |20u -> 'T'
    |21u -> 'U'
    |22u -> 'V'
    |23u -> 'W'
    |24u -> 'X'
    |25u -> 'Y'
    |26u -> 'Z'