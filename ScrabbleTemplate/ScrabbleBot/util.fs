module Util
let CharToId (c : char) =
    match c with
    |'A'-> 1u
    |'B' -> 2u
    |'C' -> 3u
    |'D' -> 4u
    |'E' -> 5u
    |'F' -> 6u
    |'G' -> 7u
    |'H' -> 8u
    |'I' -> 9u
    |'J' -> 10u
    |'K' -> 11u
    |'L' -> 12u
    |'M' -> 13u
    |'N' -> 14u
    |'O' -> 15u
    |'P' -> 16u
    |'Q' -> 17u
    |'R' -> 18u
    |'S' -> 19u
    |'T' -> 20u
    |'U' -> 21u
    |'V' -> 22u
    |'W' -> 23u
    |'X' -> 24u
    |'Y' -> 25u
    |'Z' -> 26u
   

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