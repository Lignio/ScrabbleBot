module Util
let CharToId (c : char) = (uint32)(c - 'a')+1u
   

let IdToChar (i : uint32) =
    match i with
    |1u -> 'a'
    |2u -> 'b'
    |3u -> 'c'
    |4u -> 'd'
    |5u -> 'e'
    |6u -> 'f'
    |7u -> 'g'
    |8u -> 'h'
    |9u -> 'i'
    |10u -> 'j'
    |11u -> 'k'
    |12u -> 'l'
    |13u -> 'm'
    |14u -> 'n'
    |15u -> 'o'
    |16u -> 'p'
    |17u -> 'q'
    |18u -> 'r'
    |19u -> 's'
    |20u -> 't'
    |21u -> 'u'
    |22u -> 'v'
    |23u -> 'w'
    |24u -> 'x'
    |25u -> 'y'
    |26u -> 'z'