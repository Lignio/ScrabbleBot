module Util
let CharToId (c : char) =
    match c with
    |'a' -> 1u
    |'b' -> 2u
    |'c' -> 3u
    |'d' -> 4u
    |'e' -> 5u
    |'f' -> 6u
    |'g' -> 7u
    |'h' -> 8u
    |'i' -> 9u
    |'j' -> 10u
    |'k' -> 11u
    |'l' -> 12u
    |'m' -> 13u
    |'n' -> 14u
    |'o' -> 15u
    |'p' -> 16u
    |'q' -> 17u
    |'r' -> 18u
    |'s' -> 19u
    |'t' -> 20u
    |'u' -> 21u
    |'v' -> 22u
    |'w' -> 23u
    |'x' -> 24u
    |'y' -> 25u
    |'z' -> 26u

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