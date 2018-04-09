module Utils exposing (..)
import Native.Random
import Array

flatten2D : List (List a) -> List a
flatten2D list =
  List.foldr (++) [] list

pad : Int -> String -> String -> String
pad i p s =
    if (String.length s) >= i then
        s 
    else 
        pad i p (p++s)

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)

randFirst : (a -> Bool) -> List a -> Maybe a
randFirst fn l =
    let
        arr = Array.fromList (List.filter fn l)
        idx = floor (Native.Random.rand()*(toFloat (Array.length(arr))))
    in
        Array.get idx arr


