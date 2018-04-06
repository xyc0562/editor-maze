module Utils exposing (..)

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
