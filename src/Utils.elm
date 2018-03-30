module Utils exposing (..)

flatten2D : List (List a) -> List a
flatten2D list =
  List.foldr (++) [] list
