module Extras exposing (..)

import Dict exposing (Dict)
import Missing exposing (..)

toSortedList : Dict comparable b -> List ( comparable, b )
toSortedList = List.sortBy fst << Dict.toList

mapMaybe : (a -> Maybe b) -> List a -> List b
mapMaybe f xs_ =
  case xs_ of
    [] -> []
    x :: xs ->
      let rs = mapMaybe f xs
      in
        case f x of
            Nothing -> rs
            Just r -> r :: rs

catMaybes : List (Maybe a) -> List a
catMaybes = mapMaybe (\x -> x)
