-- | Module for computing "groups" of a list structure.

module Grouping exposing (..)

import Dict exposing (Dict)
import Dict.Extra exposing (insertDedupe)
import Missing exposing (..)


type alias Group a x =
    Dict a (List x)


type alias Group2 a b x =
    Dict a (Dict b (List x))


type alias Group3 a b c x =
    Dict a (Dict b (Dict c (List x)))


groupToMapBy : (x -> comparable) -> List x -> Group comparable x
groupToMapBy f xs =
    let g x = insertDedupe (++) (f x) [ x ]
    in List.foldl g Dict.empty xs


groupToMapBy2 :
    (x -> comparable)
    -> (x -> comparable)
    -> List x
    -> Group2 comparable comparable x
groupToMapBy2 f g =
    Dict.map (const (groupToMapBy g)) << groupToMapBy f


groupToMapBy3 :
    (x -> comparable)
    -> (x -> comparable)
    -> (x -> comparable)
    -> List x
    -> Group3 comparable comparable comparable x
groupToMapBy3 f g h =
    Dict.map (const (groupToMapBy2 g h)) << groupToMapBy f
