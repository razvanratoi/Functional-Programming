module Util exposing (groupBy, maximumBy, maybeToList, minimumBy, zipFilter)



{-| Module containing utility functions
-}

zip : List a -> List b -> List (a, b)
zip lx ly =
  case (lx, ly) of
    (x::xs, y::ys) -> (x, y)::(zip xs ys)
    _ -> []

normalizeList : b -> List (b, a) -> (b, List a)
normalizeList bAux tupleList =
    let
        normalizeListAcc tList acc = 
            case tList of
                [] -> acc
                x::xs -> normalizeListAcc xs (bAux, List.foldl (::) [] <| (Tuple.second x)::(Tuple.second acc))
    in
        normalizeListAcc tupleList (bAux, [])

createGroupedList : List (b, a) -> List b -> List (b, List a) -> List (b, List a)
createGroupedList tList accResults accList = 
            case tList of
                [] -> accList
                (x,_)::xs -> 
                    let
                        filtered = List.filter (\(u,_) -> u == x) tList
                        normalized = normalizeList x filtered
                    in
                        if not (List.member x accResults)
                            then createGroupedList xs (x::(accResults)) (normalized::accList)
                            else createGroupedList xs accResults accList

{-| Description for minimumBy

    minimumBy .x [ { x = 1, y = 2 } ] --> Just {x = 1, y = 2}

    minimumBy .x [] --> Nothing

    minimumBy (modBy 10) [ 16, 23, 14, 5 ] --> Just 23

-}
minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f l =
    let
        theMin = List.minimum <| List.map f l
        findFromOriginalList fx lx minx = 
            case lx of
                [] -> Nothing
                x::xs -> if (f x) == minx 
                            then Just x
                            else findFromOriginalList fx xs minx
    in
        case theMin of
            Just a -> findFromOriginalList f l a
            _ -> Nothing


{-| Description for maximumBy

    maximumBy .x [ { x = 1, y = 2 } ] --> Just {x = 1, y = 2}

    maximumBy .x [] --> Nothing

    maximumBy (modBy 10) [ 16, 23, 14, 5 ] --> Just 16

-}
maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy f l =
    let
        theMax = List.maximum <| List.map f l
        findFromOriginalList fx lx maxx = 
            case lx of
                [] -> Nothing
                x::xs -> if (f x) == maxx 
                            then Just x
                            else findFromOriginalList fx xs maxx
    in
        case theMax of
            Just a -> findFromOriginalList f l a
            _ -> Nothing


{-| Group a list

    groupBy .x [ { x = 1 } ] --> [(1, [{x = 1}])]

    groupBy (modBy 10) [ 11, 12, 21, 22 ] --> [(1, [11, 21]), (2, [12, 22])]

    groupBy identity [] --> []

-}
groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy f l = 
    let
        tupleList = zip (List.map f l) <|  l
        
    in
        List.foldl (::) [] <| createGroupedList tupleList [] []


{-| Transforms a Maybe into a List with one element for Just, and an empty list for Nothing

    maybeToList (Just 1) --> [1]

    maybeToList Nothing --> []

-}
maybeToList : Maybe a -> List a
maybeToList x =
    case x of 
        Just a -> [a]
        Nothing -> []


{-| Filters a list based on a list of bools

    zipFilter [ True, True ] [ 1, 2 ] --> [1, 2]

    zipFilter [ False, False ] [ 1, 2 ] --> []

    zipFilter [ True, False, True, False ] [ 1, 2, 3, 4 ] --> [1, 3]

-}
zipFilter : List Bool -> List a -> List a
zipFilter boolList xList =
    zip boolList xList
        |> List.filter (\(b,_) -> b == True)
        |> List.map (\(_, x) -> x)
