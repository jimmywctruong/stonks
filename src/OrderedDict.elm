module OrderedDict exposing (OrderedDict, empty, get, insert, isEmpty, remove, size)

import Dict exposing (Dict)
import Maybe exposing (Maybe(..))


type alias OrderedDict comparable v =
    { dict : Dict comparable (OrderedValue comparable v)
    , tail : Maybe comparable
    }


type alias OrderedValue comparable v =
    { previous : Maybe comparable
    , value : v
    , next : Maybe comparable
    }


empty : OrderedDict comparable v
empty =
    OrderedDict Dict.empty Nothing


size : OrderedDict comparable v -> Int
size data =
    Dict.size data.dict


isEmpty : OrderedDict comparable v -> Bool
isEmpty data =
    Dict.isEmpty data.dict


get : comparable -> OrderedDict comparable v -> Maybe v
get key data =
    Dict.get key data.dict
        |> Maybe.map (\v -> v.value)


insert : comparable -> v -> OrderedDict comparable v -> OrderedDict comparable v
insert key value data =
    case Dict.get key data.dict of
        Just existing ->
            { data | dict = data.dict |> Dict.insert key { existing | value = value } }

        Nothing ->
            case data.tail of
                Just tailKey ->
                    data
                        |> updatePrevious (Just key) (Just tailKey)
                        |> appendItem key value

                Nothing ->
                    { data | dict = (data |> appendItem key value).dict, tail = Just key }


appendItem : comparable -> v -> OrderedDict comparable v -> OrderedDict comparable v
appendItem key value data =
    let
        item =
            OrderedValue data.tail value Nothing
    in
    { data
        | dict =
            data.dict
                |> Dict.insert key item
        , tail = Just key
    }


remove : comparable -> OrderedDict comparable v -> OrderedDict comparable v
remove key data =
    case Dict.get key data.dict of
        Just orderedValue ->
            let
                newDict : Dict comparable (OrderedValue comparable v)
                newDict =
                    (data
                        |> updatePrevious orderedValue.previous orderedValue.next
                        |> updateNext orderedValue.next orderedValue.previous
                    ).dict
            in
            { data
                | dict =
                    newDict
                        |> Dict.remove key
                , tail = data.tail |> updateTail orderedValue
            }

        Nothing ->
            data


updateTail : OrderedValue comparable v -> Maybe comparable -> Maybe comparable
updateTail value tail =
    if value.previous == tail then
        value.previous

    else
        tail


updatePrevious : Maybe comparable -> Maybe comparable -> OrderedDict comparable v -> OrderedDict comparable v
updatePrevious previous next orderedDict =
    case previous of
        Just previousKey ->
            case Dict.get previousKey orderedDict.dict of
                Just previousEntry ->
                    { orderedDict
                        | dict =
                            orderedDict.dict
                                |> Dict.insert previousKey { previousEntry | next = next }
                    }

                Nothing ->
                    orderedDict

        Nothing ->
            orderedDict


updateNext : Maybe comparable -> Maybe comparable -> OrderedDict comparable v -> OrderedDict comparable v
updateNext next previous orderedDict =
    case next of
        Just nextKey ->
            case Dict.get nextKey orderedDict.dict of
                Just nextEntry ->
                    { orderedDict
                        | dict =
                            orderedDict.dict
                                |> Dict.insert nextKey { nextEntry | previous = previous }
                    }

                Nothing ->
                    orderedDict

        Nothing ->
            orderedDict
