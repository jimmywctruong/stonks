module OrderedDictTest exposing (suite)

import Expect exposing (..)
import OrderedDict exposing (..)
import Test exposing (..)


type alias Item comparable v =
    { key : comparable
    , value : v
    }


suite : Test
suite =
    describe "OrderedDict"
        [ test "An empty OrderedDict is empty" <|
            \_ ->
                OrderedDict.empty
                    |> OrderedDict.isEmpty
                    |> Expect.true "Expected an empty OrderedDict to be empty"
        , test "An OrderedDict with a value is not empty" <|
            \_ ->
                OrderedDict.empty
                    |> OrderedDict.insert 0 0
                    |> OrderedDict.isEmpty
                    |> Expect.false "Expected the OrderedDict to not be empty"
        , test "We can insert items" <|
            \_ ->
                let
                    item1 =
                        Item 0 "value"

                    item2 =
                        Item 2 "another value"
                in
                OrderedDict.empty
                    |> OrderedDict.insert item1.key item1.value
                    |> OrderedDict.insert item2.key item2.value
                    |> OrderedDict.size
                    |> Expect.equal 2
        , test "We can get an item that was inserted" <|
            let
                item =
                    Item 0 0
            in
            \_ ->
                OrderedDict.empty
                    |> OrderedDict.insert item.key item.value
                    |> OrderedDict.get item.key
                    |> Expect.equal (Just item.value)
        , test "Get nothing when we get an item that was inserted and removed" <|
            let
                item =
                    Item 4 2
            in
            \_ ->
                OrderedDict.empty
                    |> OrderedDict.insert item.key item.value
                    |> OrderedDict.remove item.key
                    |> OrderedDict.get item.key
                    |> Expect.equal Nothing
        ]
