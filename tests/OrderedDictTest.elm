module OrderedDictTest exposing (suite)

import Expect exposing (..)
import Fuzz exposing (..)
import OrderedDict exposing (..)
import Test exposing (..)


type alias Item comparable v =
    { key : comparable
    , value : v
    }


suite : Test
suite =
    describe "OrderedDict"
        [ describe "empty"
            [ fuzz2 string int "are equal to each other" <|
                \x y ->
                    let
                        item =
                            Item x y
                    in
                    OrderedDict.empty
                        |> OrderedDict.insert item.key item.value
                        |> OrderedDict.remove item.key
                        |> Expect.equal OrderedDict.empty
            ]
        , describe "fromList"
            [ fuzz3 string string string "to list is the same" <|
                \a b c ->
                    let
                        items =
                            [ ( 1, a ), ( 2, b ), ( 3, c ) ]
                    in
                    OrderedDict.fromList items
                        |> OrderedDict.toList
                        |> Expect.equal items
            , fuzz3 string string string "is identical to individual insertions" <|
                \a b c ->
                    let
                        item1 =
                            ( 1, a )

                        item2 =
                            ( 2, b )

                        item3 =
                            ( 3, c )

                        items =
                            [ item1, item2, item3 ]

                        expected =
                            OrderedDict.empty
                                |> OrderedDict.insert (Tuple.first item1) (Tuple.second item1)
                                |> OrderedDict.insert (Tuple.first item2) (Tuple.second item2)
                                |> OrderedDict.insert (Tuple.first item3) (Tuple.second item3)
                    in
                    OrderedDict.fromList items
                        |> Expect.equal expected
            ]
        , describe "get"
            [ fuzz2 int string "We can get an item that was inserted" <|
                \x y ->
                    let
                        item =
                            Item x y
                    in
                    OrderedDict.empty
                        |> OrderedDict.insert item.key item.value
                        |> OrderedDict.get item.key
                        |> Expect.equal (Just item.value)
            ]
        , describe "insert"
            [ fuzz3 string int int "an existing key updates the value" <|
                \s x y ->
                    OrderedDict.empty
                        |> OrderedDict.insert s x
                        |> OrderedDict.insert s y
                        |> OrderedDict.get s
                        |> Expect.equal (Just y)
            , fuzz3 string int int "an existing key does not affect size" <|
                \s x y ->
                    OrderedDict.empty
                        |> OrderedDict.insert s x
                        |> OrderedDict.insert s y
                        |> OrderedDict.size
                        |> Expect.equal 1
            ]
        , describe "isEmpty"
            [ test "An empty OrderedDict is empty" <|
                \_ ->
                    OrderedDict.empty
                        |> OrderedDict.isEmpty
                        |> Expect.true "Expected an empty OrderedDict to be empty"
            , fuzz2 int int "An OrderedDict with a value is not empty" <|
                \x y ->
                    OrderedDict.empty
                        |> OrderedDict.insert x y
                        |> OrderedDict.isEmpty
                        |> Expect.false "Expected not be empty"
            ]
        , describe "member"
            [ fuzz2 string string "is true for existing keys" <|
                \a b ->
                    OrderedDict.empty
                        |> OrderedDict.insert a b
                        |> OrderedDict.member a
                        |> Expect.true "should contain key"
            , fuzz int "is false for non-existent keys" <|
                \i ->
                    OrderedDict.empty
                        |> OrderedDict.member i
                        |> Expect.false "should not contain key"
            ]
        , describe "remove"
            [ fuzz2 string int "An item that is removed cannot be retrieved" <|
                \x y ->
                    let
                        item =
                            Item x y
                    in
                    OrderedDict.empty
                        |> OrderedDict.insert item.key item.value
                        |> OrderedDict.remove item.key
                        |> OrderedDict.get item.key
                        |> Expect.equal Nothing
            , fuzz int "A non-existent key" <|
                \x ->
                    OrderedDict.empty
                        |> OrderedDict.remove x
                        |> Expect.equal OrderedDict.empty
            , fuzz3 int int int "A key in the middle" <|
                \x y z ->
                    let
                        item1 =
                            ( 0, x )

                        item2 =
                            ( 1, y )

                        item3 =
                            ( 2, z )

                        items =
                            [ item1, item2, item3 ]

                        expected =
                            OrderedDict.fromList [ item1, item3 ]
                    in
                    OrderedDict.fromList items
                        |> OrderedDict.remove (Tuple.first item2)
                        |> Expect.equal expected
            ]
        , describe "size"
            [ test "no items" <|
                \_ ->
                    OrderedDict.empty
                        |> OrderedDict.size
                        |> Expect.equal 0
            , fuzz2 string string "single item" <|
                \x y ->
                    let
                        item =
                            Item x y
                    in
                    OrderedDict.empty
                        |> OrderedDict.insert item.key item.value
                        |> OrderedDict.size
                        |> Expect.equal 1
            , fuzz2 int int "multiple items" <|
                \x y ->
                    let
                        item1 =
                            Item 1 x

                        item2 =
                            Item 0 y
                    in
                    OrderedDict.empty
                        |> OrderedDict.insert item1.key item1.value
                        |> OrderedDict.insert item2.key item2.value
                        |> OrderedDict.size
                        |> Expect.equal 2
            ]
        , describe "toList"
            [ fuzz3 string string string "from list is the same" <|
                \a b c ->
                    let
                        item1 =
                            Item 0 a

                        item2 =
                            Item 1 b

                        item3 =
                            Item 2 c

                        original =
                            OrderedDict.empty
                                |> OrderedDict.insert item1.key item1.value
                                |> OrderedDict.insert item2.key item2.value
                                |> OrderedDict.insert item3.key item3.value
                    in
                    original
                        |> OrderedDict.toList
                        |> OrderedDict.fromList
                        |> Expect.equal original
            , fuzz3 string string string "returns the expected list" <|
                \a b c ->
                    let
                        item1 =
                            ( 1, a )

                        item2 =
                            ( 2, b )

                        item3 =
                            ( 3, c )

                        expected =
                            [ item1, item2, item3 ]
                    in
                    OrderedDict.empty
                        |> OrderedDict.insert (Tuple.first item1) (Tuple.second item1)
                        |> OrderedDict.insert (Tuple.first item2) (Tuple.second item2)
                        |> OrderedDict.insert (Tuple.first item3) (Tuple.second item3)
                        |> OrderedDict.toList
                        |> Expect.equal expected
            ]
        ]
