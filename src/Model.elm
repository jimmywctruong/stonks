module Model exposing
    ( Model
    , Stock
    , StockInput
    , clearNewStock
    , empty
    , fromList
    , get
    , insert
    , member
    , remove
    , replace
    , toList
    , toStockViewList
    , updateStockInput
    )

import Dict exposing (Dict)
import OrderedDict exposing (OrderedDict, empty)


type alias Model =
    { stocks : OrderedDict StockKey StockView
    , stockNames : Dict Name StockKey
    , newStock : StockInput
    , nextKey : StockKey
    }


type alias StockKey =
    Int


type alias StockInput =
    { name : String
    , percent : String
    }


type alias StockView =
    { stockInput : StockInput
    , stock : Stock
    }


type alias Stock =
    { name : Name
    , percent : Float
    }


type alias Name =
    String


member : Stock -> Model -> Bool
member stock model =
    case Dict.get stock.name model.stockNames of
        Just key ->
            OrderedDict.member key model.stocks

        Nothing ->
            False


get : Stock -> Model -> Maybe Stock
get stock model =
    Dict.get stock.name model.stockNames
        |> Maybe.andThen
            (\key ->
                OrderedDict.get key model.stocks
                    |> Maybe.map (\v -> v.stock)
            )


insert : Stock -> Model -> Model
insert stock model =
    let
        stockView =
            stockViewFromStock stock
    in
    case Dict.get stock.name model.stockNames of
        Just key ->
            { model
                | stocks =
                    model.stocks
                        |> OrderedDict.insert key stockView
            }

        Nothing ->
            let
                newKey =
                    model.nextKey
            in
            { model
                | stocks =
                    model.stocks
                        |> OrderedDict.insert newKey stockView
                , stockNames =
                    model.stockNames
                        |> Dict.insert stock.name newKey
                , nextKey = model.nextKey + 1
            }


remove : Stock -> Model -> Model
remove stock model =
    case Dict.get stock.name model.stockNames of
        Just key ->
            { model
                | stocks = OrderedDict.remove key model.stocks
                , stockNames = Dict.remove stock.name model.stockNames
            }

        Nothing ->
            model


empty : Model
empty =
    Model OrderedDict.empty Dict.empty (StockInput "" "") 1


replace : Stock -> Stock -> Model -> Model
replace old new model =
    case Dict.get old.name model.stockNames of
        Just key ->
            if old.name == new.name then
                insert new model

            else
                { model
                    | stocks = OrderedDict.insert key (stockViewFromStock new) model.stocks
                    , stockNames =
                        model.stockNames
                            |> Dict.insert new.name key
                            |> Dict.remove old.name
                }

        Nothing ->
            model


updateStockInput : Stock -> StockInput -> Model -> Model
updateStockInput stock input model =
    Dict.get stock.name model.stockNames
        |> Maybe.map
            (\key ->
                { model
                    | stocks =
                        OrderedDict.insert key (StockView input stock) model.stocks
                }
            )
        |> Maybe.withDefault model


stockViewFromStock : Stock -> StockView
stockViewFromStock stock =
    let
        stockInput =
            StockInput stock.name (String.fromFloat stock.percent)
    in
    StockView stockInput stock


fromList : List Stock -> Model
fromList stocks =
    let
        stockViews =
            stocks
                |> List.indexedMap Tuple.pair
                |> List.map (\( k, v ) -> ( k, StockView (StockInput v.name (String.fromFloat v.percent)) v ))

        stockNames =
            stocks
                |> List.indexedMap Tuple.pair
                |> List.map (\( k, v ) -> ( v.name, k ))
    in
    Model (OrderedDict.fromList stockViews) (Dict.fromList stockNames) (StockInput "" "") (List.length stocks)


toList : Model -> List Stock
toList model =
    model
        |> toStockViewList
        |> List.map (\s -> s.stock)


toStockViewList : Model -> List StockView
toStockViewList model =
    OrderedDict.toList model.stocks


clearNewStock : Model -> Model
clearNewStock model =
    { model | newStock = StockInput "" "" }
