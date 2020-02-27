module Main exposing (Model)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import List
import Maybe exposing (..)
import Tuple exposing (..)


main : Program Int Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : Int -> ( Model, Cmd msg )
init _ =
    ( { stocks =
            initialStockView
      , newStock = StockInput "" ""
      }
    , Cmd.none
    )


initialStockView : Dict StockKey StockView
initialStockView =
    initialStocks
        |> List.indexedMap (\index stock -> stockViewFromStock index stock)
        |> List.map (\stockView -> ( stockView.stock.name, stockView ))
        |> Dict.fromList


stockViewFromStock : Index -> Stock -> StockView
stockViewFromStock index stock =
    StockView index (StockInput stock.name (String.fromFloat stock.percent)) stock


initialStocks : List Stock
initialStocks =
    [ Stock "VCN" 0.3, Stock "XAW" 0.6, Stock "ZAG" 0.3, Stock "Berry" 0.3 ]



-- Model


type alias Model =
    { stocks : Dict StockKey StockView
    , newStock : StockInput
    }


type alias Index =
    Int


type alias KeyboardCode =
    Int


type alias StockKey =
    String


type alias IndexedStock =
    { positionIndex : Index
    , stock : Stock
    }


type alias StockView =
    { index : Index
    , stockInput : StockInput
    , stock : Stock
    }


type alias StockInput =
    { name : String
    , percent : String
    }


type alias Stock =
    { name : String
    , percent : Float
    }



-- Update


type Msg
    = UpdateStock StockKey IndexedStock
    | EnterNewStock KeyboardCode
    | UpdateNewStockName String
    | UpdateNewStockPercent String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UpdateStock key indexedStock ->
            if key == indexedStock.stock.name then
                ( insertIndexedStock indexedStock model, Cmd.none )

            else
                ( { model
                    | stocks =
                        model.stocks
                            |> Dict.remove key
                            |> Dict.insert indexedStock.stock.name indexedStock
                  }
                , Cmd.none
                )

        UpdateNewStockName name ->
            ( { model | newStock = StockInput name model.newStock.percent }, Cmd.none )

        UpdateNewStockPercent percent ->
            ( { model | newStock = StockInput model.newStock.name percent }, Cmd.none )

        EnterNewStock code ->
            -- enter keycode is 13
            if code == 13 then
                case Dict.get model.newStock.name model.stocks of
                    Just stock ->
                        case String.toFloat model.newStock.percent of
                            Just percent ->
                                ( insertIndexedStock (IndexedStock stock.positionIndex (Stock model.newStock.name percent)) model, Cmd.none )

                            Nothing ->
                                ( addStock model, Cmd.none )

                    Nothing ->
                        ( addStock model, Cmd.none )

            else
                ( model, Cmd.none )


insertIndexedStock : IndexedStock -> Model -> Model
insertIndexedStock indexedStock model =
    { model | stocks = Dict.insert indexedStock.stock.name indexedStock model.stocks, newStock = StockInput "" "" }


addStock : Model -> Model
addStock model =
    case String.toFloat model.newStock.percent of
        Just percent ->
            let
                stock =
                    Stock model.newStock.name percent
            in
            { model
                | stocks = Dict.insert (stockViewFromStock model.stocks.size stock) model.stocks
                , newStock = StockInput "" ""
            }

        Nothing ->
            model



-- View


view : Model -> Document Msg
view model =
    Document "Stonks App" [ stockTable model ]


stockTable : Model -> Html Msg
stockTable model =
    div []
        [ div []
            [ div []
                [ text "Name" ]
            , div
                []
                [ text "Percentage" ]
            ]
        , div [] (renderModel model)
        , div [] (addStockView model)
        ]


addStockView : Model -> List (Html Msg)
addStockView model =
    [ div []
        [ input [ onKeyDown EnterNewStock, onInput UpdateNewStockName, placeholder "Ticker Symbol", value model.newStock.name ] []
        , input [ onKeyDown EnterNewStock, onInput UpdateNewStockPercent, placeholder "0.0", value model.newStock.percent ] []
        ]
    ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown keyCaptor =
    on "keydown" (D.map keyCaptor keyCode)


renderModel : Model -> List (Html Msg)
renderModel model =
    model.stocks
        |> Dict.toList
        |> List.sortBy (\t -> (Tuple.second t).positionIndex)
        |> List.map (\t -> (Tuple.second t).stock)
        |> List.map
            (\s ->
                div []
                    [ input [ value s.name ] []
                    , input [ value (String.fromFloat s.percent) ] []
                    ]
            )
