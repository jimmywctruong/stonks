module Main exposing
    ( KeyboardCode
    , Msg(..)
    , addStock
    , addStockView
    , init
    , initialModel
    , initialStocks
    , main
    , onKeyDown
    , renderModel
    , stockTable
    , subscriptions
    , update
    , view
    )

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import List
import Maybe exposing (..)
import Model exposing (..)
import OrderedDict exposing (..)
import Tuple exposing (..)


main : Program Int Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : Int -> ( Model, Cmd msg )
init _ =
    ( initialModel initialStocks
    , Cmd.none
    )


initialModel : List Stock -> Model
initialModel stocks =
    Model.fromList stocks


initialStocks : List Stock
initialStocks =
    [ Stock "VCN" 0.3, Stock "XAW" 0.6, Stock "ZAG" 0.3, Stock "Berry" 0.3 ]



-- Model


type alias KeyboardCode =
    Int



-- Update


type Msg
    = UpdateStockEntry Stock StockInput KeyboardCode
    | NewStockEntry KeyboardCode
    | UpdateStockInput Stock StockInput
    | UpdateNewStock StockInput
    | Remove Stock


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UpdateStockEntry stock stockInput code ->
            if code == 13 then
                case String.toFloat stockInput.percent of
                    Just percent ->
                        let
                            newStock =
                                Stock stockInput.name percent
                        in
                        ( Model.replace stock newStock model, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        UpdateStockInput stock stockInput ->
            ( Model.updateStockInput stock stockInput model, Cmd.none )

        UpdateNewStock stockInput ->
            ( { model | newStock = stockInput }, Cmd.none )

        NewStockEntry code ->
            -- enter keycode is 13
            if code == 13 then
                case String.toFloat model.newStock.percent of
                    Just percent ->
                        let
                            stock =
                                Stock model.newStock.name percent
                        in
                        ( model
                            |> Model.insert stock
                            |> Model.clearNewStock
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        Remove stock ->
            ( Model.remove stock model, Cmd.none )


addStock : Model -> Model
addStock model =
    case String.toFloat model.newStock.percent of
        Just percent ->
            let
                stock =
                    Stock model.newStock.name percent
            in
            model
                |> Model.insert stock
                |> Model.clearNewStock

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
        [ input
            [ onKeyDown NewStockEntry
            , onInput (\name -> UpdateNewStock (StockInput name model.newStock.percent))
            , placeholder "Ticker Symbol"
            , value model.newStock.name
            ]
            []
        , input
            [ onKeyDown NewStockEntry
            , onInput (\percent -> UpdateNewStock (StockInput model.newStock.name percent))
            , placeholder "0.0"
            , value model.newStock.percent
            ]
            []
        ]
    ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown keyCaptor =
    on "keydown" (D.map keyCaptor keyCode)


renderModel : Model -> List (Html Msg)
renderModel model =
    model
        |> Model.toStockViewList
        |> List.map
            (\sv ->
                let
                    stock =
                        sv.stock

                    stockInput =
                        sv.stockInput
                in
                div []
                    [ input
                        [ onKeyDown (UpdateStockEntry stock stockInput)
                        , onInput (\name -> UpdateStockInput stock (StockInput name (String.fromFloat stock.percent)))
                        , value stockInput.name
                        ]
                        []
                    , input
                        [ onKeyDown (UpdateStockEntry stock stockInput)
                        , onInput (\percent -> UpdateStockInput stock (StockInput stock.name percent))
                        , value stockInput.percent
                        ]
                        []
                    , button [ onClick (Remove stock) ] [ text "❌" ]
                    ]
            )
