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
            Dict.fromList
                [ ( "VCN", IndexedStock 0 (Stock "VCN" 0.3) )
                , ( "XAW", IndexedStock 1 (Stock "XAW" 0.6) )
                , ( "ZAG", IndexedStock 2 (Stock "ZAG" 0.3) )
                , ( "Berry", IndexedStock 3 (Stock "Berry" 0.3) )
                ]
      , newStock = Stock "" 0.0
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { stocks : Dict Key IndexedStock
    , newStock : Stock
    }


type alias Index =
    Int


type alias KeyboardCode =
    Int


type alias Key =
    String


type alias IndexedStock =
    { positionIndex : Index
    , stock : Stock
    }


type alias Stock =
    { name : String
    , percent : Float
    }



-- Update


type Msg
    = UpdateStock Key IndexedStock
    | KeyDown KeyboardCode
    | UpdateNewStockName String
    | UpdateNewStockPercent String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UpdateStock key indexedStock ->
            if key == indexedStock.stock.name then
                ( { model | stocks = Dict.insert key indexedStock model.stocks }, Cmd.none )

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
            ( { model | newStock = Stock name model.newStock.percent }, Cmd.none )

        UpdateNewStockPercent strPercent ->
            let
                percent =
                    Maybe.withDefault 0.0 (String.toFloat strPercent)
            in
            ( { model | newStock = Stock model.newStock.name percent }, Cmd.none )

        KeyDown code ->
            -- enter keycode is 13
            if code == 13 then
                ( addStock model, Cmd.none )

            else
                ( model, Cmd.none )


addStock : Model -> Model
addStock model =
    let
        stock =
            model.newStock
    in
    { model | stocks = Dict.insert stock.name (IndexedStock (Dict.size model.stocks) stock) model.stocks }



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
        , div [] addStockView
        ]


addStockView : List (Html Msg)
addStockView =
    [ div []
        [ input [ onKeyDown KeyDown, onInput UpdateNewStockName ] []
        , input [ onInput UpdateNewStockPercent ] []
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
