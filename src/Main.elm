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
    = UpdateStockEntry StockKey StockInput KeyboardCode
    | NewStockEntry KeyboardCode
    | UpdateStockInput StockKey StockInput
    | UpdateNewStock StockInput
    | Remove StockKey


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UpdateStockEntry key stockInput code ->
            if code == 13 then
                case String.toFloat stockInput.percent of
                    Just percent ->
                        let
                            stock =
                                Stock stockInput.name percent
                        in
                        case Dict.get key model.stocks of
                            Just stockView ->
                                if key == stock.name then
                                    ( insertStockView (StockView stockView.index stockView.stockInput stock) model, Cmd.none )

                                else
                                    ( { model
                                        | stocks =
                                            model.stocks
                                                |> Dict.remove key
                                                |> Dict.insert stock.name (StockView stockView.index (StockInput stock.name (String.fromFloat stock.percent)) stock)
                                      }
                                    , Cmd.none
                                    )

                            Nothing ->
                                ( model, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        UpdateStockInput key stockInput ->
            case Dict.get key model.stocks of
                Just stockView ->
                    let
                        updatedStockView =
                            StockView stockView.index stockInput stockView.stock
                    in
                    ( { model | stocks = Dict.insert key updatedStockView model.stocks }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateNewStock stockInput ->
            ( { model | newStock = stockInput }, Cmd.none )

        NewStockEntry code ->
            -- enter keycode is 13
            if code == 13 then
                case Dict.get model.newStock.name model.stocks of
                    Just stockView ->
                        let
                            name =
                                model.newStock.name

                            strPercent =
                                model.newStock.percent
                        in
                        case String.toFloat strPercent of
                            Just percent ->
                                ( insertStockView (StockView stockView.index (StockInput name strPercent) (Stock name percent)) model, Cmd.none )

                            Nothing ->
                                ( addStock model, Cmd.none )

                    Nothing ->
                        ( addStock model, Cmd.none )

            else
                ( model, Cmd.none )

        Remove key ->
            ( { model | stocks = Dict.remove key model.stocks }, Cmd.none )


insertStockView : StockView -> Model -> Model
insertStockView stockView model =
    { model | stocks = Dict.insert stockView.stock.name stockView model.stocks, newStock = StockInput "" "" }


addStock : Model -> Model
addStock model =
    case String.toFloat model.newStock.percent of
        Just percent ->
            let
                stock =
                    Stock model.newStock.name percent
            in
            { model
                | stocks = Dict.insert stock.name (stockViewFromStock (Dict.size model.stocks) stock) model.stocks
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
    model.stocks
        |> Dict.toList
        |> List.sortBy (\t -> (Tuple.second t).index)
        |> List.map
            (\t ->
                let
                    key =
                        Tuple.first t

                    stock =
                        (Tuple.second t).stock

                    stockInput : StockInput
                    stockInput =
                        (Tuple.second t).stockInput
                in
                div []
                    [ input
                        [ onKeyDown (UpdateStockEntry key stockInput)
                        , onInput (\name -> UpdateStockInput key (StockInput name (String.fromFloat stock.percent)))
                        , value stockInput.name
                        ]
                        []
                    , input
                        [ onKeyDown (UpdateStockEntry key stockInput)
                        , onInput (\percent -> UpdateStockInput key (StockInput stock.name percent))
                        , value stockInput.percent
                        ]
                        []
                    , button [ onClick (Remove key) ] [ text "❌" ]
                    ]
            )
