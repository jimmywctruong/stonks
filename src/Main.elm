module Main exposing (Model)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import List


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
            [ Stock "VCN" 0.3
            , Stock "XAW" 0.6
            , Stock "ZAGlkdjalas" 0.3
            , Stock "Berry" 0.3
            ]
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { stocks : List Stock
    }


type alias Stock =
    { name : String
    , percent : Float
    }



-- Update


type Msg
    = TextUpdate String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        TextUpdate text ->
            ( model, Cmd.none )



-- View


view : Model -> Document Msg
view model =
    Document "Stonks App" (renderModel model)


renderModel : Model -> List (Html Msg)
renderModel model =
    model.stocks
        |> List.map (\x -> div [] [ text x.name ])
