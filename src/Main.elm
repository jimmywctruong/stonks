module Main exposing (Model)

import Browser
import Html exposing (Html, div, text)
import List


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { stocks =
        [ Stock "VCN" 0.3
        , Stock "XAW" 0.6
        , Stock "ZAG" 0.1
        ]
    }



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


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextUpdate text ->
            model



-- View


view : Model -> Html Msg
view model =
    div [] (model |> renderModel |> List.map (\name -> div [] [ text name ]))


renderModel : Model -> List String
renderModel model =
    model.stocks
        |> List.map (\x -> x.name)
