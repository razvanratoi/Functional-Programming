module View.WeatherItems exposing (view,MsgMap)

import Html exposing (..)
import Html.Attributes as HA exposing (class, style)
import Html.Events exposing (..)
import Model.WeatherItems exposing (SelectedWeatherItems, WeatherItem(..))
import View.WeatherChart exposing (ShownItems)
import Model


checkbox : String -> Bool -> (WeatherItem -> Bool -> msg) -> WeatherItem -> Html msg
checkbox name state msg category =
    div [ style "display" "inline", class "checkbox" ]
        [ input [ HA.type_ "checkbox", onCheck (msg category), HA.checked state ] []
        , text name
        ]


type alias MsgMap msg =
    { onChangeSelection : WeatherItem -> Bool -> msg }

opposite : Bool -> Bool
opposite b = 
    if b == True 
        then False
        else True

view : MsgMap msg -> SelectedWeatherItems -> Html msg
view msg select =
    div [] [
        checkbox "Current Time" select.currentTime (msg.onChangeSelection) CurrentTime
    ,   checkbox "Temperature" select.temperature (msg.onChangeSelection) Temperature
    ,   checkbox "Min/Max" select.minMax (msg.onChangeSelection) MinMax
    ,   checkbox "Precipitation" select.precipitation (msg.onChangeSelection) Precipitation
    ]
