module View.Day exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Util.Time exposing (Date)


{-| Don't modify
-}
type alias DailyData =
    { date : Date
    , highTemp : Maybe Float
    , lowTemp : Maybe Float
    , totalPrecipitaion : Float
    }


{-| Generates Html based on `DailyData`

Some relevant functions:

  - `Util.Time.formatDate`

-}

getFloatTemp : Maybe Float -> String
getFloatTemp float = 
  case float of
      Just a -> String.fromFloat a
      Nothing -> "unavailable"

view : DailyData -> Html msg
view daily  =
    div [class "day"]
    [
      h3 [class "day-date", style "color" "red"] [text <| Util.Time.formatDate daily.date]
    , h3 [] [i [] [text "Maximum Temperature"]]
    , h3 [class "day-hightemp"] [text <| getFloatTemp daily.highTemp]
    , h3 [] [i [] [text "Minimum Temperature"]]
    , h3 [class "day-lowtemp"] [text <| getFloatTemp daily.lowTemp]
    , h3 [] [i [] [text "Precipitations"]]
    , h3 [class "day-precipitation"] [text <| String.fromFloat daily.totalPrecipitaion]
    ]

viewFirstLast : DailyData -> DailyData ->  Html msg
viewFirstLast first last = 
  div [class "day"]
    [
       h2 [class "day-date"] [text <| (Util.Time.formatDate first.date) ++ "-" ++ (Util.Time.formatDate last.date)]
    , h3 [] [i [] [text "Maximum Temperature"]]
    , h3 [class "day-hightemp"] [text <| getFloatTemp first.highTemp]
    , h3 [] [i [] [text "Minimum Temperature"]]
    , h3 [class "day-lowtemp"] [text <| getFloatTemp first.lowTemp]
    , h3 [] [i [] [text "Precipitations"]]
    , h3 [class "day-precipitation"] [text <| String.fromFloat first.totalPrecipitaion]
    , h3 [] [i [] [text "Date"]]
    --, h2 [class "day-date"] [text <| Util.Time.formatDate last.date]
    , h3 [] [i [] [text "Maximum Temperature"]]
    , h3 [class "day-hightemp"] [text <| getFloatTemp last.highTemp]
    , h3 [] [i [] [text "Minimum Temperature"]]
    , h3 [class "day-lowtemp"] [text <| getFloatTemp last.lowTemp]
    , h3 [] [i [] [text "Precipitations"]]
    , h3 [class "day-precipitation"] [text <| String.fromFloat last.totalPrecipitaion]
    ]
