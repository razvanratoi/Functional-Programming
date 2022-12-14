module View.Week exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Util.Time exposing (Date, formatDate)
import View.Day exposing (DailyData)
import Util.Time exposing (..)
import Time exposing (Month(..))


type alias WeeklyData =
    { dailyData : List DailyData
    }


{-| Generates Html based on `WeeklyData`

Some relevant functions:

  - `Util.Time.formatDate`

-}
view : WeeklyData -> Html msg
view week = 
  let
    firstDay = getFirst week.dailyData
    lastDay = getLast week.dailyData
    firstLast = h2 [class "day-date"] [text <| (Util.Time.formatDate firstDay.date) ++ "-" ++ (Util.Time.formatDate lastDay.date)]
  in
  
  div [class "week"] <| 
    if List.isEmpty week.dailyData
      then [h2 [] [text "No data"]]
      else firstLast::List.map viewDays week.dailyData

viewDays : DailyData -> Html msg
viewDays day = 
  View.Day.view day

getLast : List DailyData -> DailyData
getLast l = 
  case l of
      _::xs -> 
        case xs of 
          y::[] -> y
          _ -> getLast xs
      [] -> DailyData (Date {year = 2000, month = Jan, day =  1}) Nothing Nothing 0.0

getFirst : List DailyData -> DailyData
getFirst l =
  let
      first = List.head l
  in
    case first of
        Nothing -> DailyData (Date {year = 2000, month = Jan, day =  1}) Nothing Nothing 0.0
        Just d -> d
  
  