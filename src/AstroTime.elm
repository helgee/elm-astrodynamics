module AstroTime exposing
    ( Time
    , fromHourMinuteSecond
    , getHour
    , getMillisecond
    , getMinute
    , getSecond
    , getWholeSecond
    , midnight
    , noon
    , toString
    )

import Result exposing (Result)


secondsPerDay : Int
secondsPerDay =
    86400


minutesPerDay : Int
minutesPerDay =
    1440


hoursPerDay : Int
hoursPerDay =
    24


type Time
    = Time { hour : Int, minute : Int, second : Float }


midnight : Time
midnight =
    Time { hour = 0, minute = 0, second = 0.0 }


noon : Time
noon =
    Time { hour = 12, minute = 0, second = 0.0 }


getHour : Time -> Int
getHour (Time { hour, minute, second }) =
    hour


getMinute : Time -> Int
getMinute (Time { hour, minute, second }) =
    minute


getSecond : Time -> Float
getSecond (Time { hour, minute, second }) =
    second


getWholeSecond : Time -> Int
getWholeSecond time =
    getSecond time
        |> floor


getMillisecond : Time -> Int
getMillisecond time =
    getWholeSecond time
        |> toFloat
        |> (-) (getSecond time)
        |> (*) 1000.0
        |> round


toString : Time -> String
toString time =
    String.padLeft 2 '0' (String.fromInt (getHour time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (getMinute time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (getWholeSecond time))
        ++ "."
        ++ String.padLeft 3 '0' (String.fromInt (getMillisecond time))


validHour : Int -> Result String Int
validHour hour =
    if hour < 0 || hour > 23 then
        Err "hour must be an integer between 0 and 23."

    else
        Ok hour


validMinute : Int -> Result String Int
validMinute minute =
    if minute < 0 || minute > 59 then
        Err "minute must be an integer between 0 and 59."

    else
        Ok minute


validSecond : Float -> Result String Float
validSecond second =
    if second < 0 || second >= 61.0 then
        Err "second must be a float between 0 and 61."

    else
        Ok second


fromHMS : Int -> Int -> Float -> Time
fromHMS hour minute second =
    Time { hour = hour, minute = minute, second = second }


fromHourMinuteSecond : Int -> Int -> Float -> Result String Time
fromHourMinuteSecond hour minute second =
    Result.map3 fromHMS (validHour hour) (validMinute minute) (validSecond second)
