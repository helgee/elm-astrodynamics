module Epoch exposing (Epoch, TimeScale(..), fromComponents, fromDateTime, transform)

import AstroTime exposing (Time)
import Date exposing (Date)
import Result exposing (Result)


maxInt : Int
maxInt =
    2 ^ 31 - 1


minInt : Int
minInt =
    -2 ^ 31


offsetTaiTt : Float
offsetTaiTt =
    32.184


twoSum : Float -> Float -> ( Float, Float )
twoSum a b =
    let
        hi =
            a + b

        a1 =
            hi - b

        b1 =
            hi - a1

        lo =
            (a - a1) + (b - b1)
    in
    ( hi, lo )


adjust : Int -> Float -> Float -> ( Int, Float )
adjust seconds fraction offset =
    let
        ( sum, residual ) =
            twoSum fraction offset
    in
    if isInfinite sum then
        ( if sum < 0 then
            minInt

          else
            maxInt
        , sum
        )

    else
        let
            intSeconds =
                floor sum
        in
        ( seconds + intSeconds, sum - toFloat intSeconds + residual )


type TimeScale
    = TAI
    | TT
    | TDB


timeScaleToString : TimeScale -> String
timeScaleToString timescale =
    case timescale of
        TAI ->
            "TAI"

        TT ->
            "TT"

        TDB ->
            "TDB"


type Epoch
    = Epoch
        { timescale : TimeScale
        , seconds : Int
        , fraction : Float
        }


getTimeScale : Epoch -> TimeScale
getTimeScale (Epoch { timescale, seconds, fraction }) =
    timescale


getSeconds : Epoch -> Int
getSeconds (Epoch { timescale, seconds, fraction }) =
    seconds


getFraction : Epoch -> Float
getFraction (Epoch { timescale, seconds, fraction }) =
    fraction


fromDateTime : TimeScale -> Date -> Time -> Epoch
fromDateTime timescale date time =
    let
        seconds =
            AstroTime.getSecond time

        intSecond =
            AstroTime.getWholeSecond time

        minute =
            AstroTime.getMinute time

        hour =
            AstroTime.getHour time

        jd =
            Date.getJ2000 date

        newSeconds =
            60 * ((jd * 24 + hour) * 60 + minute - 720) + intSecond
    in
    Epoch
        { timescale = timescale
        , seconds = newSeconds
        , fraction = seconds - toFloat intSecond
        }


fromComponents : TimeScale -> Int -> Int -> Int -> Int -> Int -> Float -> Result String Epoch
fromComponents timescale year month day hour minute second =
    let
        date =
            Date.fromYearMonthDay year month day

        time =
            AstroTime.fromHourMinuteSecond hour minute second
    in
    Result.map2 (fromDateTime timescale) date time


j2000 : Int -> Float -> Float
j2000 seconds fraction =
    (fraction + toFloat seconds) / 86400


deltaEtK : Float
deltaEtK =
    1.657 * 10 ^ -3


deltaEtEb : Float
deltaEtEb =
    1.671 * 10 ^ -2


deltaEtM0 : Float
deltaEtM0 =
    6.239996


deltaEtM1 : Float
deltaEtM1 =
    1.99096871 * 10 ^ -7


offsetTtTdb : Float -> Float
offsetTtTdb tt =
    let
        g =
            deltaEtM0 + deltaEtM1 * tt
    in
    tt + deltaEtK * sin (g + deltaEtEb * sin g)


offsetTdbTtFunc : Float -> Float -> Float
offsetTdbTtFunc tdb tt =
    let
        g =
            deltaEtM0 + deltaEtM1 * tt
    in
    tdb - deltaEtK * sin (g + deltaEtEb * sin g)


offsetTdbTt : Float -> Float
offsetTdbTt tdb =
    -- Three iterations
    tdb
        |> offsetTdbTtFunc tdb
        |> offsetTdbTtFunc tdb
        |> offsetTdbTtFunc tdb


transform : TimeScale -> Epoch -> Epoch
transform scale epoch =
    case getTimeScale epoch of
        TAI ->
            case scale of
                TAI ->
                    epoch

                TT ->
                    let
                        ( seconds, fraction ) =
                            adjust (getSeconds epoch) (getFraction epoch) offsetTaiTt
                    in
                    Epoch
                        { timescale = scale
                        , seconds = seconds
                        , fraction = fraction
                        }

                TDB ->
                    let
                        oldSeconds =
                            getSeconds epoch

                        oldFraction =
                            getFraction epoch

                        deltaT =
                            offsetTtTdb (oldFraction + toFloat oldSeconds)
                                + offsetTaiTt

                        ( seconds, fraction ) =
                            adjust oldSeconds oldFraction deltaT
                    in
                    Epoch
                        { timescale = scale
                        , seconds = seconds
                        , fraction = fraction
                        }

        TT ->
            case scale of
                TAI ->
                    let
                        ( seconds, fraction ) =
                            adjust (getSeconds epoch) (getFraction epoch) -offsetTaiTt
                    in
                    Epoch
                        { timescale = scale
                        , seconds = seconds
                        , fraction = fraction
                        }

                TT ->
                    epoch

                TDB ->
                    let
                        oldSeconds =
                            getSeconds epoch

                        oldFraction =
                            getFraction epoch

                        deltaT =
                            offsetTtTdb (oldFraction + toFloat oldSeconds)

                        ( seconds, fraction ) =
                            adjust oldSeconds oldFraction deltaT
                    in
                    Epoch
                        { timescale = scale
                        , seconds = seconds
                        , fraction = fraction
                        }

        TDB ->
            case scale of
                TAI ->
                    let
                        oldSeconds =
                            getSeconds epoch

                        oldFraction =
                            getFraction epoch

                        deltaT =
                            offsetTdbTt (oldFraction + toFloat oldSeconds)
                                - offsetTaiTt

                        ( seconds, fraction ) =
                            adjust oldSeconds oldFraction deltaT
                    in
                    Epoch
                        { timescale = scale
                        , seconds = seconds
                        , fraction = fraction
                        }

                TT ->
                    let
                        oldSeconds =
                            getSeconds epoch

                        oldFraction =
                            getFraction epoch

                        deltaT =
                            offsetTdbTt (oldFraction + toFloat oldSeconds)

                        ( seconds, fraction ) =
                            adjust oldSeconds oldFraction deltaT
                    in
                    Epoch
                        { timescale = scale
                        , seconds = seconds
                        , fraction = fraction
                        }

                TDB ->
                    epoch
