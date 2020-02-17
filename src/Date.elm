module Date exposing
    ( Calendar(..)
    , Date
    , fromDayNumber
    , fromYearDay
    , fromYearMonthDay
    , getCalendar
    , getDay
    , getJ2000
    , getMonth
    , getYear
    , julianEpochDate
    , toString
    )

import Result exposing (Result)


daysPerYear : Int
daysPerYear =
    365


julianEpochDate : Date
julianEpochDate =
    Date
        { calendar = ProlepticJulianCalendar
        , year = -4712
        , month = 1
        , day = 1
        }


type Calendar
    = ProlepticJulianCalendar
    | JulianCalendar
    | GregorianCalendar


findCalendar : Int -> Int -> Int -> Calendar
findCalendar year month day =
    if year < 1 then
        ProlepticJulianCalendar

    else if (year == 1582 && month == 10 && day < 5) || (year == 1582 && month < 10) || year < 1582 then
        JulianCalendar

    else
        GregorianCalendar


isLeapYear : Calendar -> Int -> Bool
isLeapYear calendar year =
    case calendar of
        ProlepticJulianCalendar ->
            modBy 4 year == 0

        JulianCalendar ->
            modBy 4 year == 0

        GregorianCalendar ->
            modBy 4 year == 0 && (modBy 400 year == 0 || modBy 100 year /= 0)


findYear : Calendar -> Int -> Int
findYear calendar daysFromJ2000 =
    case calendar of
        ProlepticJulianCalendar ->
            -((-4 * daysFromJ2000 - 2920488) // 1461)

        JulianCalendar ->
            -((-4 * daysFromJ2000 - 2921948) // 1461)

        GregorianCalendar ->
            let
                year =
                    (400 * daysFromJ2000 + 292194288) // 146097
            in
            if daysFromJ2000 <= lastDayOfYearFromJ2000 calendar (year - 1) then
                year - 1

            else
                year


lastDayOfYearFromJ2000 : Calendar -> Int -> Int
lastDayOfYearFromJ2000 calendar year =
    case calendar of
        ProlepticJulianCalendar ->
            daysPerYear * year + (year + 1) // 4 - 730123

        JulianCalendar ->
            daysPerYear * year + year // 4 - 730122

        GregorianCalendar ->
            daysPerYear * year + year // 4 - year // 100 + year // 400 - 730120


findMonth : Bool -> Int -> Int
findMonth isLeap dayInYear =
    let
        offset =
            if isLeap then
                313

            else
                323
    in
    if dayInYear < 32 then
        1

    else
        (10 * dayInYear + offset) // 306


previousMonthEndDay : Bool -> Int -> Int
previousMonthEndDay isLeap month =
    if isLeap then
        case month of
            1 ->
                0

            2 ->
                31

            3 ->
                60

            4 ->
                91

            5 ->
                121

            6 ->
                152

            7 ->
                182

            8 ->
                213

            9 ->
                244

            10 ->
                274

            11 ->
                305

            12 ->
                335

            _ ->
                0

    else
        case month of
            1 ->
                0

            2 ->
                31

            3 ->
                59

            4 ->
                90

            5 ->
                120

            6 ->
                151

            7 ->
                181

            8 ->
                212

            9 ->
                243

            10 ->
                273

            11 ->
                304

            12 ->
                334

            _ ->
                0


findDay : Bool -> Int -> Int -> Result String Int
findDay isLeap month dayInYear =
    if not isLeap && dayInYear > 365 then
        Err "Day of year cannot be 366 for a non-leap year."

    else
        Ok (dayInYear - previousMonthEndDay isLeap month)


findDayInYear : Bool -> Int -> Int -> Int
findDayInYear isLeap month day =
    day + previousMonthEndDay isLeap month


type Date
    = Date
        { calendar : Calendar
        , year : Int
        , month : Int
        , day : Int
        }


getCalendar : Date -> Calendar
getCalendar (Date { calendar, year, month, day }) =
    calendar


getYear : Date -> Int
getYear (Date { calendar, year, month, day }) =
    year


getMonth : Date -> Int
getMonth (Date { calendar, year, month, day }) =
    month


getDay : Date -> Int
getDay (Date { calendar, year, month, day }) =
    day


getJ2000 : Date -> Int
getJ2000 (Date { calendar, year, month, day }) =
    calendarYearMonthDayToJ2000 calendar year month day


toString : Date -> String
toString (Date { calendar, year, month, day }) =
    String.padLeft 4 ' ' (String.fromInt year)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt month)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt day)


calendarYearMonthDayToJ2000 : Calendar -> Int -> Int -> Int -> Int
calendarYearMonthDayToJ2000 calendar year month day =
    lastDayOfYearFromJ2000 calendar (year - 1) + findDayInYear (isLeapYear calendar year) month day


yearMonthDayToJ2000 : Int -> Int -> Int -> Int
yearMonthDayToJ2000 year month day =
    let
        calendar =
            findCalendar year month day
    in
    lastDayOfYearFromJ2000 calendar (year - 1) + findDayInYear (isLeapYear calendar year) month day


fromDayNumber : Int -> Result String Date
fromDayNumber dayNumber =
    let
        calendar =
            if dayNumber >= -152384 then
                GregorianCalendar

            else if dayNumber > -730122 then
                JulianCalendar

            else
                ProlepticJulianCalendar

        year =
            findYear calendar dayNumber

        dayInYear =
            dayNumber - lastDayOfYearFromJ2000 calendar (year - 1)

        isLeap =
            isLeapYear calendar year

        month =
            findMonth isLeap dayInYear

        dayResult =
            findDay isLeap month dayInYear
    in
    case dayResult of
        Ok day ->
            Ok
                (Date
                    { calendar = calendar
                    , year = year
                    , month = month
                    , day = day
                    }
                )

        Err msg ->
            Err msg


fromYearDay : Int -> Int -> Result String Date
fromYearDay year dayInYear =
    let
        calendar =
            if year < 1 then
                ProlepticJulianCalendar

            else if year < 1583 then
                JulianCalendar

            else
                GregorianCalendar

        isLeap =
            isLeapYear calendar year

        month =
            findMonth isLeap dayInYear

        dayResult =
            findDay isLeap month dayInYear
    in
    case dayResult of
        Ok day ->
            Ok
                (Date
                    { calendar = calendar
                    , year = year
                    , month = month
                    , day = day
                    }
                )

        Err msg ->
            Err msg


fromYearMonthDay : Int -> Int -> Int -> Result String Date
fromYearMonthDay year month day =
    if month < 1 || month > 12 then
        Err ("Invalid month number: " ++ String.fromInt month)

    else
        let
            checkRes =
                fromDayNumber (yearMonthDayToJ2000 year month day)
        in
        case checkRes of
            Err msg ->
                Err msg

            Ok check ->
                if getYear check /= year || getMonth check /= month || getDay check /= day then
                    Err "Invalid date."

                else
                    Ok
                        (Date
                            { calendar = getCalendar check
                            , year = year
                            , month = month
                            , day = day
                            }
                        )
