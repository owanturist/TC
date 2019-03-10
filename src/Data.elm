module Data exposing (generate, Chart, Point)

import Random exposing (Generator)
import Time


millisecondsInDay : Int
millisecondsInDay =
    1000 * 60 * 60 * 24


type alias Point a =
    ( Time.Posix, a )


pointGenerator : Int -> Generator a -> Generator (Point a)
pointGenerator milliseconds dataGen =
    Random.map (Tuple.pair (Time.millisToPosix milliseconds)) dataGen


type alias Chart a =
    { label : String
    , color : String
    , data : List (Point a)
    }


chartGenerator : String -> String -> Int -> Time.Posix -> Generator a -> Generator (Chart a)
chartGenerator label color days now dataGen =
    dataGen
        |> Random.list days
        |> Random.map
            (List.foldr
                (\value acc ->
                    { milliseconds = acc.milliseconds - millisecondsInDay
                    , points = ( Time.millisToPosix acc.milliseconds, value ) :: acc.points
                    }
                )
                { milliseconds = Time.posixToMillis now
                , points = []
                }
            )
        |> Random.map (Chart label color << .points)


generate : Int -> Time.Posix -> List (Chart Int)
generate days now =
    let
        seed0 =
            Random.initialSeed (Time.posixToMillis now)

        ( joined, seed1 ) =
            Random.step (chartGenerator "Joined" "rgb(67, 192, 71)" days now (Random.int 0 200)) seed0

        ( left, _ ) =
            Random.step (chartGenerator "Left" "rgb(240, 78, 74)" days now (Random.int 20 150)) seed0
    in
    [ joined , left ]
