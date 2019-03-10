module Utils.DOM exposing (closest, closestOffset, hasClosest, notClosest)

import DOM
import Json.Decode as Decode exposing (Decoder)
import Regex exposing (Regex)


containsClass : String -> String -> Bool
containsClass x className =
    case
        Regex.fromStringWith
            { caseInsensitive = False
            , multiline = False
            }
            ("(^|\\s+)" ++ x ++ "($|\\s+)")
    of
        Nothing ->
            False

        Just regex ->
            Regex.contains regex className


hasClosest : String -> Decoder Bool
hasClosest class =
    Decode.oneOf
        [ closest class (Decode.succeed True)
        , Decode.succeed False
        ]


closest : String -> Decoder node -> Decoder node
closest class decoder =
    Decode.andThen
        (\className ->
            if containsClass class className then
                decoder

            else
                DOM.parentElement (closest class decoder)
        )
        DOM.className


notClosest : String -> Decoder ()
notClosest class =
    Decode.andThen
        (\withClassName ->
            if withClassName then
                Decode.fail ("Class `" ++ class ++ "` exists in closest nodes.")

            else
                Decode.succeed ()
        )
        (Decode.oneOf
            [ closest class (Decode.succeed True)
            , Decode.succeed False
            ]
        )


closestOffset : String -> node -> Decoder node -> Decoder node
closestOffset class default decoder =
    Decode.andThen
        (\className ->
            if containsClass class className then
                decoder

            else
                DOM.offsetParent default (closestOffset class default decoder)
        )
        DOM.className
