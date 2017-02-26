module Helper exposing (..)

import Regex

{-| Split a CSV line, with the traditional comma separator

    >>> splitLine "hello,world"
    ["hello", "world"]
-}
splitLine : String -> List String
splitLine =
    splitLineWith ","


{-| Split a CSV line with the given separator

    >>> splitLineWith " " "a b"
    ["a", "b"]

    >>> splitLineWith " " "a \"b\""
    ["a", "b"]

    >>> splitLineWith " " "a \"b c\""
    ["a", "b c"]
-}
splitLineWith : String -> String -> List String
splitLineWith separator line =
    let
        values =
            String.split separator line
    in
        List.map (trimQuotes << String.trim) values


{-| Extract text from a csv field

    trimQuotes "abc" == "abc"
    trimQuotes "\"this is a \"\"test\"\" ! \"" == "This is a \"test\" ! "
-}
trimQuotes : String -> String
trimQuotes value =
    let
        start =
            String.startsWith "\"" value

        end =
            String.endsWith "\"" value
    in
        if start && end then
            -- Replace escaped quotes like "" and \"
            Regex.replace Regex.All (Regex.regex "[\"\\\\]\"") (always "\"") value
                |> String.dropRight 1
                |> String.dropLeft 1
        else
            value
