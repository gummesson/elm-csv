module Helper exposing (parseRemaining, splitLine, splitLineWith)

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

    >>> splitLineWith " " "a b "
    ["a", "b", ""]

    >>> splitLineWith " " "a \"b\""
    ["a", "b"]

    >>> splitLineWith " " "a \"\"b"
    ["a", "\"b"]

    >>> splitLineWith " " "a \\\"b"
    ["a", "\"b"]

    >>> splitLineWith " " "a b\"c\"d e"
    ["a", "b\"c\"d", "e"]

    >>> splitLineWith " " "a \"b c\""
    ["a", "b c"]

    >>> splitLineWith "" "abc"
    ["abc"]

    >>> splitLineWith "" ""
    []

    >>> String.repeat 1000000 "a," |> splitLineWith ","
    (List.repeat 1000000 "a") ++ [""]

-}
splitLineWith : String -> String -> List String
splitLineWith separator line =
    parseRemaining separator False line []
        |> List.reverse


parseRemaining : String -> Bool -> String -> List String -> List String
parseRemaining separator quoted remaining done =
    if remaining == "" then
        done

    else if separator /= "" && not quoted && String.startsWith separator remaining then
        let
            newQuoted =
                False

            nextChars =
                String.dropLeft (String.length separator) remaining
        in
        parseRemaining separator False nextChars ("" :: done)

    else
        let
            current =
                List.head done |> Maybe.withDefault ""

            others =
                List.tail done |> Maybe.withDefault []

            nextChar =
                String.slice 0 1 remaining

            nextNextChar =
                String.slice 1 2 remaining

            startQuote =
                nextChar == "\"" && nextNextChar /= "\"" && current == ""

            isEscapedQuote =
                not quoted && (nextChar == "\\" || nextChar == "\"") && nextNextChar == "\""

            endQuote =
                quoted && nextChar == "\"" && not isEscapedQuote

            newQuoted =
                (quoted && not endQuote) || startQuote

            nextChars =
                String.dropLeft
                    (if isEscapedQuote then
                        2

                     else
                        1
                    )
                    remaining

            newChar =
                if isEscapedQuote then
                    "\""

                else if startQuote || endQuote then
                    ""

                else
                    nextChar

            newDone =
                (current ++ newChar) :: others
        in
        parseRemaining separator newQuoted nextChars newDone
