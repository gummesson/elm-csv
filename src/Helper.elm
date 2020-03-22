module Helper exposing (parseRemaining, splitLine, splitLineWith)

{-| Helper functions
-}


{-| Split a CSV line, with the traditional comma separator

    splitLine "hello,world"
    --> ["hello", "world"]

-}
splitLine : String -> List String
splitLine =
    splitLineWith ","


{-| Split a CSV line with the given separator

    splitLineWith " " "a b"
    --> ["a", "b"]

    splitLineWith " " "a b "
    --> ["a", "b", ""]

    splitLineWith " " "a \"b\""
    --> ["a", "b"]

    splitLineWith " " "a \"\"b"
    --> ["a", "\"b"]

    splitLineWith " " "a \\\"b"
    --> ["a", "\"b"]

    splitLineWith " " "a b\"c\"d e"
    --> ["a", "b\"c\"d", "e"]

    splitLineWith " " "a \"b c\""
    --> ["a", "b c"]

    splitLineWith "" "abc"
    --> ["abc"]

    splitLineWith "," ",a,b,c"
    --> ["", "a", "b", "c"]

    splitLineWith "" ""
    --> []

    String.repeat 1000000 "a," |> splitLineWith ","
    --> (List.repeat 1000000 "a") ++ [""]

-}
splitLineWith : String -> String -> List String
splitLineWith separator line =
    parseRemaining separator False True line []
        |> List.reverse


parseRemaining : String -> Bool -> Bool -> String -> List String -> List String
parseRemaining separator quoted startOfLine remaining result =
    if remaining == "" then
        result

    else if separator /= "" && not quoted && String.startsWith separator remaining then
        let
            newQuoted =
                False

            nextChars =
                String.dropLeft (String.length separator) remaining

            newResult =
                if startOfLine then
                    [ "", "" ]

                else
                    "" :: result
        in
        parseRemaining separator False False nextChars newResult

    else
        let
            current =
                List.head result |> Maybe.withDefault ""

            others =
                List.tail result |> Maybe.withDefault []

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
        parseRemaining separator newQuoted False nextChars newDone
