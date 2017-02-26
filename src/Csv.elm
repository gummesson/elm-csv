module Csv exposing (Csv, parse, parseWith, split, splitWith)

{-| A CSV parser that supports different separators, and quoted fields.
The results are provided as lists.

## Definitions
@docs Csv

## Parsing functions
@docs parseWith, parse, split, splitWith
-}

import List
import String
import Maybe

import Helper exposing (..)

{-| The `Csv` type structure.
-}
type alias Csv =
    { headers : List String
    , records : List (List String)
    }


{-| Convert a string of comma-separated values into a `Csv` structure.

    >>> parse "id,value\n1,one\n2,two\n"
    {
      headers = ["id", "value"],
      records = [
                    ["1", "one"],
                    ["2", "two"]
                ]
    }

Values that contain the character ',' can be quoted

    >>> parse "id,value\n\"1,2,3\",\"one,two,three\"\n"
    {
      headers = ["id", "value"],
      records = [
                    ["1,2,3", "one,two,three"]
                ]
    }

Double quotes can be escaped with a backslash or a second quote

    >>> parse "value\n,Here is a quote:\"\"\nAnother one:\\\"\n"
    {
      headers = ["value"],
      records = [
                    ["Here is a quote:\""],
                    ["Another one:\""]
                ]
    }
-}
parse : String -> Csv
parse =
    parseWith ","


{-| Convert a string of values separated by a *separator* into a `Csv` structure.

    >>> parseWith ";" "id;value\n1;one\n2;two\n"
    {
      headers = ["id", "value"],
      records = [
                    ["1", "one"],
                    ["2", "two"]
                ]
    }
-}
parseWith : String -> String -> Csv
parseWith separator lines =
    let
        values =
            splitWith separator lines

        headers =
            List.head values
                |> Maybe.withDefault []

        records =
            List.drop 1 values
    in
        { headers = headers
        , records = records
        }


{-| Convert a string of comma-separated values into a list of lists.

    >>> split "id,value\n1,one\n2,two\n"
    [["id", "value"], ["1", "one"], ["2", "two"]]
-}
split : String -> List (List String)
split =
    splitWith ","


{-| Convert a string of values separated by a character into a list of lists.

    >>> splitWith "," "id,value\n1,one\n2,two\n"
    [["id", "value"], ["1", "one"], ["2", "two"]]
-}
splitWith : String -> String -> List (List String)
splitWith separator lines =
    let
        values =
            String.lines lines
                |> List.filter (\x -> not (String.isEmpty x))
    in
        List.map (splitLineWith separator) values


