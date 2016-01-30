module Csv (Csv, parse, split) where

{-| A CSV parser.

## Parser
@docs Csv, parse, split
-}

import List
import String
import Maybe


{-| The `Csv` type structure.
-}
type alias Csv =
  { headers : List String
  , records : List (List String)
  }


{-| Convert a string of comma-separated values into a `Csv` structure.

    -- { headers = ["id", "value"], records = [["1", "one"], ["2", "two"]] }

    Csv.parse "id,value\n1,one\n2,two\n"
-}
parse : String -> Csv
parse lines =
  let
    values =
      split lines

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

    -- [["id", "value"], ["1", "one"], ["2", "two"]]

    Csv.split "id,value\n1,one\n2,two\n"
-}
split : String -> List (List String)
split lines =
  let
    values =
      String.lines lines
        |> List.filter (\x -> not (String.isEmpty x))
  in
    List.map splitLine values


splitLine : String -> List String
splitLine line =
  let
    values =
      String.split "," line
  in
    List.map String.trim values
