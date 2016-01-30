module Tests where

import Csv
import ElmTest as Test exposing (Test)

import String
import List


all : Test
all =
  Test.suite "Csv"
    [ Test.test "split"
      (Test.assertEqual
        [["id", "value"], ["1", "one"], ["2", "two"]]
        (Csv.split "id,value\n1,one\n2,two\n"))
    , Test.test "parse"
      (Test.assertEqual
        { headers = ["id", "value"], records = [["1", "one"], ["2", "two"]] }
        (Csv.parse "id,value\n1,one\n2,two\n"))
    ]
