-- exporting functions inside this
module Bingo exposing (..)

import Html

-- main =
--   Html.text (String.repeat 3 (String.toUpper "Elm live working"))

{-
main =
  "- Elm live working - " |> String.toUpper |> String.repeat 3 |> Html.text
-}

main =
  "Elm live working"
    |> String.toUpper
    |> String.repeat 3
    |> String.pad 100 '*'
    |> Html.text
