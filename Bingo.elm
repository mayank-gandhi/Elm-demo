-- exporting functions inside this
module Bingo exposing (..)

import Html


-- afunc = \x y -> x + y 3
-- what do you think above statement will do

playerInfo name gameNumber =
  name ++ " - game #" ++ (toString gameNumber)

playerInfoHtml name gameNumber =
  playerInfo name gameNumber
    |> String.toUpper
    |> Html.text

main =
  playerInfoHtml "mayank" 3
