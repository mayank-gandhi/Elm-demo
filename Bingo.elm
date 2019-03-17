-- exporting functions inside this
module Bingo exposing (..)

import Html exposing (text, h2, div, h1, a, header, footer)
import Html.Attributes exposing (id, class, href)


-- afunc = \x y -> x + y 3
-- what do you think above statement will do

playerInfo : String -> Int -> String
playerInfo name gameNumber =
  name ++ " - game #" ++ (toString gameNumber)

stylePlayerHtml : String -> Int -> Html.Html msg
stylePlayerHtml name gameNumber =
  let
    playerInfoHtml =
      playerInfo name gameNumber
        |> String.toUpper
        |> text
  in
    h2 [ id "info", class "classy" ]
      [ playerInfoHtml ]

pageHeader : String -> Html.Html msg
pageHeader title =
  header []
    [ h1 [] [ text title ] ]

pageFooter : Html.Html msg
pageFooter =
  footer []
    [ a [ href "https://elm-lang.org" ]
      [ text "powered by Elm" ]
    ]

pageContent : Html.Html msg
pageContent =
  div [ class "content" ]
    [ pageHeader "Bingo"
    , stylePlayerHtml "mayank" 3
    , pageFooter
    ]

main : Html.Html msg
main =
  pageContent
