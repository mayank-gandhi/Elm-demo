-- exporting functions inside this
module Bingo exposing (..)

import Html exposing (text, h2, div, h1, a, header, footer)
import Html.Attributes exposing (id, class, href)


-- MODEL
initialPlayer =
  { name = "max"
  , gameNumber = 9
  , words = initialWords
  }


initialWords =
  [ {id = 1, word = "code Elm", points = 100, marked = False}
  , {id = 2, word = "code Java", points = 200, marked = False}
  ]

-- VIEW
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

-- pageContent : Html.Html msg
pageContent player =
  div [ class "content" ]
    [ pageHeader "Bingo"
    , stylePlayerHtml player.name player.gameNumber
    , div [ class "debug" ] [ text (toString player) ]
    , pageFooter
    ]

main : Html.Html msg
main =
  pageContent initialPlayer
