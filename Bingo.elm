-- exporting functions inside this
module Bingo exposing (..)

import Html exposing (text, h2, div, h1, a, header, footer, Html)
import Html.Attributes exposing (id, class, href)


-- MODEL

type alias Player =
  { name: String
  , gameNumber: Int
  , words: List Word
  }

type alias Word =
  { id: Int
  , word: String
  , points: Int
  , marked: Bool
  }


initialPlayer: Player
initialPlayer =
  Player "max" 9 initialWords

initialWords: List Word
initialWords =
  [ Word 1 "code Elm" 100 False
  , Word 2 "code Java" 200 False
  , Word 3 "code Ruby" 300 False
  , Word 4 "code Red" 400 False
  ]

-- VIEW
playerInfo : String -> Int -> String
playerInfo name gameNumber =
  name ++ " - game #" ++ (toString gameNumber)

stylePlayerHtml : String -> Int -> Html msg
stylePlayerHtml name gameNumber =
  let
    playerInfoHtml =
      playerInfo name gameNumber
        |> String.toUpper
        |> text
  in
    h2 [ id "info", class "classy" ]
      [ playerInfoHtml ]

pageHeader : String -> Html msg
pageHeader title =
  header []
    [ h1 [] [ text title ] ]

pageFooter : Html msg
pageFooter =
  footer []
    [ a [ href "https://elm-lang.org" ]
      [ text "powered by Elm" ]
    ]

pageContent : Player -> Html msg
pageContent player =
  div [ class "content" ]
    [ pageHeader "Bingo"
    , stylePlayerHtml player.name player.gameNumber
    , div [ class "debug" ] [ text (toString player) ]
    , pageFooter
    ]

main : Html msg
main =
  pageContent initialPlayer
