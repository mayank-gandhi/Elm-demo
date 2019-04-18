-- exporting functions inside this
module Bingo exposing (..)

import Html exposing (text, h2, div, h1, a, header, footer, Html, ul, li, span, button)
import Html.Attributes exposing (id, class, href, classList)
import Html.Events exposing (onClick)
import Random


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


-- UPDATE


type Msg = NewGame | Mark Int | NewRandom Int


update : Msg -> Player -> ( Player, Cmd Msg )
update msg player =
  case msg of
    NewRandom randomNumber ->
      { player | gameNumber = randomNumber } ! []
    NewGame ->
      { player | words = initialWords } ! [generateRandomNumber]
    Mark id ->
      let
        markEntry e =
          if e.id == id then
            { e | marked = (not e.marked) }
          else
            e
      in
        { player | words = List.map markEntry player.words } ! []


-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
  Random.generate NewRandom (Random.int 1 100)


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

getWordItem: Word -> Html Msg
getWordItem word =
  li [ classList [ ("marked", word.marked) ], onClick (Mark word.id) ]
    [ span [ class "phrase" ] [ text word.word ]
    , span [ class "points" ] [ text (toString word.points) ]
    ]

pageWordList: List Word -> Html Msg
pageWordList words =
  words
    |> List.map getWordItem
    |> ul []


sumMarkedWords : List Word -> Int
sumMarkedWords words =
  words
    |> List.filter .marked
    -- |> List.map .points
    -- |> List.sum
    |> List.foldl (\w sum -> sum + w.points) 0


viewScore : Int -> Html Msg
viewScore sum =
  div
    [ class "score" ]
    [ span [ class "label" ] [ text "Score" ]
    , span [ class "value" ] [ text (toString sum) ]
    ]


pageContent : Player -> Html Msg
pageContent player =
  div [ class "content" ]
    [ pageHeader "Bingo"
    , stylePlayerHtml player.name player.gameNumber
    , pageWordList player.words
    , viewScore (sumMarkedWords player.words)
    , div [ class "button-group" ]
          [ button [ onClick NewGame ] [text "New Game"] ]
    , div [ class "debug" ] [text (toString player)]
    , pageFooter
    ]


-- main : Html Msg
-- main =
--   update NewGame initialPlayer
--     |> pageContent


main : Program Never Player Msg
main =
  Html.program
    { init = ( initialPlayer, generateRandomNumber )
    , view = pageContent
    , update = update
    , subscriptions = always Sub.none
    }

