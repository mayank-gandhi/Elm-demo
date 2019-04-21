-- exporting functions inside this
module Bingo exposing (..)

import Html exposing (text, h2, div, h1, a, header, footer, Html, ul, li, span, button)
import Html.Attributes exposing (id, class, href, classList)
import Html.Events exposing (onClick)
import Random
import Http
import Json.Decode as Decode
import Json.Encode as Encode


-- MODEL


type alias Player =
  { name: String
  , gameNumber: Int
  , words: List Word
  , alertMessage: Maybe String
  }

type alias Word =
  { id: Int
  , phrase: String
  , points: Int
  , marked: Bool
  }

type alias Score =
  { id: Int
  , name: String
  , score: Int
  }


initialPlayer: Player
initialPlayer =
  Player "max" 9 [] Nothing

initialWords: List Word
initialWords =
  [ Word 1 "code Elm" 100 False
  , Word 2 "code Java" 200 False
  , Word 3 "code Ruby" 300 False
  , Word 4 "code Red" 400 False
  ]


-- UPDATE


type Msg =
  NewGame
  | Mark Int
  | NewRandom Int
  | NewWords (Result Http.Error (List Word))
  | CloseAlert
  | ShareScore
  | NewScore (Result Http.Error Score)

update : Msg -> Player -> ( Player, Cmd Msg )
update msg player =
  case msg of
    NewRandom randomNumber ->
      { player | gameNumber = randomNumber } ! []
    NewGame ->
      player ! [getWords, generateRandomNumber]
    NewWords result ->
      case result of
        Ok words ->
          { player | words = words } ! []
        Err error ->
          let
            errorMessage =
              case error of
                Http.NetworkError ->
                  "Please check server is running???"
                Http.BadStatus response ->
                  (toString response.status)
                Http.BadPayload message _ ->
                  "Decoding Failed: " ++ message
                _ ->
                  "Something went wrong!!!"
          in
            { player | alertMessage = (Just errorMessage) } ! []
    CloseAlert ->
      { player | alertMessage = Nothing } ! []
    Mark id ->
      let
        markEntry e =
          if e.id == id then
            { e | marked = (not e.marked) }
          else
            e
      in
        { player | words = List.map markEntry player.words } ! []
    ShareScore ->
      player ! [ postScore player ]
    NewScore result ->
      case result of
        Ok score ->
          let
            message =
              score.name
                ++ " your Score of "
                ++ (toString score.score)
                ++ " has beed successfully shared!!"
          in
            { player | alertMessage = Just message } ! []
        Err error ->
          let
            message =
              "Error in posting your score"
                ++ (toString error)
          in
            { player | alertMessage = Just message } ! []


-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
  Random.generate NewRandom (Random.int 1 100)


baseUrl : String
baseUrl =
  "http://localhost:3000"


wordsUrl : String
wordsUrl =
  baseUrl ++ "/random-entries"


scoreUrl : String
scoreUrl =
  baseUrl ++ "/scores"


getWords : Cmd Msg
getWords =
  wordListDecoder
    |> Http.get wordsUrl
    |> Http.send NewWords


postScore : Player -> Cmd Msg
postScore player =
  let
    body =
      player
        |> encodeScore
        |> Http.jsonBody
  in
    scoreDecoder
      |> Http.post scoreUrl body
      |> Http.send NewScore

-- DECODER/ENCODERS


wordDecoder : Decode.Decoder Word
wordDecoder =
  Decode.map4 Word
    (Decode.field "id" Decode.int)
    (Decode.field "phrase" Decode.string)
    (Decode.field "points" Decode.int)
    (Decode.succeed False)


wordListDecoder : Decode.Decoder (List Word)
wordListDecoder =
  Decode.list wordDecoder


scoreDecoder : Decode.Decoder Score
scoreDecoder =
  Decode.map3 Score
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "score" Decode.int)


encodeScore : Player -> Encode.Value
encodeScore player =
  Encode.object
    [ ("name", Encode.string player.name)
    , ("score", Encode.int (sumMarkedWords player.words))
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

getWordItem: Word -> Html Msg
getWordItem word =
  li [ classList [ ("marked", word.marked) ], onClick (Mark word.id) ]
    [ span [ class "phrase" ] [ text word.phrase ]
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
    , viewAlertMessage player.alertMessage
    , pageWordList player.words
    , viewScore (sumMarkedWords player.words)
    , div [ class "button-group" ]
          [ button [ onClick NewGame ] [text "New Game"]
          , button [ onClick ShareScore ] [ text "Share Score" ]
          ]
    , div [ class "debug" ] [text (toString player)]
    , pageFooter
    ]


viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
  case alertMessage of
    Just message ->
      div [ class "alert" ]
          [ span [ class "close", onClick CloseAlert ] [ text "x" ]
          , text message
          ]
    Nothing ->
      text ""

-- main : Html Msg
-- main =
--   update NewGame initialPlayer
--     |> pageContent


main : Program Never Player Msg
main =
  Html.program
    { init = initialPlayer ! [ getWords, generateRandomNumber ]
    , view = pageContent
    , update = update
    , subscriptions = always Sub.none
    }

