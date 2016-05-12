module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String exposing (toUpper, repeat, trimRight)
import BingoUtils as Utils


--MODEL


type alias Entry =
  { phrase : String
  , points : Int
  , wasSpoken : Bool
  , id : Int
  }


type alias Model =
  { entries : List Entry
  , phraseInput : String
  , pointsInput : String
  , nextId : Int
  }


initialModel : Model
initialModel =
  { entries = []
  , phraseInput = ""
  , pointsInput = ""
  , nextId = 1
  }


newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
  Entry phrase points False id



--UPDATE


type Msg
  = NoOp
  | Sort
  | Delete Int
  | Mark Int
  | UpdatePhraseInput String
  | UpdatePointsInput String
  | Add


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model

    Sort ->
      { model | entries = List.sortBy .points model.entries }

    Delete id ->
      let
        isntFromId e =
          e.id /= id

        remainingEntries =
          List.filter isntFromId model.entries

      in
        { model | entries = remainingEntries }

    Mark id ->
      let
        updateEntry e =
          if e.id == id then
            { e | wasSpoken = (not e.wasSpoken) }
          else
            e
      in
        { model | entries = List.map updateEntry model.entries }

    UpdatePhraseInput contents ->
      { model | phraseInput = contents }

    UpdatePointsInput contents ->
      { model | pointsInput = contents }

    Add ->
      let
        entryToAdd =
          newEntry model.phraseInput (Utils.parseInt model.pointsInput) model.nextId

        isInvalid model =
          String.isEmpty model.phraseInput || String.isEmpty model.pointsInput
      in
        if isInvalid model then
          model
        else
          { model
            | phraseInput = ""
            , pointsInput = ""
            , entries = entryToAdd :: model.entries
            , nextId = model.nextId + 1
          }



--VIEW


title : String -> Int -> Html Msg
title message times =
  message
    ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text


pageHeader : String -> Html Msg
pageHeader logo =
  h1 [] [ title logo 3 ]


pageFooter : Html Msg
pageFooter =
  footer
    []
    [ a
        [ href "https://neighborly.com" ]
        [ text "Neighborly" ]
    ]


entryItem : Entry -> Html Msg
entryItem entry =
  li
    [ classList
        [ ( "highlight", entry.wasSpoken )
        , ( "entry-not-spoken", not <| entry.wasSpoken )
        ]
    , onClick (Mark entry.id)
    ]
    [ span [ class "phrase" ] [ text entry.phrase ]
    , span [ class "points" ] [ toString entry.points |> text ]
    , button [ class "delete", onClick (Delete entry.id) ] []
    ]


totalPoints : List Entry -> Int
totalPoints entries =
  entries
    |> List.filter .wasSpoken
    |> List.map .points
    |> List.sum


totalItem : Int -> Html Msg
totalItem total =
  li
    [ class "total" ]
    [ span [ class "label" ] [ text "Total" ]
    , span [ class "points" ] [ text (toString total) ]
    ]


entryList : List Entry -> Html Msg
entryList entries =
  let
    entryItems =
      List.map (entryItem) entries

    items =
      entryItems ++ [ totalItem (totalPoints entries) ]

  in
    ul [ classList [ ( "Hey", True ), ( "Ho", False ) ] ] items


entryForm : Model -> Html Msg
entryForm model =
  div
    []
    [ input
        [ type' "text"
        , placeholder "Phrase"
        , value model.phraseInput
        , name "phrase"
        , autofocus True
        , onInput UpdatePhraseInput
        ]
        []
    , input
        [ type' "number"
        , placeholder "Points"
        , value model.pointsInput
        , name "points"
        , onInput UpdatePointsInput
        ]
        []
    , button [ class "add", onClick Add ] [ text "Add" ]
    , h2 [] [ text (model.phraseInput ++ " " ++ model.pointsInput) ]
    ]


view : Model -> Html Msg
view model =
  div
    [ id "container" ]
    [ pageHeader "Bingo!"
    , entryForm model
    , entryList model.entries
    , button
        [ class "sort", onClick Sort ]
        [ text "Sort" ]
    , pageFooter
    ]



--WIRE IT ALL TOGETHER


main : Program Never
main =
  App.beginnerProgram
    { model = initialModel
    , view = view
    , update = update
    }
