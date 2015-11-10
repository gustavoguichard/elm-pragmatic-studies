{-

Studying Elm
Elm comment block

-}

module Bingo where

--Qualified import, needs to be called with it's module at the front
--import Html
--Unqualified import, exposes the functions to "global" scope
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toUpper, repeat, trimRight)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import Debug


--MODEL

type alias Entry =  { phrase: String
                    , points: Int
                    , wasSpoken: Bool
                    , id: Int }


type alias Model = { entries: List Entry }

initialModel =
  { entries =
    [ newEntry "Doing Agile" 200 2
    , newEntry "In the Cloud" 300 3
    , newEntry "Future-proof" 100 1
    , newEntry "Rock-Star Ninja" 400 4
    ]
  }


newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
  { phrase = phrase
  , points = points
  , wasSpoken = False
  , id = id
  }


--UPDATE

type Action
  = NoOp
  | Sort
  | Delete Int
  | Mark Int


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Sort ->
      { model | entries <- List.sortBy .points model.entries }
      --This is same as:
      --List.sortBy (\entry -> entry.points) model.entries

    Delete id ->
      let
        isntFromId e = e.id /= id
        remainingEntries =  List.filter isntFromId model.entries
                            |> Debug.log "The remaining entries"
        --Same as:
        --_ = Debug.log "The remaining entries" remainingEntries
      in
        { model | entries <- remainingEntries }

    Mark id ->
      let
        updateEntry e =
          if e.id == id then { e | wasSpoken <- (not e.wasSpoken) } else e
      in
        { model | entries <- List.map updateEntry model.entries }


--VIEW

title : String -> Int -> Html
title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text


--A "function" with no arguments is a definition, not a function
pageHeader =
  h1 [] [ title "bingo!" 3 ]


pageFooter =
  footer []
    [ a [ href "https://neighborly.com" ]
        [ text "Neighborly" ]
    ]


entryItem : Address Action -> Entry -> Html
entryItem address entry =
  li
    [ classList
      [ ("highlight", entry.wasSpoken)
      , ("entry-not-spoken", not <| entry.wasSpoken) ]
    , onClick address (Mark entry.id) ]
    [ span [ class "phrase" ] [ text entry.phrase ]
    , span [ class "points" ] [ toString entry.points |> text ]
    , button [ class "delete", onClick address (Delete entry.id) ] []
    ]


totalPoints : List Entry -> Int
totalPoints entries =
  entries
    |> List.filter .wasSpoken
    |> List.foldl (\e sum -> e.points + sum) 0
    --Same as:
    -- |> List.map .points spokenEntries
    -- |> List.sum


totalItem : Int -> Html
totalItem total =
  li
    [ class "total" ]
    [ span [ class "label" ] [ text "Total" ]
    , span [ class "points" ] [ text (toString total) ] ]


entryList : Address Action -> List Entry -> Html
entryList address entries =
  let
    entryItems = List.map (entryItem address) entries
    items = entryItems ++ [ totalItem (totalPoints entries) ]
    --Same as:
    --entryWithAddress entry = entryItem address entry
  in
    ul [] items
    --Same as:
    --ul [] (List.map entryWithAddress entries)


view : Address Action -> Model -> Html
view address model =
  div [ id "container" ]
    [ pageHeader
    , entryList address model.entries
    , button
      [ class "sort", onClick address Sort ]
      [ text "Sort" ]
    , pageFooter
    ]


--WIRE IT ALL TOGETHER

main =
  --initialModel
  --  |> update Sort
  --  |> view
  StartApp.start
    { model = initialModel
    , view = view
    , update = update }
