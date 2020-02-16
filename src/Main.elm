module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (..)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

winningCombinations : List (List Int)
winningCombinations = [[1,2,3],
                       [4,5,6],
                       [7,8,9],
                       [1,4,7],
                       [2,5,8],
                       [3,6,9],
                       [1,5,9],
                       [3,5,7]]

type Msg
  = PlaceSymbol Int 
type GameStatus = Inplay | Over

type Symbol = X | O | Empty

type Move = Position Int

type alias Status = 
  { gameStatus:GameStatus,
    winner: Symbol}

type alias Model = 
  { board : List Symbol,
    currentPlayer : List Int,
    otherPlayer : List Int,
    currentSymbol : Symbol,
    status: Status}

init : Model
init = 
  { board = repeat 9 Empty,
    currentPlayer = [],
    otherPlayer = [],
    currentSymbol = X,
    status = { gameStatus = Inplay,
               winner = Empty} }

-- UPDATE

update : Move -> Model -> Model
update (Position pos) model =
  { board = List.indexedMap (updateVal model.currentSymbol pos) model.board,
    currentPlayer = model.otherPlayer,
    otherPlayer = model.currentPlayer ++ [pos + 1],
    status = updateStatus (model.currentPlayer ++ [pos + 1]) model.currentSymbol,
    currentSymbol = updateCurrentSymbol model.currentSymbol}

hasWon : List Int -> Bool 
hasWon moves = 
  List.any (\combination -> (List.all (includes moves) combination)) winningCombinations

updateStatus : List Int -> Symbol -> Status
updateStatus moves symbol =
  if (hasWon moves)
  then {gameStatus = Over, winner = symbol}
  else {gameStatus = Inplay, winner = Empty}

updateCurrentSymbol : Symbol -> Symbol
updateCurrentSymbol sym =
  case sym of 
    X -> O
    _ -> X

updateVal : Symbol -> Int -> Int -> Symbol -> Symbol
updateVal newSym pos currPos currSym =
  if pos == currPos
  then newSym
  else currSym

includes : List Int -> Int -> Bool
includes moves pos =
  List.member pos moves

-- VIEW

view : Model -> Html Move
view model =
  div []
    [ button [ onClick (Position 0)] [ text "click Me!" ]
    ,button [ onClick  (Position 1)] [ text "click Me!" ]
    ,button [ onClick (Position 2)] [ text "click Me!" ]
    ,button [ onClick (Position 3)] [ text "click Me!" ]
    ,button [ onClick (Position 4)] [ text "click Me!" ]
    ,button [ onClick (Position 5)] [ text "click Me!" ]
    ,button [ onClick (Position 6)] [ text "click Me!" ]
    ,button [ onClick (Position 7)] [ text "click Me!" ]
    ,button [ onClick (Position 8)] [ text "click Me!" ]
    ,div [] [text (Debug.toString (model.status))]
    ,div [] [text (Debug.toString (model.currentPlayer))]
    ,div [] [text (Debug.toString (model.otherPlayer))]]