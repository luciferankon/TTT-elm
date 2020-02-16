module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
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

type GameStatus = Inplay | Over

type Symbol = X | O | Empty

type Move = Position Int

type alias Status = 
  { gameStatus:GameStatus,
    winner: Symbol,
    hasWon: Bool}

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
               winner = Empty,
               hasWon = False} }

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
  List.any (\combination -> List.all (includes moves) combination) winningCombinations

updateStatus : List Int -> Symbol -> Status
updateStatus moves symbol =
  if hasWon moves
  then {gameStatus = Over, winner = symbol, hasWon = True}
  else {gameStatus = Inplay, winner = Empty, hasWon = False}

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

symbolToString : Symbol -> String
symbolToString sym = 
  case sym of 
    X -> "X"
    O -> "O"
    _ -> " "

-- VIEW
cell : Bool -> Int -> Symbol -> Html Move
cell won position symbol =
    button 
    (if symbol == Empty && won == False
            then [style "font-size" "30px",onClick (Position position)]
            else [style "font-size" "30px"])  [text (symbolToString symbol)]

view : Model -> Html Move
view model =
  div []
    [div [style "display" "grid"
        ,style "grid-template-columns" "auto auto auto"
        ,style "width" "40%"
        ,style "height" "500px"
        ,style "margin-left" "30%"] 
        (indexedMap (cell model.status.hasWon) model.board)
      ,div [ style "margin" "2% 30%"
            ,style "font-size" "30px"] (if model.status.hasWon then [text (Debug.toString model.status.winner ++ " has won!")] else [])]