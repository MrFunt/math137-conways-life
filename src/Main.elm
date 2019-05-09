import Lib.Color as Color
import Lib.Graphics as Graphics
import Lib.Game as Game
import Lib.Keyboard as Keyboard
import Lib.Memoize as Memoize
import Lib.List as List

import Debug exposing (todo)

import Life exposing (Board, Cell, CellStatus(..), nextBoard)


-- To define a program, we need
--   1. A type for the possible states         type State
--   2. A type for the possible events         type Event
--   3. A transition function                  update : event -> state -> state
--   4. An initial state                       init : state
--   5. A view function                        view : state -> Canvas event


-- A type for your game state. As your game gets more features, you will
-- probably add more fields to this type.
type alias State = { board : Board, paused : Bool }



-- A type for your game events. As your game gets more features, you will
-- probably add more variants to this type.
type Event = NoOp | MouseFlip Cell


main : Game.Game State Event
main =
  Game.game
    { init = initialState
    , update = updateGame
    , view = drawGame
    }


-- This is the board our game will start with.
initialState : State
initialState =
  let
    startingBoard : Board
    startingBoard cell =
      case (cell.x, cell.y) of
        --(0, 0) -> Alive
        --(0, 1) -> Alive
        --(0, 2) -> Alive
        --(3, 5) -> Alive
        --(3, 6) -> Alive
        -- (4, 5) -> Alive
        -- (5, 5) -> Alive
        (10, 29) -> Alive
        (11, 30) -> Alive
        (9, 31) -> Alive
        (10, 31) -> Alive
        (11, 31 ) -> Alive
        _ -> Dead
  in
  {board = startingBoard, paused = True }


-- This function uses the incoming event and the current game state to
-- decide what the next game state should be.

memoStrat : Memoize.MemoizeStrategy (Int, Int) Cell CellStatus
memoStrat = 
  let 
    cellToPair cell = (cell.x, cell.y)
    pairToCell (x0, y0) = {x = x0, y = y0}
    allPairs = List.map cellToPair allCells
    defaultStatus = Dead
  in
  {toKey = cellToPair, fromKey = pairToCell, domain = allPairs, default = defaultStatus}


updateGame : Game.GameEvent Event -> State -> State
updateGame event currentState =
  case event of
    -- What to do when we get a `ClockTick`
    Game.ClockTick timestamp ->
      if currentState.paused
        then currentState
        else
          let
            updatedBoard = nextBoard currentState.board
            memoizedBoard = Memoize.memoize memoStrat updatedBoard
          in
          { board = memoizedBoard, paused = currentState.paused }

    -- What to do when the player presses or releases a key
    -- What to do when we get a `NoOp`
    Game.Custom NoOp->
      currentState
    Game.Keyboard (Keyboard.KeyEventDown Keyboard.KeyP) ->
        case currentState.paused of
          True -> {board = currentState.board, paused = False}
          False -> {board = currentState.board, paused = True}
    Game.Keyboard (Keyboard.KeyEventDown Keyboard.KeyRight) ->
        case currentState.paused of
          True -> {board = Memoize.memoize memoStrat (nextBoard currentState.board), paused = True}
          False -> currentState
    Game.Custom (MouseFlip somecell) ->
      case currentState.paused of
        True -> {board = flipCell somecell currentState.board, paused = True}
        False -> {board = flipCell somecell currentState.board, paused = False}
    Game.Keyboard _ ->
      currentState


-- Pick a size for the game board.
-- Hint: Use this when you go to write `drawCell` and `drawGame`
boardSize : Int
boardSize = 50

invertStatus: CellStatus -> CellStatus
invertStatus x = case x of
  Alive -> Dead
  Dead -> Alive

flipCell : Cell -> Board -> Board
flipCell c b = 
  let
      newBoard : Cell -> CellStatus
      newBoard someCell = if someCell == c
        then invertStatus (b(c))
        else b(someCell)

  in
    newBoard


-- The list of all cells based on your `boardSize`.
allCells : List Cell
allCells =
  let
    range = List.range 0 boardSize
    toCell (x_coord, y_coord) = { x = x_coord, y = y_coord }
  in
  List.map toCell (List.cross range range)


-- This function will use the game state to decide what to draw on the screen.
drawGame : State -> Graphics.Canvas Event
drawGame state =
  let
    drawCell : Cell -> Graphics.Svg Event
    drawCell cell =
      let
        cellStatus : CellStatus
        cellStatus = state.board cell

        cellColor : Color.Color
        cellColor =
          case cellStatus of
            Dead -> Color.black
            Alive -> Color.fuchsia

      in
      Graphics.drawRect
        {fill = cellColor
        , x0 = (toFloat cell.x) * 10
        , y0 = (toFloat cell.y) * 10
        , width = 10
        , height = 10
        , onClick = Just (MouseFlip cell)
        }

    cells : List (Graphics.Svg Event)
    cells = List.map drawCell allCells
  in
  Graphics.canvas
  { title = "Ching Chong"
  , widthPx = 500
  , heightPx = 500
  , xMax = 500
  , yMax = 500
  , children = cells
  }
-- type Maybe a = Nothing | Just a