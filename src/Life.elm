-- Necessary code to allow Life to run
module Life exposing (..)

-- Cell is a new type, that has x and y coordinates
type alias Cell = { x : Int, y : Int }

-- We create a new type "Cell Status" that is basically a Bool
-- Alive is True, Dead is False
type CellStatus = Dead | Alive

-- Board takes a cell (x,y) and returns whether it is dead or alive
-- but board is contingent on how many living neighbors it has?
type alias Board = Cell -> CellStatus



-- Takes the number of living neighbors along with a Cell Status
-- and returns the next Cell Status in the proceeding tick
-- needs "numberofLivingNeighbors" to work
getStatus : Int -> CellStatus -> CellStatus
getStatus numberOfLivingNeighbors currentstatus =
    let
        newStatus : CellStatus
        newStatus =
            if numberOfLivingNeighbors < 2 && currentstatus == Alive
            then Dead
            else if numberOfLivingNeighbors > 3 && currentstatus == Alive
                then Dead
                else if numberOfLivingNeighbors == 3 && currentstatus == Dead
                    then Alive
                    else currentstatus
    in
    newStatus


-- numericCell converts a Cell Status (Alive or Dead) into 1 for Alive, 
-- 0 for Dead (like numericBool)
numericCell : CellStatus -> Int
numericCell c =
    case c of
        Alive -> 1
        Dead -> 0

-- Board takes a coordinate (Cell) and returns a cell status
-- So living neighbors takes a Board (returned cell status of a cell)
-- then returns the sum of all of those neighbors that are alive
-- That sum is an Integer
livingNeighbors : Board -> Cell -> Int
livingNeighbors currentBoard cell =
    let

        myCoordinateList : List Cell
        myCoordinateList = neighborCoordinateList cell
        
        neighborStatusList : List CellStatus
        neighborStatusList = List.map currentBoard myCoordinateList

        listOfInts : List Int
        listOfInts = List.map numericCell neighborStatusList

        sumOfList : Int
        sumOfList = List.sum listOfInts
    in 
    sumOfList

-- Creates a list of the neighboring coordinates of a cell
neighborCoordinateList : Cell -> List Cell
neighborCoordinateList {x,y} = 
    let
        cell1 : Cell
        cell1 = {x = x + 1, y=y}

        cell2 : Cell
        cell2 = {x = x + 1, y = y + 1}

        cell3 : Cell
        cell3 = {x = x + 1, y = y - 1}

        cell4 : Cell
        cell4 = {x = x, y = y + 1}

        cell5 : Cell
        cell5 = {x = x, y = y - 1}

        cell6 : Cell
        cell6 = {x = x - 1, y = y + 1}

        cell7 : Cell
        cell7 = {x = x - 1, y=y}

        cell8 : Cell
        cell8 = {x = x - 1, y = y - 1}

        myArray : List Cell
        myArray = [cell1, cell2, cell3, cell4, cell5, cell6, cell7, cell8]
    in
    myArray



-- Maps a list of coordinates to a list of Cell Statuses

--Board{x = x + 1, y=y}
--Board{x = x + 1, y = y + 1}
--Board{x = x + 1, y = y - 1}
--Board{x = x, y + 1}
--Board{x = x, y - 1}
--Board{x = x - 1, y + 1}
--Board{x = x - 1, y}
--Board{x = x - 1, y - 1}

 
-- this function will represent one tick of our game,
-- it will use the two functions above

nextBoard : Board -> Board
nextBoard currentBoard cell =
    let
        currentNeighbors = livingNeighbors currentBoard cell
        currentStatus = getStatus currentNeighbors (currentBoard cell)
    in
    currentStatus



