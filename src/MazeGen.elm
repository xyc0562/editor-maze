module MazeGen exposing (..)
import Native.Random
import Array exposing (Array, get, set, fromList)
import String
import Debug
import Char

-- Utils
get2 : Int -> Int -> Array (Array a) -> Maybe a
get2 x y ll =
    case get x ll of
        Nothing -> Nothing
        Just l -> get y l

set2 : Int -> Int -> a -> Array (Array a) -> Array (Array a)
set2 x y v ll =
    case get x ll of
        Nothing -> ll
        Just l -> set y v l |> flip (set x) ll

randFirst : (a -> Bool) -> List a -> Maybe a
randFirst fn l =
    let
        arr = fromList (List.filter fn l)
        idx = floor (Native.Random.rand()*(toFloat (Array.length(arr))))
    in
        get idx arr

-- Types

type alias Pos = 
    { id: Int
    , x: Int
    , y: Int
    }

type Wall
    = IntactWall
    | BreachedWall

type Cell
    = CleanCell
    | DirtyCell
    | OccupiedCell Int

type alias Maze =
    { cells: Array (Array Cell)
    , walls: (Array (Array Wall), Array (Array Wall))
    }

initMaze : Int -> Maze
initMaze n =
    let
        cells = Array.repeat n (Array.repeat n CleanCell)
        sideWalls = Array.repeat (n+1) (Array.repeat (n+1) IntactWall)
        walls = (sideWalls, sideWalls)
    in
        Maze cells walls

genMaze : Int -> Maze
genMaze n =
    let
        aux (x, y) s {cells,walls} =
            case get2 x y cells of
                Nothing -> Maze cells walls
                Just c ->
                    let
                        newCells = set2 x y DirtyCell cells
                        nbs = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
                        fn (i,j) =
                            case get2 i j newCells of
                                Just CleanCell -> True
                                _ -> False
                    in
                        case randFirst fn nbs of
                            Just (i,j) ->
                                let
                                    (xWalls,yWalls) = walls
                                    newWalls =
                                        if x == i
                                        then (set2 i (max y j) BreachedWall xWalls, yWalls)
                                        else (xWalls, set2 (max x i) j BreachedWall yWalls)
                                in
                                    aux (i,j) ((x,y)::s) (Maze newCells newWalls)
                            Nothing -> case s of
                                idx::s -> aux idx s (Maze newCells walls)
                                [] -> Maze newCells walls
    in
        aux (0,0) [] (initMaze n)
        
size : Maze -> Int
size {cells} = Array.length cells

updatePos : Maze -> Pos -> Maze
updatePos {cells,walls} {id,x,y} =
    let 
        newCells = set2 x y (OccupiedCell id) cells
    in
        Maze newCells walls

hasWall : (Int, Int) -> (Int, Int) -> Maze -> Bool
hasWall (x1, y1) (x2, y2) {cells, walls} =
    let
        (xWalls, yWalls) = walls
        wall =
            if x1 == x2 then get2 x1 (max y1 y2) xWalls
            else get2 (max x1 x2) y1 yWalls
    in
        case wall of
            Just BreachedWall -> False
            _ -> True

-- Generate an ASCII representation of the maze
asciiMaze : Maze -> List String
asciiMaze {cells,walls} =
    let
        (xWalls, yWalls) = walls
        dim = Array.length cells
        base = Array.repeat (dim*2+1) (Array.repeat (dim*2+1) '█')
        goe x y ll =
            case get2 x y ll of
                Nothing -> '█'
                Just IntactWall -> '█'
                Just BreachedWall -> '░'
        carve b x y =
            if x < dim*2+1 then
                if y < dim*2+1 then
                    let
                        char = case get2 (floor ((toFloat x)/2)) (floor ((toFloat y)/2)) cells of
                            Just (OccupiedCell id) -> Char.fromCode (48 + id)
                            _ -> '░'
                    in 
                        carve (set2 x y char b) x (y+2)
                else carve b (x+2) 1
            else b
        carveX b x y =
            if x < dim+1 then
                if y < dim+1 then carveX (set2 (2*x+1) (2*y) (goe x y xWalls) b) x (y+1)
                else carveX b (x+1) 0
            else b
        carveY b x y =
            if x < dim+1 then
                if y < dim+1 then carveY (set2 (2*x) (2*y+1) (goe x y yWalls) b) x (y+1)
                else carveY b (x+1) 0
            else b
        model = let b = carveX (carveY (carve base 1 1) 0 0) 0 0
                in (set2 0 1 '░' b) |> (set2 (2*dim) (2*dim-1) '░')
    in
        Array.map (\arr -> String.fromList (Array.toList arr)) model |> Array.toList

-- Print ASCII representation of the maze to the console
printMaze : Maze -> ()
printMaze = (flip Debug.log ()) << String.join "\n" << asciiMaze
