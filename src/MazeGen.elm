module MazeGen exposing (..)
import Native.Random
import Array exposing (Array, get, set, fromList)
import String
import Debug

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

type Wall = IntactWall | BreachedWall

type Cell = CleanCell | DirtyCell

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
        goe df x y ll fn =
            case get2 x y ll of
                Nothing -> df
                Just a -> fn a

        aux (x, y) s {cells,walls} =
            goe (Maze cells walls) x y cells (\c ->
                let
                    newCells = set2 x y DirtyCell cells
                    nbs = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
                    fn (i,j) =
                        case get2 i j newCells of
                            Nothing -> False
                            Just DirtyCell -> False
                            Just CleanCell -> True
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
                            [] -> Maze newCells walls)
    in
        aux (0,0) [] (initMaze n)

printMaze : Maze -> Array ()
printMaze {cells,walls} =
    let
        (xWalls, yWalls) = walls
        dim = Array.length cells
        base = Array.repeat (dim*2+1) (Array.repeat (dim*2+1) '#')
        goe x y ll =
            case get2 x y ll of
                Nothing -> '#'
                Just IntactWall -> '#'
                Just BreachedWall -> ' '
        carve b x y =
            if x < dim*2+1 then
                if y < dim*2+1 then carve (set2 x y ' ' b) x (y+2)
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
        model = carveX (carveY (carve base 1 1) 0 0) 0 0
    in
        Array.map (flip Debug.log ()) (Array.map (\arr -> String.fromList (Array.toList arr)) model)
