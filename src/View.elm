module View exposing (view)

import Html exposing (div, span, br, Html, text, button)
import Html.Attributes exposing (style)
import Collage
import Color as C
import Messages exposing (Msg)
import Model exposing (Model)
import MazeGen as MG exposing (Maze, Pos, Wall(..), Cell(..), set2, get2)
import Element
import Char
import Array
import Utils as U

cellDim = 24
wallDim = 2

getOffset : Float -> Float
getOffset raw =
    toFloat (floor (raw/2))

renderCell : C.Color -> ( Float, Float ) -> ( Int, Int ) -> Collage.Form
renderCell c ( xOff, yOff ) ( x, y ) =
    Collage.rect cellDim cellDim 
    |> Collage.filled c
    |> Collage.move (
        (toFloat x) + (getOffset xOff)*(cellDim+wallDim) + wallDim
        ,-1*((toFloat y) + (getOffset yOff)*(cellDim+wallDim) + wallDim)
        )

renderHWall : Float -> C.Color -> ( Float, Float ) -> ( Int, Int ) -> Collage.Form
renderHWall l c ( xOff, yOff ) ( x, y ) =
    Collage.rect l wallDim
    |> Collage.filled c
    |> Collage.move (
        (toFloat x) + (getOffset xOff)*(cellDim+wallDim)+wallDim
        , -1*((toFloat y) + (getOffset yOff)*(cellDim+wallDim) + wallDim) + (cellDim+wallDim)/2
        )

renderVWall : Float -> C.Color -> ( Float, Float ) -> ( Int, Int ) -> Collage.Form
renderVWall l c ( xOff, yOff ) ( x, y ) =
    Collage.rect wallDim l
    |> Collage.filled c
    |> Collage.move (
        (toFloat x) + (getOffset xOff)*(cellDim+wallDim) - (cellDim-wallDim)/2
        , -1*((toFloat y) + (getOffset yOff)*(cellDim+wallDim) + wallDim)
        )


renderNth _ _ = Collage.rect 0 0 |> Collage.filled (C.rgb 0 0 0) |> Collage.move (0, 0)
renderConcreteCell = renderCell (C.rgb 0 0 0)
renderEmptyCell = renderCell (C.rgb 182 182 182)
renderPlayer = renderCell (C.rgb 255 0 0)

renderConcreteVWall = renderVWall (cellDim+wallDim*2) (C.rgb 0 0 0)
renderBreachedVWall = renderVWall cellDim (C.rgb 182 182 182)

renderConcreteHWall = renderHWall (cellDim+wallDim*2) (C.rgb 0 0 0)
renderBreachedHWall = renderHWall cellDim (C.rgb 182 182 182)

renderMaze : Maze -> Html Msg
renderMaze {cells,walls} =
    let
        (xWalls, yWalls) = walls
        dim = Array.length cells
        base = Array.initialize (dim*2+1) (
            \x -> (Array.initialize (dim*2+1) (
                \y ->
                    if x == dim*2 && y == dim*2 then
                        renderNth
                    else if x == dim*2 then
                        renderConcreteHWall
                    else if y == dim*2 then
                        renderConcreteVWall
                    else renderEmptyCell
                    )))
        canvasDim = dim*2*(cellDim+wallDim)+wallDim
        halfOffset = ceiling ((toFloat (canvasDim - dim + wallDim))/(-2))
        goe x y ll r1 r2 =
            case get2 x y ll of
                Nothing -> r1
                Just IntactWall -> r1
                Just BreachedWall -> r2
        carve b x y =
            if x < dim*2+1 then
                if y < dim*2+1 then
                    let
                        char = case get2 (floor ((toFloat x)/2)) (floor ((toFloat y)/2)) cells of
                            Just (OccupiedCell id) -> renderPlayer
                            _ -> renderEmptyCell
                    in 
                        carve (set2 x y char b) x (y+2)
                else carve b (x+2) 1
            else b
        carveX b x y =
            if x < dim+1 then
                if y < dim+1 then carveX (set2 (2*x+1) (2*y) (goe x y xWalls renderConcreteVWall renderBreachedVWall) b) x (y+1)
                else carveX b (x+1) 0
            else b
        carveY b x y =
            if x < dim+1 then
                if y < dim+1 then carveY (set2 (2*x) (2*y+1) (goe x y yWalls renderConcreteHWall renderBreachedHWall) b) x (y+1)
                else carveY b (x+1) 0
            else b
        model = let b = carveX (carveY (carve base 1 1) 0 0) 0 0
                in (set2 0 1 renderBreachedHWall b) |> (set2 (2*dim) (2*dim-1) renderBreachedHWall)

    in
        List.indexedMap (\y row ->
            List.indexedMap (\x render -> 
                render (toFloat x,toFloat y) (halfOffset, halfOffset)
            ) (Array.toList row)
        ) (Array.toList model)
        |> U.flatten2D
        |> Collage.collage canvasDim canvasDim 
        |> Element.toHtml

view : Model -> Html Msg
view {maze,me} =
    let
        {pos} = me
        newMaze = MG.updatePos maze pos
        genRow s =
            [ span [ style [ ("margin", "-2px") ] ] [text s]
            , br [] []
            ]
    in 
        div [ style
                [ ("font-family", "monospace")
                , ("line-height", "0.8em")
                , ("font-size", "2em")
                ]
            ]
            [(renderMaze newMaze)]


