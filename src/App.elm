module Main exposing (..)

import Char exposing (fromCode, toCode)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Mouse
import Keyboard
import MazeGen as MG exposing (Maze, Pos)
import Utils as U
import Debug

-- MODEL

type alias Model =
    { maze: Maze
    , pos: Pos
    } 

init : () -> ( Model, Cmd Msg )
init () =
    (Model (MG.genMaze 28) (Pos 1 0 0), Cmd.none)



-- MESSAGES


type Msg
    = KeyMsg Keyboard.KeyCode



-- VIEW


view : Model -> Html Msg
view {maze,pos} =
    let
        newMaze = MG.updatePos maze pos
        mazeRows = MG.asciiMaze newMaze
        genRow s =
            [ span [ css [ margin (px -2) ] ] [text s]
            , br [] []
            ]
    in 
        div [ css
                [ fontFamily monospace
                , lineHeight (Css.em 0.8)
                , fontSize (Css.em 2)
                ]
            ]
            (List.map genRow mazeRows |> U.flatten2D)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg {maze,pos} =
    case msg of
        KeyMsg code ->
            let
                dim = MG.size maze
                {x,y,id} = pos
                (dx,dy) =
                    case fromCode code of
                        'H' -> (0,-1)
                        'J' -> (1,0)
                        'K' -> (-1,0)
                        'L' -> (0,1)
                        _ -> (0,0)
                x1 = x + dx
                y1 = y + dy
                hasWall = MG.hasWall (x,y) (x1,y1) maze
                newPos = if hasWall then pos else Pos id x1 y1
            in
                if newPos == Pos id (dim-1) (dim-1)
                then init ()
                else (Model maze newPos, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init ()
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
