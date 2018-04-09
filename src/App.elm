module Main exposing (..)

import Time exposing (Time, second)
import Html exposing (program)
import Char exposing (fromCode, toCode)
import Mouse
import Keyboard.Combo
import Keyboard
import MazeGen as MG exposing (Maze, Pos)
import Utils as U
import Debug
import View exposing (..)
import Model exposing (..)

resetPos : Player -> Player
resetPos p = { p | pos = Pos 1 0 0 }

init : Maybe Model -> ( Model, Cmd Msg )
init mModel =
    let size =
            case mModel of
                Nothing -> 25
                Just m -> if m.size < 18 then 18 else if m.size > 50 then 50 else m.size

        maze =
            MG.genMaze size

        freshModel =
            { maze = maze
            , canvas = renderEmptyMaze maze
            , me = Player (Pos 1 0 0) ModeVim
            , players = []
            , combos = Keyboard.Combo.init keyboardCombos ComboMsg
            , time = 0
            , size = size
            }

        newModel =
            case mModel of
                Nothing -> freshModel
                Just m -> { freshModel | me = resetPos m.me, players = List.map resetPos m.players }
    in
        newModel ! []
    

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({maze,me,time} as model) =
    case msg of

        ComboMsg msg ->
            let
                (updatedKeys,cmd) =
                    Keyboard.Combo.update msg model.combos
            in
                {model | combos = updatedKeys} ! [cmd]

        UseMode m ->
            {model | me = { me | mode = m } }  ! []

        Tick ->
            {model | time = time + 1} ! []

        Reset ->
            init (Just model)

        UpdateSize str ->
            let
                newSize = case String.toInt str of
                    Ok s -> s
                    Err _ -> model.size
            in
                {model | size = newSize } ! []

        _ ->
            let 
                {pos,mode} = me
                dim = MG.size maze
                {x,y,id} = pos
                (dx,dy) =
                    case mode of

                        ModeVim ->
                        case msg of
                            KeyMsg code ->
                                case fromCode code of
                                    'H' -> (0,-1)
                                    'J' -> (1,0)
                                    'K' -> (-1,0)
                                    'L' -> (0,1)
                                    _ -> (0,0)
                            _ -> (0,0)

                        ModeEmacs ->
                        case msg of
                            RealComboMsg Left -> (0,-1)
                            RealComboMsg Right -> (0,1)
                            RealComboMsg Up -> (-1,0)
                            RealComboMsg Down -> (1,0)
                            _ -> (0,0)
                x1 = x + dx
                y1 = y + dy
                hasWall = MG.hasWall (x,y) (x1,y1) maze
                newPos = if hasWall then pos else Pos id x1 y1
            in
                if newPos == Pos id (dim-1) (dim-1) then
                    init (Just model)
                else { model | me = (Player newPos mode) } ! []


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg
        , Keyboard.Combo.subscriptions model.combos
        , Time.every second (always Tick)
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init Nothing
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
