module Main exposing (..)

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
import Messages exposing (..)

init : () -> ( Model, Cmd Msg )
init () =
    (
        { maze = MG.genMaze 25
        , me = Player (Pos 1 0 0) ModeVim
        , players = []
        , combos = Keyboard.Combo.init keyboardCombos ComboMsg
        },
        Cmd.none
    )

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({maze,me} as model) =
    case msg of
        ComboMsg msg ->
            let
                (updatedKeys,cmd) =
                    Keyboard.Combo.update msg model.combos
            in
                {model | combos = updatedKeys} ! [cmd]

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
                if newPos == Pos id (dim-1) (dim-1)
                then init ()
                else { model | me = (Player newPos mode) } ! []


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg
        , Keyboard.Combo.subscriptions model.combos]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init ()
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
