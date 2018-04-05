module Main exposing (..)

import Char exposing (fromCode, toCode)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Mouse
import Keyboard.Combo
import Keyboard
import MazeGen as MG exposing (Maze, Pos)
import Utils as U
import Debug

-- MODEL

keyboardCombos : List (Keyboard.Combo.KeyCombo Msg)
keyboardCombos =
    [ Keyboard.Combo.combo2 ( Keyboard.Combo.control, Keyboard.Combo.f ) (RealComboMsg Right)
    , Keyboard.Combo.combo2 ( Keyboard.Combo.control, Keyboard.Combo.b ) (RealComboMsg Left)
    , Keyboard.Combo.combo2 ( Keyboard.Combo.control, Keyboard.Combo.p ) (RealComboMsg Up)
    , Keyboard.Combo.combo2 ( Keyboard.Combo.control, Keyboard.Combo.n ) (RealComboMsg Down)
    ]

type Mode
    = ModeVim
    | ModeEmacs

type alias Player =
    { pos: Pos
    , mode: Mode
    }

type alias Model =
    { maze: Maze
    , me: Player
    , players: List Player
    , combos: Keyboard.Combo.Model Msg
    } 

init : () -> ( Model, Cmd Msg )
init () =
    (
        { maze = MG.genMaze 28
        , me = Player (Pos 1 0 0) ModeVim
        , players = []
        , combos = Keyboard.Combo.init keyboardCombos ComboMsg
        },
        Cmd.none
    )


-- MESSAGES

type ComboDir
    = Right
    | Left
    | Up
    | Down

type Msg
    = KeyMsg Keyboard.KeyCode
    | ComboMsg Keyboard.Combo.Msg
    | RealComboMsg ComboDir



-- VIEW


view : Model -> Html Msg
view {maze,me} =
    let
        {pos} = me
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
