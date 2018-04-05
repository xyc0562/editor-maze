module Model exposing (..)
import MazeGen exposing (Pos, Maze)
import Messages exposing (Msg(..), ComboDir(..))
import Keyboard.Combo

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


