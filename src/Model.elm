module Model exposing (..)
import MazeGen exposing (Pos, Maze)
import Keyboard.Combo
import Keyboard
import Element exposing (Element)

--Model

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
    , canvas: Element
    , me: Player
    , players: List Player
    , combos: Keyboard.Combo.Model Msg
    , time: Int
    , size: Int
    } 

-- Messages
type ComboDir
    = Right
    | Left
    | Up
    | Down

type Msg
    = KeyMsg Keyboard.KeyCode
    | ComboMsg Keyboard.Combo.Msg
    | RealComboMsg ComboDir
    | UseMode Mode
    | Tick
    | Reset
    | UpdateSize String
    | SockMsg String
    | SendSockMsg


