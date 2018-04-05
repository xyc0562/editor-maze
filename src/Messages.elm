module Messages exposing (..)
import Keyboard.Combo
import Keyboard

type ComboDir
    = Right
    | Left
    | Up
    | Down

type Msg
    = KeyMsg Keyboard.KeyCode
    | ComboMsg Keyboard.Combo.Msg
    | RealComboMsg ComboDir

