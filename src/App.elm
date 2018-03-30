module Main exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Mouse
import Keyboard
import MazeGen as MG exposing (Maze)
import Utils as U

-- MODEL

type alias Model =
    { maze: Maze
    , pos: (Int, Int)
    } 


init : ( Model, Cmd Msg )
init =
    (Model (MG.genMaze 25) (0,0), Cmd.none)



-- MESSAGES


type Msg
    = KeyMsg Keyboard.KeyCode



-- VIEW


view : Model -> Html Msg
view {maze,pos} =
    let
        mazeRows = MG.asciiMaze maze
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
update msg model =
    case msg of
        KeyMsg code ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
