module Main exposing (..)

import Array
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
import WebSocket
import Json.Encode as JE
import Json.Decode exposing (Decoder, decodeString, int, string, list, field, bool)
import Json.Decode.Pipeline exposing (required, decode)

-- Server related

socketServer : String
socketServer =
  "ws://127.0.0.1:3000"

sockSend = WebSocket.send socketServer
sockListen = WebSocket.listen socketServer

-- Serialization & deserialization

type alias MazeUpdate =
    { xWalls: List (List Int)
    , yWalls: List (List Int)
    }

type alias PlayerUpdate =
    { id: Int
    , x: Int
    , y: Int
    , mode: String
    }

type alias StateUpdate =
    { players: List PlayerUpdate }

type alias NewGameUpdate =
    { me: PlayerUpdate
    , maze: MazeUpdate
    , players: List PlayerUpdate
    }

encodeMazeUpdate : Model -> Bool -> String
encodeMazeUpdate {maze,me} newGame =
    let
        (xWalls, yWalls) = maze.walls
        mapper walls =        
            Array.map (\row ->
                Array.map (\w ->
                    if w == MG.IntactWall
                    then JE.int 1
                    else JE.int 0) row |> JE.array)
                    walls |> JE.array
        obj =
            JE.object
                [ ( "t", JE.string "mazeUpdate" )
                , ( "id", JE.int me.pos.id )
                , ( "newGame", JE.bool newGame )
                , ( "maze", JE.object
                    [ ( "xWalls", mapper xWalls )
                    , ( "yWalls", mapper yWalls )
                    ] )
                ]
    in
        JE.encode 0 obj


resetPos : Player -> Player
resetPos p = { p | pos = Pos p.pos.id 0 0 }

init : Maybe Model -> Bool -> ( Model, Cmd Msg )
init mModel newGame =
    let
        (rawSize,id) =
            case mModel of
                Nothing -> (25, -1)
                Just m -> (m.size, m.me.pos.id)

        size =
            if rawSize < 18 then 18 else if rawSize > 50 then 50 else rawSize

        maze =
            MG.genMaze size

        freshModel =
            { maze = renderEmptyMaze maze
            , me = Player (Pos id 0 0) ModeVim
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
        (newModel, sockSend (encodeMazeUpdate newModel newGame))
    
inflateMaze : MazeUpdate -> Maze
inflateMaze {xWalls,yWalls} =
    let
        mapWalls walls = 
            List.map (\row ->
                List.map (\w ->
                    if w == 1
                    then MG.IntactWall
                    else MG.BreachedWall) row |> Array.fromList)
                    walls |> Array.fromList
        dim = (List.length xWalls) - 1
        cleanCells = Array.repeat dim MG.CleanCell
    in
        { walls = (mapWalls xWalls, mapWalls yWalls)
        , cells = Array.repeat dim cleanCells
        }
    
inflatePlayer : PlayerUpdate -> Player
inflatePlayer {id,x,y,mode} =
    Player (Pos id x y) (if mode == "ModeEmacs" then ModeEmacs else ModeVim)

-- UPDATE

mazeUpdateDecoder : Decoder MazeUpdate
mazeUpdateDecoder =
    decode MazeUpdate
        |> required "xWalls" (list (list int))
        |> required "yWalls" (list (list int))

playerUpdateDecoder : Decoder PlayerUpdate
playerUpdateDecoder =
    decode PlayerUpdate
        |> required "id" int
        |> required "x" int
        |> required "y" int
        |> required "mode" string

stateUpdateDecoder : Decoder StateUpdate
stateUpdateDecoder =
    decode StateUpdate
        |> required "players" (list playerUpdateDecoder)

newGameDecoder : Decoder NewGameUpdate
newGameDecoder =
    decode NewGameUpdate
        |> required "me" playerUpdateDecoder
        |> required "maze" mazeUpdateDecoder
        |> required "players" (list playerUpdateDecoder)

handleNewGameMsg : String -> Model -> Model
handleNewGameMsg str m =
    case decodeString newGameDecoder str of
        Err _ -> Debug.log ("Malformed newGame message: " ++ str) m
        Ok {me,maze,players} ->
            let
                newMaze = inflateMaze maze
                dim = MG.getDimension newMaze
            in
                { m
                | maze = newMaze
                , me = inflatePlayer me
                , players = List.map inflatePlayer players
                , time = 0
                , size = dim
                }

handleStateUpdateMsg : String -> Model -> Model
handleStateUpdateMsg str m =
    case decodeString stateUpdateDecoder str of
        Err _ -> Debug.log ("Malformed state udpate message: " ++ str) m
        Ok state ->
            { m
            | players = List.map inflatePlayer state.players
            }

handleSockMsg : String -> Model -> ( Model, Cmd Msg )
handleSockMsg str m =
    let
        t = decodeString (field "t" string) str
        newModel =
            case t of
                Ok "newGameUpdate" -> handleNewGameMsg str m
                Ok "stateUpdate" -> handleStateUpdateMsg str m
                _ -> Debug.log ("Cannot find valid type from sock msg: " ++ str) m
    in
        newModel ! [] 


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
            let
                newModel = { model | time = time + 1}
                obj = 
                    JE.object
                        [ ( "t", JE.string "keepAlive" )
                        , ( "id", JE.int me.pos.id )
                        ]
                msg =
                    JE.encode 0 obj
                cmd = sockSend msg
            in 
                ( newModel, cmd )

        Reset ->
            init (Just model) True

        UpdateSize str ->
            let
                newSize = case String.toInt str of
                    Ok s -> s
                    Err _ -> model.size
            in
                {model | size = newSize } ! []

        SockMsg str -> handleSockMsg str model

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
                    -- Start a new game
                    init (Just model) True
                else let
                        newModel = { model | time = time + 1}
                        obj = 
                            JE.object
                                [ ( "t", JE.string "playerStateUpdate" )
                                , ( "id", JE.int id )
                                , ( "x", JE.int newPos.x )
                                , ( "y", JE.int newPos.y )
                                , ( "mode", JE.string (if mode == ModeVim then "ModeVim" else "ModeEmacs") )
                                ]
                        msg = JE.encode 0 obj
                    in
                        ( { model | me = (Player newPos mode) }, sockSend msg )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg
        , Keyboard.Combo.subscriptions model.combos
        , Time.every second (always Tick)
        , WebSocket.listen socketServer SockMsg
        ]

-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init Nothing False
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
