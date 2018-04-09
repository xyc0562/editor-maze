module View exposing (view, renderEmptyMaze)

import Html exposing (div, span, br, Html, text, button, input)
import Html.Attributes exposing (style, class, type_, step, value)
import Html.Events exposing (onClick, onInput)
import Collage
import Color as C
import Model exposing (Model, Player, Mode(..), Msg(..))
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

moveCell (xOff, yOff) (x, y) =
    Collage.move (
        (toFloat x) + (getOffset xOff)*(cellDim+wallDim) + wallDim
        ,-1*((toFloat y) + (getOffset yOff)*(cellDim+wallDim) + wallDim)
        )

renderCell : C.Color -> ( Float, Float ) -> ( Int, Int ) -> Collage.Form
renderCell c offsets coords =
    Collage.rect cellDim cellDim 
    |> Collage.filled c
    |> moveCell offsets coords

renderHWall : Float -> Float -> C.Color -> ( Float, Float ) -> ( Int, Int ) -> Collage.Form
renderHWall w l c ( xOff, yOff ) ( x, y ) =
    Collage.rect l w
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

cGray = C.rgba 236 240 241 0.85

renderNth _ _ = Collage.rect 0 0 |> Collage.filled (C.rgb 0 0 0) |> Collage.move (0, 0)
renderConcreteCell = renderCell (C.rgb 0 0 0)
renderEmptyCell = renderCell cGray

renderPlayer : Int -> Player -> List Player -> ( Float, Float ) -> ( Int, Int ) -> Collage.Form
renderPlayer id me players offsets coords =
    let
        vimSrc = "img/vim.png"
        emacsSrc = "img/emacs.png"
        vimMeSrc = "img/vim.png"
        emacsMeSrc = "img/emacs.png"
        allPlayers = me::players
        player =
            List.filter (\p -> p.pos.id == id) allPlayers |> List.head
        src =
            case player of
                Just p ->
                    if p.mode == ModeVim then
                        if p == me then vimMeSrc else vimSrc
                    else 
                        if p == me then emacsMeSrc else emacsSrc
                _ -> emacsSrc
    in
        Element.image cellDim cellDim src
        |> Collage.toForm
        |> moveCell offsets coords

renderConcreteVWall = renderVWall (cellDim+wallDim*2) (C.rgb 0 0 0)
renderBreachedVWall = renderVWall cellDim cGray

renderConcreteHWall = renderHWall wallDim (cellDim+wallDim*2) (C.rgb 0 0 0)
renderBreachedHWall = renderHWall wallDim cellDim cGray

renderStartHWall = renderHWall (wallDim*2) cellDim (C.rgb 232 64 56)
renderEndHWall = renderHWall (wallDim*2) cellDim (C.rgb 56 147 208)

renderEmptyMaze : Maze -> Element.Element
renderEmptyMaze {cells,walls} =
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
        canvasDim = dim*(cellDim+wallDim)+2*wallDim
        halfOffset = round (-1*((toFloat (canvasDim - cellDim - wallDim))/2))
        goe x y ll r1 r2 =
            case get2 x y ll of
                Nothing -> r1
                Just IntactWall -> r1
                Just BreachedWall -> r2
        carve b x y =
            if x < dim*2+1 then
                if y < dim*2+1 then
                    carve (set2 x y renderEmptyCell b) x (y+2)
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
                in (set2 0 1 renderStartHWall b) |> (set2 (2*dim) (2*dim-1) renderEndHWall)

    in
        List.indexedMap (\y row ->
            List.indexedMap (\x render -> 
                render (toFloat x,toFloat y) (halfOffset, halfOffset)
            ) (Array.toList row)
        ) (Array.toList model)
        |> U.flatten2D
        |> Collage.collage canvasDim canvasDim 

updateMaze : Maze -> Element.Element -> List Player -> Html Msg
updateMaze {cells} canvas players =
    let
        dim =
            Array.length cells

        canvasDim =
            dim*(cellDim+wallDim)+2*wallDim

        halfOffset =
            round (-1*((toFloat (canvasDim - cellDim - wallDim))/2))

        renderPlayerOnCanvas {pos,mode} =
            let
                {x,y,id} = pos
            in
                renderPlayer id players (toFloat (2*y+1), toFloat (2*x+1)) (halfOffset, halfOffset)

        playerForms =
            List.map renderPlayerOnCanvas players

        forms =
            [Collage.toForm canvas] ++ playerForms
    in
        forms
            |> Collage.collage canvasDim canvasDim 
            |> Element.toHtml

renderMainButton : String -> String -> String -> Msg -> Html Msg
renderMainButton bg color txt msg =
        button
            [ style
                [ ( "background", bg )
                , ( "color", color)
                , ( "border", "0" )
                , ( "bottom", "30px" )
                , ( "cursor", "pointer" )
                , ( "display", "block" )
                , ( "font-size", "18px" )
                , ( "font-weight", "300" )
                , ( "height", "60px" )
                , ( "left", "30px" )
                , ( "line-height", "60px" )
                , ( "outline", "none" )
                , ( "padding", "0" )
                , ( "margin-left", "20px" )
                , ( "width", "160px" )
                , ( "margin-top", "10px" )
                ]
            , onClick msg
            ]
            [ text txt ]

renderModeButton : Player -> Html Msg
renderModeButton player =
    let
        ( txt, msg ) =
            case player.mode of
                ModeEmacs ->
                    ( "Use Vim Mode", UseMode ModeVim )

                ModeVim ->
                    ( "Use Emacs Mode", UseMode ModeEmacs )
    in
        renderMainButton "#95c43d" "#ffffff" txt msg

renderResetButton : Html Msg
renderResetButton =
    renderMainButton "#34495f" "#ffffff" "New Game" Reset

renderSizeInput : Int -> Html Msg
renderSizeInput size =
    div
        [ style
            [ ( "font-size", "18px" ) ]]
        [ span
            []
            [ text "Maze Size: " ]
        , input [ type_ "number"
                , Html.Attributes.min "18"
                , Html.Attributes.max "40"
                , onInput UpdateSize
                , value (toString size)
                , style
                    [ ( "font-size", "18px" )
                    , ( "margin-right", "20px" )
                    ]
                ]
                []
        ]

renderPanel : Model -> Html Msg
renderPanel ({me,time,size} as model) =
    let
        {mode} = me

        renderModeDesc =
            span
                [ style
                    [ ( "margin-top", "20px" )
                    , ( "font-size", "20px" )
                    ]
                ]
                [text (if mode == ModeEmacs then "Current Mode: Emacs" else "Current Mode: Vim")]

        renderControlDoc =
            let keys =
                    case mode of
                        ModeEmacs -> ["Ctrl-p", "Ctrl-n", "Ctrl-b", "Ctrl-f"]
                        ModeVim -> ["k", "j", "h", "l"]

                rows =
                    List.map2 (++) ["Up: ", "Down: ", "Left: ", "Right: "] keys
                
                ctrlSpans =
                    (List.map (\s -> span [ style [ ( "display", "block" ) ] ] [text s]) rows)

                allSpans =
                    case mode of
                        ModeVim -> ctrlSpans
                        ModeEmacs ->
                            let s =
                                span
                                    [ style
                                        [ ( "font-size", "18px" )
                                        , ("font-weight", "bold" )
                                        , ("color", "#e84038" )
                                        , ("display", "block" )
                                        , ("margin-bottom", "15px" )
                                        ]
                                    ]
                                    [ text "Use Full Screen Mode!" ]
                            in
                                s::ctrlSpans
            in
                div
                    [ style
                        [ ( "margin-top", "10px" )
                        , ( "margin-bottom", "10px" )
                        ]
                    ]
                    allSpans    

        renderTitle =
            div
                [ style
                    [ ( "color", "rgb(52, 73, 95)" ) 
                    ]
                ]
                [ span
                    [ style
                        [ ( "font-size", "35px" ) ]
                    ]
                    [ text "Editor Maze" ]
                , span
                    [ style 
                        [ ( "display", "block" ) ]
                    ]
                    [ text "(MP)" ]
                ]
        renderTime =
            let
                pad = U.pad 2 "0"
                seconds = time%60 |> toString |> pad
                minutes = floor ((toFloat time)/60) |> flip (%) 60 |> toString |> pad
                hours = floor ((toFloat time)/3600) |> flip (%) 60 |> toString |> pad
            in
                span
                    [ style
                        [ ( "font-size", "35px" )
                        , ( "background", "rgb(57, 147, 208)"  )
                        , ( "color", "#ffffff" )
                        , ( "line-height", "60px" )
                        , ( "margin-top", "20px" )
                        ]
                    ]
                    [ ( text ( hours ++ ":" ++ minutes ++ ":" ++ seconds ) ) ]
    in
        div
        [ style
            [ ( "display", "inline-block" )
            , ( "flex", "1" )
            , ( "width", "200px" )
            , ( "text-align", "center" ) 
            , ( "padding-left", "25px" ) 
            , ( "vertical-align", "top" )
            , ( "position", "relative" )
            ]
        , class "panel"
            ]
        [ renderTitle
        , renderTime
        , renderModeDesc
        , renderControlDoc
        , renderModeButton me
        , div
            [ style
                [ ( "position", "absolute" )
                , ( "bottom", "0" )
                , ( "width", "100%" )
                ]
            ]
            [ renderSizeInput size
            , renderResetButton
            ]
        ]

--renderResult : Result -> Html Msg
renderResult {mode,desc} =
    case mode of
        Nothing -> div [] []
        Just m ->
            let
                title = (if m == ModeEmacs then "Emacs" else "Vim") ++ " player wins!"
            in
                div
                    [ style
                        [ ( "height", "100%" )
                        , ( "transform", "translateY(-100%)" )
                        , ( "background", "rgba(236, 240, 241, 0.95)" )
                        , ( "color", "#35495e" )
                        , ( "font-size", "18px" )
                        , ( "position", "absolute" )
                        , ( "width", "100%" )
                        , ( "text-align", "left" )
                        ]
                    ]
                    [ div
                        [ style
                            [ ( "padding", "15px" ) ]
                        ]
                        [ span
                            [ style
                                [ ( "display", "block" ) 
                                , ( "font-size", "26px" )
                                , ( "text-align", "center" )
                                , ( "margin-bottom", "30px" )
                                ]
                            ]
                            [ text title ]

                        , span
                            []
                            [ text desc ]

                        , span
                            [ style
                                [ ( "margin-top", "30px" )
                                , ( "font-size", "20px" )
                                , ( "display", "block" )
                                , ( "position", "absolute" )
                                , ( "left", "0" )
                                , ( "bottom", "15px" )
                                , ( "width", "100%" )
                                , ( "text-align", "center" )
                                ]
                            ]
                            [ text "Press Enter to continue." ]
                        ]
                    ]

view : Model -> Html Msg
view ({canvas,maze,me,players,result} as model) =
    let
        {pos} = me
        newMaze = List.foldl (\player mz -> MG.updatePos mz player.pos) maze players
        genRow s =
            [ span [ style [ ("margin", "-2px") ] ] [text s]
            , br [] []
            ]
    in 

        div
            [ style
                [ ( "display", "flex" ) ]
            ]
            [ div
                [ style
                    [ ( "display", "inline-block" )
                    , ( "position", "relative" ) ]
                ]
                [ updateMaze newMaze canvas (me::players) -- Main game
                , renderResult result
                ] 
            , renderPanel model -- Panel
            ]


