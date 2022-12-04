module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick)
import Set


type Color
    = Black
    | White


type Piece
    = Pawn Color
    | Knight Color
    | Bishop Color
    | Rook Color
    | Queen Color
    | King Color
    | Empty


flatBoard : List ( Int, Int )
flatBoard =
    List.range 0 7
        |> List.map (List.repeat 8)
        |> List.map (List.indexedMap Tuple.pair)
        |> List.concat


blacks : List Piece
blacks =
    [ Pawn Black, Knight Black, Rook Black, Bishop Black, Queen Black, King Black ]


whites : List Piece
whites =
    [ Pawn White, Knight White, Rook White, Bishop White, Queen White, King White ]


type alias Row =
    Array Piece


type alias Board =
    Array Row


type alias Cell =
    ( Int, Int )


type alias Move =
    { player : Color
    , start : Cell
    , end : Cell
    , piece : Piece
    , killedPiece : Piece
    }


type AvailableCells
    = NoAvailableCells
    | AvailCells (List Cell)


type CurrentCell
    = NoCell
    | CurCell Cell


type HistModel
    = HistModel Model


type Mode
    = Normal
    | History Int


type alias Model =
    { board : Board
    , currentTurn : Color
    , currentCell : CurrentCell
    , lostPieces : Array Piece
    , availableCells : AvailableCells
    , moves : Array Move
    , models : Array HistModel
    , mode : Mode
    , message : String
    , visiblePiecePicker : Bool
    , cellToChangePawn : Cell
    }


type Msg
    = Touch CurrentCell
    | MakeMove Cell
    | GoToTurn Int
    | PickPiece Piece


initialModel : Model
initialModel =
    { board =
        Array.fromList
            [ Array.fromList [ Rook White, Knight White, Bishop White, Queen White, King White, Bishop White, Knight White, Rook White ]
            , Array.repeat 8 (Pawn White)
            , Array.repeat 8 Empty
            , Array.repeat 8 Empty
            , Array.repeat 8 Empty
            , Array.repeat 8 Empty
            , Array.repeat 8 (Pawn Black)
            , Array.fromList [ Rook Black, Knight Black, Bishop Black, Queen Black, King Black, Bishop Black, Knight Black, Rook Black ]
            ]
    , currentTurn = White
    , currentCell = NoCell
    , lostPieces = Array.empty
    , availableCells = NoAvailableCells
    , moves = Array.empty
    , models = Array.empty
    , mode = Normal
    , message = ""
    , visiblePiecePicker = False
    , cellToChangePawn = ( 0, 0 )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "first-container" ] [ span [] [ text "CHESS" ] ]
        , div [ class "second-container" ]
            [ viewHistory model
            , viewBoard model
            , viewDashboard model
            ]
        , viewMessage model.message
        , viewModal (changeTurn model.currentTurn) model.visiblePiecePicker
        ]


viewHistory : Model -> Html Msg
viewHistory model =
    div [ class "history" ]
        [ table []
            [ thead []
                [ tr []
                    [ th [ Attr.colspan 3, onClick (GoToTurn -1) ] [ text "Moves" ]
                    ]
                , tbody
                    []
                    (List.indexedMap viewMove (Array.toList model.moves))
                ]
            ]
        ]


viewMove : Int -> Move -> Html Msg
viewMove i move =
    tr [ onClick (GoToTurn i) ]
        [ td [] [ div [] [ text (String.fromInt (i + 1)) ] ]
        , td [] [ div [] [ text (textPiece move.piece) ] ]
        , td [] [ div [] [ text (textMoveCell move.start ++ " - " ++ textMoveCell move.end) ] ]
        ]


textMoveCell : Cell -> String
textMoveCell ( row, col ) =
    let
        letters =
            Array.fromList [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H' ]

        letter =
            case Array.get row letters of
                Nothing ->
                    '?'

                Just l ->
                    l
    in
    String.fromChar letter ++ String.fromInt col


textPiece : Piece -> String
textPiece piece =
    case piece of
        Pawn _ ->
            "Pawn"

        Rook _ ->
            "Rook"

        Knight _ ->
            "Knight"

        Bishop _ ->
            "Bishop"

        King _ ->
            "King"

        Queen _ ->
            "Queen"

        Empty ->
            "????"


viewDashboard : Model -> Html Msg
viewDashboard model =
    div [ class "dashboard" ]
        [ div [ classList [ ( "player2", True ), ( "current-player", model.currentTurn == Black ) ] ] [ span [] [ text "Black" ] ]
        , div []
            [ div [ class "killedW" ] (viewLostPieces White model.lostPieces)
            , div [ class "killedB" ] (viewLostPieces Black model.lostPieces)
            ]
        , div [ classList [ ( "player1", True ), ( "current-player", model.currentTurn == White ) ] ] [ span [] [ text "White" ] ]
        ]


viewLostPieces : Color -> Array Piece -> List (Html Msg)
viewLostPieces color arr =
    case color of
        Black ->
            Array.filter (\p -> List.member p blacks) arr
                |> Array.toList
                |> List.map viewPiece

        White ->
            Array.filter (\p -> List.member p whites) arr
                |> Array.toList
                |> List.map viewPiece


viewMessage : String -> Html Msg
viewMessage message =
    div [ class "message" ]
        [ span [] [ text message ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    div [ class "board" ]
        (model.board
            |> Array.indexedMap (viewRow model)
            |> Array.foldl List.append []
        )


viewRow : Model -> Int -> Row -> List (Html Msg)
viewRow model rowN row =
    row
        |> Array.indexedMap (viewCell model rowN)
        |> Array.toList


viewCell : Model -> Int -> Int -> Piece -> Html Msg
viewCell model rowN colN piece =
    div [ classList [ ( "cell", True ), ( "black", modBy 2 (rowN + colN) == 0 ), ( "selected", checkSelected model.currentCell rowN colN ), ( "attacted", checkAttacted model rowN colN ) ] ]
        [ if checkAvailable model.availableCells rowN colN then
            -- cell available and no piece branch
            if getPiece model.board ( rowN, colN ) == Empty then
                div [ class "empty", onClick (MakeMove ( rowN, colN )) ]
                    [ div [ class "available" ] []
                    ]

            else
                -- cell available and piece is here branch
                div [ onClick (MakeMove ( rowN, colN )) ] [ viewPieceWithAction model.currentTurn rowN colN piece ]

          else
            -- cell unavilable branch
            div [] [ viewPieceWithAction model.currentTurn rowN colN piece ]
        ]


viewModal : Color -> Bool -> Html Msg
viewModal color visible =
    div [ classList [ ( "active", visible ) ], class "modal" ]
        [ div [ class "modal-content" ]
            [ div [ class "cell", onClick (PickPiece (Queen color)) ] [ viewPiece (Queen color) ]
            , div [ class "cell", onClick (PickPiece (Bishop color)) ] [ viewPiece (Bishop color) ]
            , div [ class "cell", onClick (PickPiece (Knight color)) ] [ viewPiece (Knight color) ]
            , div [ class "cell", onClick (PickPiece (Rook color)) ] [ viewPiece (Rook color) ]
            ]
        ]


checkAttacted : Model -> Int -> Int -> Bool
checkAttacted model row col =
    getPiece model.board ( row, col ) /= Empty && checkAvailable model.availableCells row col


checkAvailable : AvailableCells -> Int -> Int -> Bool
checkAvailable availableCells rowN colN =
    case availableCells of
        NoAvailableCells ->
            False

        AvailCells list ->
            List.member ( rowN, colN ) list


checkSelected : CurrentCell -> Int -> Int -> Bool
checkSelected currentCell rowN colN =
    case currentCell of
        CurCell ( row, col ) ->
            if row == rowN && col == colN then
                True

            else
                False

        NoCell ->
            False


viewPieceWithAction : Color -> Int -> Int -> Piece -> Html Msg
viewPieceWithAction turn rowN colN piece =
    if List.member piece blacks then
        div [ clickOnRightTurn turn Black ( rowN, colN ) ] [ viewPiece piece ]

    else if List.member piece whites then
        div [ clickOnRightTurn turn White ( rowN, colN ) ] [ viewPiece piece ]

    else
        div [ class "empty", onClick (Touch NoCell) ] []


viewPiece : Piece -> Html Msg
viewPiece piece =
    case piece of
        Pawn Black ->
            img [ src "assets/black-pawn.png" ] []

        Pawn White ->
            img [ src "assets/white-pawn.png" ] []

        Knight Black ->
            img [ src "assets/black-knight.png" ] []

        Knight White ->
            img [ src "assets/white-knight.png" ] []

        Rook Black ->
            img [ src "assets/black-rook.png" ] []

        Rook White ->
            img [ src "assets/white-rook.png" ] []

        Bishop Black ->
            img [ src "assets/black-bishop.png" ] []

        Bishop White ->
            img [ src "assets/white-bishop.png" ] []

        Queen Black ->
            img [ src "assets/black-queen.png" ] []

        Queen White ->
            img [ src "assets/white-queen.png" ] []

        King Black ->
            img [ src "assets/black-king.png" ] []

        King White ->
            img [ src "assets/white-king.png" ] []

        Empty ->
            div [ class "empty" ] []


clickOnRightTurn : Color -> Color -> Cell -> Attribute Msg
clickOnRightTurn turnColor pieceColor cell =
    if turnColor == pieceColor then
        onClick (Touch (CurCell cell))

    else
        attribute "noClick" "nothing"


type EnPassant
    = Left
    | Right
    | No


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Touch (CurCell ( row, col )) ->
            ( { model | currentCell = CurCell ( row, col ), availableCells = getAvailableCells model ( row, col ) }, Cmd.none )

        Touch NoCell ->
            ( { model | currentCell = NoCell, availableCells = NoAvailableCells }, Cmd.none )

        GoToTurn i ->
            let
                newModel =
                    case Array.get (i + 1) model.models of
                        Nothing ->
                            model

                        Just (HistModel hstModel) ->
                            let
                                newModels =
                                    case model.mode of
                                        Normal ->
                                            Array.push (HistModel { model | availableCells = NoAvailableCells, currentCell = NoCell }) model.models

                                        _ ->
                                            model.models
                            in
                            { hstModel | mode = History (i + 1), models = newModels }
            in
            ( { newModel | moves = model.moves }, Cmd.none )

        PickPiece piece ->
            let
                rowN =
                    Tuple.first model.cellToChangePawn

                colN =
                    Tuple.second model.cellToChangePawn

                row =
                    case Array.get rowN model.board of
                        Nothing ->
                            Array.empty

                        Just row_ ->
                            row_

                board =
                    Array.set rowN (Array.set colN piece row) model.board
            in
            ( { model | board = board, visiblePiecePicker = False }, Cmd.none )

        MakeMove cell ->
            let
                refreshedModel =
                    case model.mode of
                        History i ->
                            { model | models = Array.slice 0 i model.models, moves = Array.slice 0 i model.moves }

                        Normal ->
                            model

                castling =
                    case model.currentCell of
                        NoCell ->
                            False

                        CurCell curCell ->
                            case getPiece model.board curCell of
                                King _ ->
                                    if abs (Tuple.second cell - Tuple.second curCell) == 2 then
                                        True

                                    else
                                        False

                                _ ->
                                    False
            in
            case model.currentCell of
                NoCell ->
                    ( model, Cmd.none )

                CurCell ( startRow, startCol ) ->
                    let
                        ( endRow, endCol ) =
                            cell

                        pieceMoved =
                            getPiece refreshedModel.board ( startRow, startCol )

                        enPassant : EnPassant
                        enPassant =
                            case pieceMoved of
                                Pawn _ ->
                                    if getPiece refreshedModel.board ( endRow, endCol ) == Empty && (endCol - startCol) == 1 then
                                        Right

                                    else if getPiece refreshedModel.board ( endRow, endCol ) == Empty && (endCol - startCol) == -1 then
                                        Left

                                    else
                                        No

                                _ ->
                                    No

                        board1 =
                            case Array.get startRow refreshedModel.board of
                                Nothing ->
                                    refreshedModel.board

                                Just row ->
                                    let
                                        castleRow =
                                            if castling && (endCol > startCol) then
                                                Array.set 7 Empty row

                                            else if castling && (endCol < startCol) then
                                                Array.set 0 Empty row

                                            else
                                                row

                                        refreshedRow =
                                            case pieceMoved of
                                                Pawn _ ->
                                                    if enPassant == Right then
                                                        Array.set (startCol + 1) Empty castleRow

                                                    else if enPassant == Left then
                                                        Array.set (startCol - 1) Empty castleRow

                                                    else
                                                        castleRow

                                                _ ->
                                                    castleRow
                                    in
                                    Array.set startRow (Array.set startCol Empty refreshedRow) refreshedModel.board

                        board2 =
                            case Array.get endRow board1 of
                                Nothing ->
                                    refreshedModel.board

                                Just row ->
                                    let
                                        refreshedRow =
                                            if castling && (endCol > startCol) then
                                                Array.set 5 (Rook model.currentTurn) row

                                            else if castling && (endCol < startCol) then
                                                Array.set 3 (Rook model.currentTurn) row

                                            else
                                                row
                                    in
                                    Array.set endRow (Array.set endCol pieceMoved refreshedRow) board1

                        lostPiece =
                            case enPassant of
                                No ->
                                    case Array.get endRow board1 of
                                        Nothing ->
                                            Empty

                                        Just row ->
                                            case Array.get endCol row of
                                                Nothing ->
                                                    Empty

                                                Just piece ->
                                                    piece

                                Left ->
                                    case Array.get startRow refreshedModel.board of
                                        Nothing ->
                                            Empty

                                        Just row ->
                                            case Array.get (startCol - 1) row of
                                                Nothing ->
                                                    Empty

                                                Just piece ->
                                                    piece

                                Right ->
                                    case Array.get startRow refreshedModel.board of
                                        Nothing ->
                                            Empty

                                        Just row ->
                                            case Array.get (startCol + 1) row of
                                                Nothing ->
                                                    Empty

                                                Just piece ->
                                                    piece

                        newModel_ =
                            { refreshedModel
                                | board = board2
                                , currentCell = NoCell
                                , availableCells = NoAvailableCells
                                , currentTurn = changeTurn refreshedModel.currentTurn
                                , lostPieces =
                                    if lostPiece == Empty then
                                        refreshedModel.lostPieces

                                    else
                                        Array.push lostPiece refreshedModel.lostPieces
                                , moves = Array.push { player = refreshedModel.currentTurn, start = ( startRow, startCol ), end = cell, piece = pieceMoved, killedPiece = lostPiece } refreshedModel.moves
                                , models = Array.push (HistModel { refreshedModel | availableCells = NoAvailableCells, currentCell = NoCell, moves = Array.empty, models = Array.empty }) refreshedModel.models
                                , mode = Normal
                                , message = ""
                            }

                        newModel =
                            if (pieceMoved == Pawn Black && endRow == 0) || (pieceMoved == Pawn White && endRow == 7) then
                                { newModel_ | visiblePiecePicker = True, cellToChangePawn = ( endRow, endCol ) }

                            else
                                newModel_
                    in
                    case getPieceCell newModel.board (King model.currentTurn) of
                        Nothing ->
                            ( { model | message = " Something broken!!!!" }, Cmd.none )

                        Just cell_ ->
                            if Debug.log "isAttacted" (willBeAttacked newModel (Debug.log "cell " cell_)) then
                                ( { model | message = " Protect your King!!!" }, Cmd.none )

                            else
                                case getPieceCell model.board (King (changeTurn model.currentTurn)) of
                                    Nothing ->
                                        ( { model | message = "Something broken!!!!" }, Cmd.none )

                                    Just cell__ ->
                                        if isAttacked newModel cell__ then
                                            ( { newModel | message = " CHECK!!!" }, Cmd.none )

                                        else
                                            ( newModel, Cmd.none )


getPieceCell : Board -> Piece -> Maybe Cell
getPieceCell board piece =
    List.filter (checkPiece board piece) flatBoard
        |> List.head


checkPiece board piece cell =
    getPiece board cell == piece


isNotMoved : Model -> Cell -> Bool
isNotMoved model cell =
    let
        moves =
            case model.mode of
                History i ->
                    Array.slice 0 i model.moves

                Normal ->
                    model.moves
    in
    Array.filter (\a -> a.start == cell) moves
        |> Array.isEmpty


willBeAttacked : Model -> Cell -> Bool
willBeAttacked model cell =
    flatBoard
        |> List.filter (\a -> not (isEmpty model.board a))
        |> List.filter (\a -> getColor model.board a == model.currentTurn)
        |> List.filter (\a -> List.member cell (getListOfAvailableCells model a))
        |> List.isEmpty
        |> not


isAttacked : Model -> Cell -> Bool
isAttacked model cell =
    flatBoard
        |> List.filter (\a -> not (isEmpty model.board a))
        |> List.filter (\a -> not (getColor model.board a == model.currentTurn))
        |> List.filter (\a -> List.member cell (getListOfAvailableCells model a))
        |> List.isEmpty
        |> not


changeTurn : Color -> Color
changeTurn color =
    case color of
        White ->
            Black

        Black ->
            White


getListOfAvailableCells : Model -> Cell -> List Cell
getListOfAvailableCells model cell =
    case getAvailableCells model cell of
        NoAvailableCells ->
            []

        AvailCells lst ->
            lst


getAvailableCells : Model -> Cell -> AvailableCells
getAvailableCells model cell =
    case getPiece model.board cell of
        Pawn color ->
            case color of
                White ->
                    if Tuple.first cell == 1 then
                        Array.fromList [ Tuple.mapFirst ((+) 1) cell, Tuple.mapFirst ((+) 2) cell ]
                            |> Array.filter (isReachable model.board cell)
                            |> Array.filter (isEmpty model.board)
                            |> Array.append (Array.filter (isEnemy model.board cell) (Array.fromList [ ( Tuple.first cell + 1, Tuple.second cell - 1 ), ( Tuple.first cell + 1, Tuple.second cell + 1 ) ]))
                            |> Array.toList
                            |> AvailCells

                    else
                        Array.fromList [ Tuple.mapFirst ((+) 1) cell ]
                            |> Array.filter (isEmpty model.board)
                            |> Array.append (Array.filter (isEnemy model.board cell) (Array.fromList [ ( Tuple.first cell + 1, Tuple.second cell - 1 ), ( Tuple.first cell + 1, Tuple.second cell + 1 ) ]))
                            |> Array.append (getEnPassant model cell)
                            |> Array.toList
                            |> AvailCells

                Black ->
                    if Tuple.first cell == 6 then
                        Array.fromList [ Tuple.mapFirst ((+) -1) cell, Tuple.mapFirst ((+) -2) cell ]
                            |> Array.filter (isReachable model.board cell)
                            |> Array.filter (isEmpty model.board)
                            |> Array.append (Array.filter (isEnemy model.board cell) (Array.fromList [ ( Tuple.first cell - 1, Tuple.second cell - 1 ), ( Tuple.first cell - 1, Tuple.second cell + 1 ) ]))
                            |> Array.toList
                            |> AvailCells

                    else
                        Array.fromList [ Tuple.mapFirst ((+) -1) cell ]
                            |> Array.filter (isEmpty model.board)
                            |> Array.append (Array.filter (isEnemy model.board cell) (Array.fromList [ ( Tuple.first cell - 1, Tuple.second cell - 1 ), ( Tuple.first cell - 1, Tuple.second cell + 1 ) ]))
                            |> Array.append (getEnPassant model cell)
                            |> Array.toList
                            |> AvailCells

        Rook _ ->
            Array.initialize 8 (\n -> ( Tuple.first cell, n ))
                |> Array.append (Array.initialize 8 (\n -> ( n, Tuple.second cell )))
                |> Array.filter (isReachable model.board cell)
                |> Array.toList
                |> AvailCells

        Bishop _ ->
            Array.initialize 8 (\n -> ( Tuple.first cell + n, Tuple.second cell + n ))
                |> Array.append (Array.initialize 8 (\n -> ( Tuple.first cell - n, Tuple.second cell - n )))
                |> Array.append (Array.initialize 8 (\n -> ( Tuple.first cell + n, Tuple.second cell - n )))
                |> Array.append (Array.initialize 8 (\n -> ( Tuple.first cell - n, Tuple.second cell + n )))
                |> Array.filter (\( a, b ) -> a <= 7 && a >= 0 && b <= 7 && b >= 0)
                |> Array.toList
                |> Set.fromList
                |> Set.toList
                |> Array.fromList
                |> Array.filter (isReachable model.board cell)
                |> Array.toList
                |> AvailCells

        Queen _ ->
            Array.initialize 8 (\n -> ( Tuple.first cell + n, Tuple.second cell + n ))
                |> Array.append (Array.initialize 8 (\n -> ( Tuple.first cell - n, Tuple.second cell - n )))
                |> Array.append (Array.initialize 8 (\n -> ( Tuple.first cell + n, Tuple.second cell - n )))
                |> Array.append (Array.initialize 8 (\n -> ( Tuple.first cell - n, Tuple.second cell + n )))
                |> Array.append (Array.initialize 8 (\n -> ( Tuple.first cell, n )))
                |> Array.append (Array.initialize 8 (\n -> ( n, Tuple.second cell )))
                |> Array.filter (\( a, b ) -> a <= 7 && a >= 0 && b <= 7 && b >= 0)
                |> Array.toList
                |> Set.fromList
                |> Set.toList
                |> Array.fromList
                |> Array.filter (isReachable model.board cell)
                |> Array.toList
                |> AvailCells

        King col ->
            Array.initialize 2 (\n -> ( Tuple.first cell + n, Tuple.second cell + n ))
                |> Array.append (Array.initialize 2 (\n -> ( Tuple.first cell - n, Tuple.second cell - n )))
                |> Array.append (Array.initialize 2 (\n -> ( Tuple.first cell + n, Tuple.second cell - n )))
                |> Array.append (Array.initialize 2 (\n -> ( Tuple.first cell - n, Tuple.second cell + n )))
                |> Array.append (Array.initialize 2 (\n -> ( Tuple.first cell + n, Tuple.second cell )))
                |> Array.append (Array.initialize 2 (\n -> ( Tuple.first cell - n, Tuple.second cell )))
                |> Array.append (Array.initialize 2 (\n -> ( Tuple.first cell, Tuple.second cell + n )))
                |> Array.append (Array.initialize 2 (\n -> ( Tuple.first cell, Tuple.second cell - n )))
                |> Array.append
                    (if col == model.currentTurn then
                        getCastlingCells model cell

                     else
                        Array.empty
                    )
                -- for breaking infinfite loop
                |> Array.filter (\( a, b ) -> a <= 7 && a >= 0 && b <= 7 && b >= 0)
                |> Array.toList
                |> Set.fromList
                |> Set.toList
                |> Array.fromList
                |> Array.filter (isReachable model.board cell)
                |> Array.toList
                |> AvailCells

        Knight _ ->
            Array.fromList [ ( Tuple.first cell + 2, Tuple.second cell + 1 ) ]
                |> Array.append (Array.fromList [ ( Tuple.first cell + 2, Tuple.second cell - 1 ) ])
                |> Array.append (Array.fromList [ ( Tuple.first cell - 2, Tuple.second cell - 1 ) ])
                |> Array.append (Array.fromList [ ( Tuple.first cell - 2, Tuple.second cell + 1 ) ])
                |> Array.append (Array.fromList [ ( Tuple.first cell - 1, Tuple.second cell + 2 ) ])
                |> Array.append (Array.fromList [ ( Tuple.first cell + 1, Tuple.second cell + 2 ) ])
                |> Array.append (Array.fromList [ ( Tuple.first cell + 1, Tuple.second cell - 2 ) ])
                |> Array.append (Array.fromList [ ( Tuple.first cell - 1, Tuple.second cell - 2 ) ])
                |> Array.filter (\( a, b ) -> a <= 7 && a >= 0 && b <= 7 && b >= 0)
                |> Array.filter (isEmptyOrEnemy model.board cell)
                |> Array.toList
                |> AvailCells

        _ ->
            NoAvailableCells


getEnPassant : Model -> Cell -> Array Cell
getEnPassant model cell =
    case getPiece model.board cell of
        Pawn Black ->
            if Tuple.first cell == 3 && isLastMoveEnPassantPawn model cell == Right then
                Array.fromList [ ( Tuple.first cell - 1, Tuple.second cell + 1 ) ]

            else if Tuple.first cell == 3 && isLastMoveEnPassantPawn model cell == Left then
                Array.fromList [ ( Tuple.first cell - 1, Tuple.second cell - 1 ) ]

            else
                Array.empty

        Pawn White ->
            if Tuple.first cell == 4 && isLastMoveEnPassantPawn model cell == Right then
                Array.fromList [ ( Tuple.first cell + 1, Tuple.second cell + 1 ) ]

            else if Tuple.first cell == 4 && isLastMoveEnPassantPawn model cell == Left then
                Array.fromList [ ( Tuple.first cell + 1, Tuple.second cell - 1 ) ]

            else
                Array.empty

        _ ->
            Array.empty


isLastMoveEnPassantPawn : Model -> Cell -> EnPassant
isLastMoveEnPassantPawn model cell =
    case Array.get (Array.length model.moves - 1) model.moves of
        Nothing ->
            No

        Just move ->
            case move.piece of
                Pawn _ ->
                    if Tuple.first move.end /= 3 && Tuple.first move.end /= 4 then
                        No

                    else if Tuple.second move.end == Tuple.second cell + 1 then
                        Right

                    else if Tuple.second move.end == Tuple.second cell - 1 then
                        Left

                    else
                        No

                _ ->
                    No


getCastlingCells : Model -> Cell -> Array Cell
getCastlingCells model cell =
    case getPiece model.board cell of
        King _ ->
            if
                isNotMoved model cell
                    && isNotMoved model ( Tuple.first cell, 0 )
                    && isReachable model.board cell ( Tuple.first cell, 1 )
                    && Array.isEmpty (Array.filter (isAttacked model) (Array.initialize 3 (\n -> ( Tuple.first cell, Tuple.second cell - n ))))
            then
                Array.fromList [ ( Tuple.first cell, Tuple.second cell - 2 ) ]

            else if
                isNotMoved model cell
                    && isNotMoved model ( Tuple.first cell, 7 )
                    && isReachable model.board cell ( Tuple.first cell, 6 )
                    && Array.isEmpty (Array.filter (isAttacked model) (Array.initialize 3 (\n -> ( Tuple.first cell, Tuple.second cell + n ))))
            then
                Array.fromList [ ( Tuple.first cell, Tuple.second cell + 2 ) ]

            else
                Array.empty

        _ ->
            Array.empty


isEmptyOrEnemy : Board -> Cell -> Cell -> Bool
isEmptyOrEnemy board startCell endCell =
    case getPiece board endCell of
        Empty ->
            True

        _ ->
            if sameColor board startCell endCell then
                False

            else
                True


isEnemy : Board -> Cell -> Cell -> Bool
isEnemy board startCell endCell =
    case getPiece board endCell of
        Empty ->
            False

        _ ->
            if sameColor board startCell endCell then
                False

            else
                True


isEmpty : Board -> Cell -> Bool
isEmpty board cell =
    case getPiece board cell of
        Empty ->
            True

        _ ->
            False


isReachable : Board -> Cell -> Cell -> Bool
isReachable board (( startRow, startCol ) as startCell) (( endRow, endCol ) as endCell) =
    if startRow == endRow && startCol == endCol then
        -- same cell
        False

    else if getPiece board endCell /= Empty && sameColor board startCell endCell then
        -- has Piece with same color
        False

    else if abs (startRow - endRow) == 1 || abs (startCol - endCol) == 1 then
        -- cell next to me
        True

    else if startRow == endRow then
        -- horizontal
        let
            rowArray =
                case Array.get startRow board of
                    Nothing ->
                        Array.empty

                    Just row ->
                        row
        in
        if startCol > endCol then
            emptyPieces (Array.slice (endCol + 1) startCol rowArray)

        else
            emptyPieces (Array.slice (startCol + 1) endCol rowArray)

    else if startCol == endCol then
        -- vertical
        if startRow > endRow then
            Array.slice (endRow + 1) startRow board
                |> Array.map (Array.get startCol)
                |> Array.map (Maybe.withDefault Empty)
                |> emptyPieces

        else
            Array.slice (startRow + 1) endRow board
                |> Array.map (Array.get startCol)
                |> Array.map (Maybe.withDefault Empty)
                |> emptyPieces

    else if abs (startRow - endRow) == abs (startCol - endCol) then
        -- diagonal
        if startRow > endRow && startCol > endCol then
            Array.slice (endRow + 1) startRow board
                |> Array.indexedMap (\i col -> Array.get (endCol + i + 1) col)
                |> Array.map (Maybe.withDefault Empty)
                |> emptyPieces

        else if startRow > endRow && startCol < endCol then
            Array.slice (endRow + 1) startRow board
                |> Array.indexedMap (\i col -> Array.get (endCol - (i + 1)) col)
                |> Array.map (Maybe.withDefault Empty)
                |> emptyPieces

        else if startRow < endRow && startCol > endCol then
            Array.slice (startRow + 1) endRow board
                |> Array.indexedMap (\i col -> Array.get (startCol - (i + 1)) col)
                |> Array.map (Maybe.withDefault Empty)
                |> emptyPieces

        else
            -- startRow < endRow && startCol < endCol
            Array.slice (startRow + 1) endRow board
                |> Array.indexedMap (\i col -> Array.get (startCol + (i + 1)) col)
                |> Array.map (Maybe.withDefault Empty)
                |> emptyPieces

    else
        False


sameColor : Board -> Cell -> Cell -> Bool
sameColor board startCell endCell =
    if List.member (getPiece board startCell) whites == List.member (getPiece board endCell) whites then
        True

    else
        False


emptyPieces : Array Piece -> Bool
emptyPieces arr =
    arr
        |> Array.filter ((/=) Empty)
        |> Array.isEmpty


getPiece : Board -> Cell -> Piece
getPiece board cell =
    case Array.get (Tuple.first cell) board of
        Just rowArray ->
            case Array.get (Tuple.second cell) rowArray of
                Just piece ->
                    piece

                Nothing ->
                    Empty

        Nothing ->
            Empty


getColor : Board -> Cell -> Color
getColor board cell =
    if List.member (getPiece board cell) whites then
        White

    else
        Black


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
