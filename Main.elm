module Main exposing (main)

import Browser
import Browser.Events exposing (..)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Text exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as Decode


type alias Model =
    { lapsedTime : Float
    , ballX : Float
    , ballY : Float
    , phase : Phase
    , scaffolds : List Scaffold
    , leftPressed : Bool
    , rightPressed : Bool
    , score : Int
    , offSet : Float
    , deltaOffSet : Float
    , lowerBase : Float
    }


type alias Scaffold =
    { leftTip : Canvas.Point
    , length : Float
    }


type Phase
    = Name
    | Top
    | Ranking
    | Game


type Direction
    = Right
    | Left
    | Other


type Msg
    = Frame Float
    | Collided Float
    | Test
    | KeyDown Direction
    | KeyUp Direction


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lapsedTime = -50
      , ballX = width / 2
      , ballY = height - radius
      , phase = Game
      , scaffolds =
            [ Scaffold ( 10, 50 ) 50
            , Scaffold ( 80, 100 ) 50
            , Scaffold ( 100, 540 ) 50
            , Scaffold ( 150, 500 ) 50
            , Scaffold ( 200, 440 ) 50
            , Scaffold ( 120, 380 ) 50
            , Scaffold ( 80, 340 ) 50
            , Scaffold ( 50, 280 ) 50
            , Scaffold ( 120, 200 ) 50
            , Scaffold ( 180, 150 ) 50
            , Scaffold ( 0, height ) width
            ]
      , leftPressed = False
      , rightPressed = False
      , score = height
      , offSet = -30
      , deltaOffSet = 0
      , lowerBase = height
      }
    , Cmd.none
    )


width =
    400


height =
    640


radius =
    20


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            let
                isC : Scaffold -> Bool
                isC scaffold =
                    Tuple.second scaffold.leftTip
                        - (model.ballY + radius)
                        > 0
                        && Tuple.second scaffold.leftTip
                        - (model.ballY + radius)
                        < 10
                        && Tuple.first scaffold.leftTip
                        <= model.ballX
                        && scaffold.length
                        + Tuple.first scaffold.leftTip
                        >= model.ballX
                        && model.lapsedTime
                        >= 0

                isColliding : List Scaffold
                isColliding =
                    List.filter isC model.scaffolds

                getScaffoldY : List Scaffold -> Float
                getScaffoldY scaffolds =
                    case scaffolds of
                        [] ->
                            height

                        x :: _ ->
                            Tuple.second x.leftTip

                newModel : Model
                newModel =
                    { model
                        | ballY = model.ballY + 0.1 * model.lapsedTime
                        , ballX =
                            if model.leftPressed == model.rightPressed then
                                model.ballX

                            else if model.leftPressed then
                                model.ballX - 2

                            else
                                model.ballX + 2
                        , score =
                            if model.ballY < toFloat model.score then
                                floor model.ballY

                            else
                                model.score
                        , offSet =
                            if model.deltaOffSet > 0 then
                                model.offSet + 1

                            else
                                model.offSet
                        , deltaOffSet =
                            if model.deltaOffSet > 0 then
                                model.deltaOffSet - 1

                            else
                                0
                        , lapsedTime =
                            model.lapsedTime + 1
                    }
            in
            if not <| List.isEmpty isColliding then
                update (Collided (Debug.log "hoge" <| getScaffoldY isColliding)) newModel

            else
                ( newModel, Cmd.none )

        Collided y ->
            ( { model
                | lowerBase =
                    if model.lowerBase - y > 5 then
                        y

                    else
                        Debug.log "LB" model.lowerBase
                , deltaOffSet =
                    if model.lowerBase - y > 5 then
                        model.lowerBase - y

                    else
                        model.deltaOffSet
                , lapsedTime =
                    -50
              }
            , Cmd.none
            )

        KeyDown Left ->
            ( { model
                | leftPressed = True
              }
            , Cmd.none
            )

        KeyDown Right ->
            ( { model
                | rightPressed = True
              }
            , Cmd.none
            )

        KeyDown Other ->
            ( model
            , Cmd.none
            )

        KeyUp Left ->
            ( { model
                | leftPressed = False
              }
            , Cmd.none
            )

        KeyUp Right ->
            ( { model
                | rightPressed = False
              }
            , Cmd.none
            )

        KeyUp Other ->
            ( model
            , Cmd.none
            )

        Test ->
            ( { model
                | ballY = 100
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map KeyDown keyDecoder)
        , onKeyUp (Decode.map KeyUp keyDecoder)
        , onAnimationFrameDelta Frame
        ]


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        _ ->
            Other


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "5px solid rgba(0,0,0,0.1)"
            , Html.Events.onClick Test
            ]
            ([ clearScreen ]
                ++ render model
            )
        ]


clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render : Model -> List Renderable
render model =
    case model.phase of
        Top ->
            renderTop model

        Ranking ->
            renderRanking model

        Game ->
            renderGame model

        Name ->
            renderName model


renderTop : Model -> List Renderable
renderTop model =
    [ text
        [ font { size = 48, family = "sans-serif" }
        ]
        ( 50, 50 )
        "Jumping"

    --renderButton
    ]



--renderButton :


renderName : Model -> List Renderable
renderName model =
    [ text
        [ font { size = 48, family = "sans-serif" }
        ]
        ( 50, 50 )
        "Get Started"

    --renderButton
    ]


renderRanking : Model -> List Renderable
renderRanking model =
    [ text
        [ font { size = 48, family = "sans-serif" }
        ]
        ( 50, 50 )
        "Get Started"

    --renderButton
    ]


renderGame : Model -> List Renderable
renderGame model =
    [ shapes
        []
        [ circle ( model.ballX, model.ballY + model.offSet ) radius ]
    ]
        ++ renderScaffolds model
        ++ [ text
                [ font { size = 24, family = "sans-serif" }
                ]
                ( 250, 20 )
                ("Score:" ++ String.fromInt (height - model.score))
           ]


renderScaffolds : Model -> List Renderable
renderScaffolds model =
    let
        renderS : Scaffold -> Renderable
        renderS scaffold =
            shapes
                []
                [ rect ( Tuple.first scaffold.leftTip, model.offSet + Tuple.second scaffold.leftTip ) scaffold.length 5 ]
    in
    List.map renderS model.scaffolds
