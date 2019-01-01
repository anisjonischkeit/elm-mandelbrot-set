import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mandelbrot exposing (getPoints)

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = Int

init : Model
init =
  0


-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-- VIEW

getPointsHtml : Int -> Int -> List (Html Msg)
getPointsHtml w h  = 
    let 
        halfHeight = (toFloat h) / 2
        halfWidth = (toFloat w) / 2
    in
        getPoints w h -halfWidth halfHeight halfWidth -halfHeight
        |> List.indexedMap 
            (\rowNo row -> 
                row |> List.indexedMap (\colNo point ->
                    rect 
                        [ x (String.fromInt colNo)
                        , y (String.fromInt rowNo)
                        , width "1"
                        , height "1"
                        , fill <| if point.on then "green" else "none"
                        ]
                        []
                )
            )
        |> List.concat

view : Model -> Html Msg
view model =
    let 
        canvasWidth = 480
        canvasHeight = 360
        stringCanvasWidth = (String.fromInt canvasWidth)
        stringCanvasHeight = (String.fromInt canvasHeight)
    in
        svg
            [ width stringCanvasWidth
            , height stringCanvasHeight
            , viewBox ("0 0 " ++ stringCanvasWidth ++ stringCanvasHeight)
            ]
            (getPointsHtml canvasWidth canvasHeight)
