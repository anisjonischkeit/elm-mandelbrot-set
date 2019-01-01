import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mandelbrot exposing (getPoints, MandelbrotPixel(..))

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

-- TODO: focus point should be relative to the numbering system used by the mandelbrot set
-- rather than the pixels. This will make movements more consistant rahter than being slow
-- the beginning and really fast when zoomed
getPointsHtml : Int -> Int -> Int -> (Int, Int) -> List (Html Msg)
getPointsHtml w h zoom (focusPointX, focusPointY) = 
    let 
        halfHeight = (toFloat h) / 2
        halfWidth = (toFloat w) / 2

        startX = (toFloat focusPointX) - (halfWidth / (toFloat zoom))  -- x starts at a negative           +y/2
        endX = (toFloat focusPointX) + (halfWidth / (toFloat zoom))    --                            -x/2 ---|--- +x/2
        startY = (toFloat focusPointY) + (halfHeight / (toFloat zoom)) -- y starts at a positive           -y/2
        endY = (toFloat focusPointY) - (halfHeight / (toFloat zoom))

        interval = 1 / toFloat zoom

        maxIterations = 50
    in
        getPoints interval w h startX startY endX endY maxIterations
        |> List.indexedMap 
            (\rowNo row -> 
                row |> List.indexedMap (\colNo point ->
                    rect 
                        [ x (String.fromInt colNo)
                        , y (String.fromInt rowNo)
                        , width "1"
                        , height "1"
                        , fill <| case point.result of
                            InSet -> "rgba(0,0,0,1)"
                            NotInSet iterations ->  "rgba(0,0,0," ++ ((toFloat iterations)/maxIterations |> String.fromFloat) ++ ")"
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
            (getPointsHtml canvasWidth canvasHeight 1 (0, 0))
