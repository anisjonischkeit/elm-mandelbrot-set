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

getPointsHtml = 
    getPoints 480 360 -240 180 0 0
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
  svg
    [ width "480"
    , height "360"
    , viewBox "0 0 480 360"
    ]
    getPointsHtml

        
    -- [ rect
    --     [ x "10"
    --     , y "10"
    --     , width "100"
    --     , height "100"
    --     , rx "15"
    --     , ry "15"
    --     ]
    --     []
    -- , circle
    --     [ cx "50"
    --     , cy "50"
    --     , r "50"
    --     ]
    --     []
    -- ]