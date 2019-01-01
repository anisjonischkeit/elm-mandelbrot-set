module Mandelbrot exposing (getPoints)

{- 
From: https://hardmath123.github.io/scratch-mandelbrot.html
-}


type alias Point = 
    { on: Bool 
    }




testPoint iteration zR zX x y =
    let 
        cR = x/120
        cX = y/120

        newZR = zR^2 + -1 * zX^2 + cR
        newZX = 2*zR*zX + cX
    in
        if iteration > 50
        then False
        else
            if zR > 2
            then True
            else 
                testPoint (iteration + 1) newZR newZX x y





isPointInSet : Float -> Float -> Bool
isPointInSet = testPoint 0 0 0
    



createRow : Float -> Float -> Float -> Int -> List Point
createRow x y interval itemsLeft =
    if itemsLeft <= 0 
    then []
    else 
        if isPointInSet x y
        then { on = True } :: createRow (x + interval) y interval (itemsLeft - 1)
        else { on = False } :: createRow (x + interval) y interval (itemsLeft - 1)




getPoints : Int -> Int -> Float -> Float -> Float -> Float -> List (List Point)
getPoints width height startX startY endX endY
    = createRow startX 60 1 width
        |> List.repeat height