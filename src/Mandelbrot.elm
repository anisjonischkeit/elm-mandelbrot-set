module Mandelbrot exposing (getPoints)

{- 
From: https://hardmath123.github.io/scratch-mandelbrot.html
-}


type alias Point = 
    { on: Bool 
    }



-- TODO: Memoising this function will change the complexity of 
-- this function to O(1) when rendering the second half 
-- of the set and probably for whenever we go through the
-- MUV loop
testPoint iteration zR zX x y =
    let 
        cR = x/120
        cX = y/120

        newZR = zR^2 - 1 * zX^2 + cR
        newZX = 2 * zR * zX + cX
    in
        if iteration > 50
        then True
        else
            if zR > 2
            then False
            else 
                testPoint (iteration + 1) newZR newZX x y





isPointInSet : Float -> Float -> Bool
isPointInSet = testPoint 0 0 0
    



createRow : Float -> Float -> Float -> Float -> List Point
createRow x y interval lastItem =
    if x > lastItem 
    then []
    else 
        let 
            rowTail = createRow (x + interval) y interval lastItem
        in 
            if isPointInSet x y
            then { on = True } :: rowTail
            else { on = False } :: rowTail



getPoints : Int -> Int -> Float -> Float -> Float -> Float -> List (List Point)
getPoints width height startX startY endX endY = 
    if startY < endY
    then []
    else 
        let 
            firstRow = (createRow startX startY 1 endX)
            rowsTail = (getPoints width height startX (startY - 1) endX endY)
        in
            firstRow :: rowsTail 