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
testPoint iteration zR zX magnification x y =
    let 
        cR = x/magnification
        cX = y/magnification

        newZR = zR^2 - 1 * zX^2 + cR
        newZX = 2 * zR * zX + cX
    in
        if iteration > 1000
        then True
        else
            if zR > 2
            then False
            else 
                testPoint (iteration + 1) newZR newZX magnification x y





isPointInSet : Float -> Float -> Float -> Bool
isPointInSet = testPoint 0 0 0
    



createRow : Float -> Float -> Float -> Float -> Float -> List Point
createRow x y interval lastItem magnification =
    if x > lastItem 
    then []
    else 
        let 
            rowTail = createRow (x + interval) y interval lastItem magnification
        in 
            if isPointInSet magnification x y
            then { on = True } :: rowTail
            else { on = False } :: rowTail



getPoints : Float -> Int -> Int -> Float -> Float -> Float -> Float -> List (List Point)
getPoints interval width height startX startY endX endY = 
    if startY < endY
    then []
    else 
        let 
            firstRow = createRow startX startY interval endX (toFloat width / 4)
            rowsTail = getPoints interval width height startX (startY - interval) endX endY
        in
            firstRow :: rowsTail 