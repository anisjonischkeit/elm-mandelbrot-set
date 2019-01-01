module Mandelbrot exposing (getPoints, MandelbrotPixel(..))

{- 
From: https://hardmath123.github.io/scratch-mandelbrot.html
-}

type MandelbrotPixel
    = InSet
    | NotInSet Int

type alias Point = 
    { result: MandelbrotPixel 
    }



-- TODO: Memoising this function will change the complexity of 
-- this function to O(1) when rendering the second half 
-- of the set and probably for whenever we go through the
-- MUV loop
testPoint : Int -> Float -> Float -> Int -> Float -> Float -> Float -> MandelbrotPixel
testPoint iteration zR zX maxIterations magnification x y =
    let 
        cR = x/magnification
        cX = y/magnification

        newZR = zR^2 - 1 * zX^2 + cR
        newZX = 2 * zR * zX + cX
    in
        if iteration > maxIterations
        then InSet
        else
            if zR > 2
            then NotInSet iteration
            else 
                testPoint (iteration + 1) newZR newZX maxIterations magnification x y





getPixelValue : Int -> Float -> Float -> Float -> MandelbrotPixel
getPixelValue = testPoint 0 0 0
    



createRow : Float -> Float -> Float -> Float -> Float -> Int -> List Point
createRow x y interval lastItem magnification maxIterations =
    if x > lastItem 
    then []
    else 
        let 
            rowTail = createRow (x + interval) y interval lastItem magnification maxIterations
        in 
            { result = getPixelValue maxIterations magnification x y } :: rowTail



getPoints : Float -> Int -> Int -> Float -> Float -> Float -> Float -> Int -> List (List Point)
getPoints interval width height startX startY endX endY maxIterations = 
    if startY < endY
    then []
    else 
        let 
            firstRow = createRow startX startY interval endX (toFloat width / 4) maxIterations
            rowsTail = getPoints interval width height startX (startY - interval) endX endY maxIterations
        in
            firstRow :: rowsTail 