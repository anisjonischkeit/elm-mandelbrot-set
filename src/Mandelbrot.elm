module Mandelbrot exposing (getPoints)

{- 
From: https://hardmath123.github.io/scratch-mandelbrot.html
-}


type alias Point = 
    { on: Bool 
    }



{-


int testmal(float realx, float imgx, int k)
{
	int i;
	float re,im,re2,im2;
	
	re = realx; x
	im = imgx; y

	for(i=0;i<2-k;i++)
	{
		re2 = re*re;
		im2 = im*im;
		if ((re2+im2) > 256)
		{
			return 1;
		}
		im = 2*re*im + imgx; y
		re = re2 - im2 + realx; x
	}
	return(0);
}	

-}



testPoint iteration x y =
    let 
        x2 = x^2
        y2 = y^2
    in
        if iteration <= 0
        then False
        else
            if (x2 + y2) > 256
            then True
            else 
                let 
                    nextX = x2 - y2 + x
                    nextY = 2 * x * y + y

                in
                    testPoint (iteration - 1) nextX nextY





isPointInSet : Float -> Float -> Bool
isPointInSet = testPoint 50
    



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