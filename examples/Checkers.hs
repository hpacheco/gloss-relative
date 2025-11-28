module Main where

import Graphics.Gloss.Relative

lightWood :: Color
lightWood = makeColorI 222 184 135 255

darkWood :: Color
darkWood = makeColorI 62 49 49 255

draw :: Frame
draw = Aspect (11,11) alignCenter $ Frames [border,slots]
    where
    border = solid darkWood
    slots = Zoom (10/11) (10/11) alignCenter $ grid 10 10 drawSlot

drawSlot :: Int -> Int -> Frame
drawSlot i j = if even i == even j
    then solid white 
    else solid black 

main :: IO ()
main = do
    displayRelative FullScreen lightWood draw