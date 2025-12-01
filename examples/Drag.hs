module Main where

import Graphics.Gloss.Relative
import Data.List as List

data State = State { ballPosition :: Point, ballClick :: Bool }
initial :: State
initial = State (0,0) False

draw :: State -> Frame
draw st = fixedZoom (50,50) (AbsoluteAlignment $ ballPosition st) (Label "ball" True $ solidEllipsis blue)

event :: Event -> State -> State
event e s = case e of
    EventKey { eventKey = MouseButton LeftButton, eventKeyState = kst } -> case kst of
        Down -> if isInside
            then s { ballClick = True }
            else s
        Up -> s { ballClick = False }
    EventMotion mouse -> if ballClick s
        then s { ballPosition = mousePosition mouse }
        else s
  where
    isInside = List.elem "ball" (mouseInside $ eventMouse e)

advance :: Float -> State -> State
advance t s = s

main :: IO ()
main = do
    playRelative FullScreen white 30 initial (draw) event advance