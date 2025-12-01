module Main where

import Graphics.Gloss.Relative

import Data.List as List

data State = State { pressed :: Bool, selected :: Bool }

initial :: State
initial = State False False

draw :: State -> Frame
draw st = Zoom 0.5 0.5 alignCenter $ bordered 10 borderc $ Label "button" False $ Frames [solid c,labels]
    where
    borderc = if pressed st then red else c
    c = if selected st then grey else black
    labels = Grid [[banner "Press" red],[banner "Hover" grey]]
    grey = greyN 0.5

event :: Event -> State -> State
event e s = case e of
        EventKey { eventKey = MouseButton LeftButton, eventKeyState = st } -> case st of
            Down -> if isInside then s' { pressed = True } else s'
            Up -> s' { pressed = False }
        otherwise -> s'
    where
    isInside = List.elem "button" (mouseInside $ eventMouse e)
    s' = s { selected = isInside }

advance :: Float -> State -> State
advance t s = s

main :: IO ()
main = playRelative FullScreen white 30 initial draw event advance