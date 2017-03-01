
module Main where

import TAPF.Turtle
import Control.Monad

square :: Double -> Turtle ()
square len
  = let side = do { forward len; right 90}
    in do {side; side; side; side}

-- alternatives:
-- square size = sequence_ [do {forward size; right 90} | i<-[1..4]]
-- square size = forM_ [1..4] $ \i -> do {forward size; right 90}
-- square size = replicateM_ 4 $ do {forward size; right 90}

-- a recursive spiral example
-- from the Wikipedia page:
-- http://en.wikipedia.org/wiki/Turtle_graphics
spiral :: Double -> Double -> Double -> Int -> Turtle ()
spiral dist angle incr segs
  | segs>0 =
    do forward dist
       right angle
       spiral (dist+incr) angle incr (segs-1)
  | otherwise =
      return ()

example = spiral 2 89.5 4 92

main = runTurtle example (800,600)

-- alternative run using the deep embedding:
-- main = putStr $ simulateTurtle example (800,600)

