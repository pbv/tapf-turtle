{-
  Turtle: a simple turtle graphics DSL in Haskell
  Pedro Vasconcelos, 2015, 2017
-}
module TAPF.Turtle
       ( module Impl   -- re-export implementation module
       , Point, Size   -- re-export from HGL
       -- * derived operations
       , left, backward
       ) where

import Graphics.HGL (Point, Size)
import TAPF.Turtle.Deep as Impl
-- a simpler "shallow" implementation:
-- import TAPF.Turtle.Shallow as Impl

-- * derived operations
-- | turn left
left :: Double -> Turtle ()
left ang = right (-ang)

-- | move backwards
backward :: Double -> Turtle ()
backward dist = forward (-dist)



