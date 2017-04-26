{-
  Implementation of the Turtle graphics DSEL using shallow embedding
  Using IORefs instead of state transformer
  Pedro Vasconcelos, 2015--2017
-}
{-# LANGUAGE DeriveFunctor #-}

module TAPF.Turtle.ShallowIORefs
       (  -- * abstract type for turtle programs
         Turtle
         -- * run function
       , runTurtle
         -- * primitive turtle commands
       , forward
       , right 
       , heading         -- get the current heading
       , output          -- logging 
       ) where

import Data.IORef
import Control.Applicative
import Graphics.HGL

-- | the Turtle monad; 
-- functions from mutable state to IO actions 
newtype Turtle a = Turtle { unTurtle :: TurtleStateRef -> IO a }
                   deriving (Functor)

instance Applicative Turtle where
  pure x  = Turtle $ \_ -> pure x
  a <*> b = Turtle $ \ref -> unTurtle a ref <*> unTurtle b ref

instance Monad Turtle where
  m >>= k = Turtle $ \ref -> unTurtle m ref >>= (\x -> unTurtle (k x) ref)


-- the internal turtle state;
-- current position and heading
-- window for drawing 
data TurtleState
  = TurtleState { _position :: Point -- from Graphics.HGL
                , _heading :: Double -- degrees
                , _window :: Window  -- from Graphics.HGL
                }

-- state reference (for mutable state)
type TurtleStateRef = IORef TurtleState


runTurtle :: Turtle a -> Size -> IO ()
runTurtle m size =
    runGraphics $ 
    withWindow "Turtle Graphics" size $ \w -> do
      -- get window actual dimensions
      sz <- snd <$> getWindowRect w  
      ref <- newIORef (initialTurtle w sz)
      unTurtle m ref
      getKey w -- wait for a keypress
      return ()



-- | initial turtle state;
-- position turtle in the center of window, heading east
initialTurtle :: Window -> Size -> TurtleState
initialTurtle w (width, height)
  = TurtleState { _position = (width`div`2, height`div`2)
                , _heading = 0
                , _window = w
                }


-- move forward a certain distance (in pixels)
forward :: Double -> Turtle ()
forward dist = Turtle $ \ref -> do
  s <- readIORef ref
  let start = _position s
  let dest = advance start (radians $ _heading s) dist
  drawInWindow (_window s) $ line start dest
  writeIORef ref s { _position=dest }

-- | turtle movement
right :: Double -> Turtle ()
right ang = Turtle $ \ref ->
  modifyIORef ref (\s -> s { _heading = _heading s + ang})

-- | get current heading
heading :: Turtle Double
heading = Turtle $ \ref ->
  do s <- readIORef ref; return (_heading s)
             

-- | logging
output :: String -> Turtle ()
output str = Turtle $ \_ -> putStrLn str


-- | auxiliary functions
-- | move a point forward by heading and distance
advance :: Point -> Double -> Double -> Point
advance (x,y) angle dist = (x',y')
  where x' = x + round (dist * cos angle)
        y' = y + round (dist * sin angle)
  

-- | convert degrees to radians
radians :: Double -> Double
radians = (pi/180*)

