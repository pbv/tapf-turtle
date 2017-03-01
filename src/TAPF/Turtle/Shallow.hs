{-
  Implementation of the Turtle graphics DSEL using shallow embedding
  Pedro Vasconcelos, 2015--2017
-}
{-# LANGUAGE DeriveFunctor #-}
module TAPF.Turtle.Shallow
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

import Control.Monad.State
import Control.Applicative
import Graphics.HGL

-- | the Turtle monad; 
-- state transformer over IO (for graphics)
newtype Turtle a = Turtle { unTurtle :: StateT TurtleState IO a }
                   deriving Functor

-- we use a newtype to "hide" the implementation;
-- alternatively, we could use: 
-- > type Turtle a = StateT TurtleState IO a

-- make it a monad instance;
-- just unwrap the newtype constructor 
instance Monad Turtle where
  return = Turtle . return 
  Turtle m >>= k = Turtle $ m >>= \x -> unTurtle (k x)

instance Applicative Turtle where
  pure = return
  a <*> b = do {f <- a; x <- b; return (f x)}

-- the internal turtle state;
-- current position and heading
-- window for drawing 
data TurtleState
  = TurtleState { _position :: Point -- from Graphics.HGL
                , _heading :: Double -- degrees
                , _window :: Window  -- from Graphics.HGL
                }

runTurtle :: Turtle a -> Size -> IO ()
runTurtle m size =
    runGraphics $ 
    withWindow "Turtle Graphics" size $ \w -> do
      -- get window actual dimensions
      sz <- liftM snd (getWindowRect w)  
      runStateT (unTurtle m) (initialTurtle w sz)
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
forward dist = Turtle $ do
  s <- get
  let start = _position s
  let dest = advance start (radians $ _heading s) dist
  lift $ drawInWindow (_window s) $ line start dest
  put s { _position=dest }

-- | turtle movement
right :: Double -> Turtle ()
right ang = Turtle $ modify $ \s -> s { _heading = _heading s + ang}

-- | get current heading
heading :: Turtle Double
heading = Turtle $ do s <- get; return (_heading s)

-- alternative:
-- heading = Turtle $ gets _heading
                      
-- NB: the `gets' function
-- gets :: MonadState s m  => (s -> a) -> m a
-- applies a function to the current state
                      

-- | logging
output :: String -> Turtle ()
output str = Turtle $ lift $ putStrLn str


-- | auxiliary functions
-- | move a point forward by heading and distance
advance :: Point -> Double -> Double -> Point
advance (x,y) angle dist = (x',y')
  where x' = x + round (dist * cos angle)
        y' = y + round (dist * sin angle)
  

-- | convert degrees to radians
radians :: Double -> Double
radians = (pi/180*)

