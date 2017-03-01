{-
  Implementation of the Turtle DSEL using a "deep" embedding
  Pedro Vasconcelos, 2015-2017
-}
{-# LANGUAGE DeriveFunctor #-}
module TAPF.Turtle.Deep
       ( -- * abstract type for turtle programs
         Turtle
         -- * two run functions
       , runTurtle
       , simulateTurtle
         -- * primitive turtle commands
       , forward
       , right
       , heading 
       , output
       ) where


import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Graphics.HGL

-- the Turtle program representation
data Turtle a = Forward Double (Turtle a)
              | TurnRight Double (Turtle a)
              | Heading (Double -> Turtle a)
              | Output String (Turtle a)
              | Return a
                deriving Functor

{-
instance Show (a -> b) where
  showsPrec _ f = ("<<function>>"++)
-}

{- monad instance for Turtle programs
  NB: this is essentially a "free monad":
  http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.htm
-}
instance Monad Turtle where
  return = Return 
  Forward dist k  >>= k' = Forward dist (k >>= k')
  TurnRight ang k >>= k' = TurnRight ang (k >>= k')
  Heading k       >>= k' = Heading (\h -> (k h >>= k'))
  Output str k    >>= k' = Output str (k >>= k')
  Return a        >>= k' = k' a


-- applicative instance for Turtle programs
instance Applicative Turtle where
  pure = return
  a <*> b = do {f <- a; x <- b; return (f x)}


-- | basic movement and rotation commands
forward, right :: Double -> Turtle ()
forward dist = Forward dist (Return ())
right ang    = TurnRight ang (Return ())

-- | get current heading
heading      :: Turtle Double
heading      = Heading Return 

-- | output a string message
output :: String -> Turtle ()
output str = Output str (Return ())


-- internal turtle state
data TurtleState
  = TurtleState { _position :: Point
                , _heading :: Double  -- in degrees
                }
  
-- initial  state;
-- position turtle in the center of window, heading east
initialTurtle :: Size -> TurtleState
initialTurtle (width, height)
  = TurtleState { _position = (width`div`2, height`div`2)
                , _heading = 0
                }

-- | run function in a graphic window
-- 
runTurtle :: Turtle a -> Size -> IO ()
runTurtle m size =
    runGraphics $ 
    withWindow "Turtle Graphics" size $ \w -> do
      -- get window *actual* dimensions
      (_, sz) <- getWindowRect w  
      runStateT (interpret w m) (initialTurtle sz)
      -- wait for a keypress before closing the window
      getKey w 
      return ()

-- | interpret in state transformer over IO
interpret :: Window -> Turtle a -> StateT TurtleState IO a
interpret w (Return a) = return a
interpret w (Forward dist next) = do
  s <- get
  let start = _position s
  let dest = advance start (radians $ _heading s) dist
  lift $ drawInWindow w $ line start dest
  put s { _position=dest }
  interpret w next

interpret w (TurnRight ang next) = do
  modify $ \s -> s { _heading = _heading s + ang }
  interpret w next

interpret w (Heading next) = do
  s <- get
  interpret w (next $ _heading s)

interpret w (Output str next) = do
  lift $ putStrLn str
  interpret w next


-- | a "simulator" run function that simply logs all lines drawn
simulateTurtle :: Turtle a -> Size -> String
simulateTurtle m size
    = snd $ runWriter $ evalStateT (simulate m) (initialTurtle size)

simulate :: Turtle a -> StateT TurtleState (Writer String) a
simulate (Return a) = return a
simulate (Forward dist next) = do
  s <- get
  let start = _position s
  let dest = advance start (radians $ _heading s) dist
  put s { _position= dest }
  tell ("line " ++ show start ++ " to " ++ show dest ++ "\n")
  simulate next
       
simulate (TurnRight ang next) = do
  modify $  \s -> s { _heading = _heading s + ang }
  simulate next

simulate (Heading next) = do
  s <- get
  simulate (next $ _heading s)

simulate (Output str next) = do
  tell (str ++ "\n")
  simulate next


-- * helper functions

-- | move a point forward given heading and distance
advance :: Point -> Double -> Double -> Point
advance (x,y) angle dist = (x',y')
  where x' = x + round (dist * cos angle)
        y' = y + round (dist * sin angle)
  

-- | convert degrees to radians
radians :: Double -> Double
radians = (pi/180*)

