module Main where

import FerromagnetStiffness
import System.Environment

--data Theta = Theta {theta_x :: Double, theta_y :: Double}
data Radius = Circle Double | Square Double

space :: Radius->Double
space (Circle x) = pi*x**2
space (Square x) = (2*x)**2

parserArgs args len = 
  if (null args)
  then -1
  else calculation (args!!0)


calculation lambda = read lambda :: Double

main = do
  args <- getArgs
  putStrLn "This is Main module"
  (putStrLn . show) (parserArgs args (length args))
  
