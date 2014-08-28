module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Data.Function (on)

-- import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import           Graphics.UI.GLUT (($=), GLfloat)

import qualified Chapter3.AStar as AStar
-- import qualified Chapter3.BreadthFirst
import           Chapter3.Problem (Problem (..), Solution (..))
-- import qualified Chapter3.UniformCost

import           Chapter3.Geometry

-----  Polygons world state space -----

type PolySpace = Rational
type PolyState = Vertex PolySpace
type PolyAction = (PolyState, PolyState)

triangle :: (Num a) => a -> a -> a -> Polygon a
triangle x y w = [(x+w,y-w),(x,y+w),(x-w,y-w)]

randomTriangle :: (Applicative m, MonadRandom m) => m (Polygon PolySpace)
randomTriangle = on triangle fromInteger <$> getRandomR (100,350) <*> getRandomR (0,400) <*> (fromInteger <$> getRandomR (2, 30))

terrain :: [Polygon PolySpace]
terrain = evalRand (replicateM 100 randomTriangle) (mkStdGen 8)

polystart :: PolyState
polystart = (0,0)
polygoal :: PolyState
polygoal = (400,400)

polycost :: PolyState -> PolyAction -> Double
polycost _ ((x0,y0),(x1,y1)) = sqrt . fromRational $ (x1-x0)^2 + (y1-y0)^2

inmap :: PolyState -> Bool
inmap (x, y) = 0 <= x && x <= 400 && 0 <= y && y <= 400

polyActions :: PolyState -> [PolyAction]
polyActions st = filter legal [(st,next) | next <- polygoal:(concat terrain), st /= next]
    where legal action = not (or (map (p_intersects action) terrain))
                         && inmap (snd action)

polymove :: PolyState -> PolyAction -> PolyState
polymove _ (_,s) = s

--- solve it ---

prob :: Problem PolyAction PolyState
prob = Problem { initial = polystart,
                 actions = polyActions,
                 result = polymove,
                 goal = (==polygoal),
                 cost = polycost }

getPath :: Problem a s -> Solution a s -> [s]
getPath problem sol = scanl (result problem) (initial problem) (reverse $ getActions sol)

heuristic :: PolyState -> Double
heuristic s = polycost s (s,polygoal)

main :: IO ()
main = do
  let sol = AStar.search prob heuristic
  case sol of
    Just solution -> print (getPath prob solution)
    Nothing -> putStrLn "No solutions found"

  void GLUT.getArgsAndInitialize
  void $ GLUT.createWindow "Hello World"
  GLUT.displayCallback $= display sol
  GLUT.mainLoop

type GLVertex = (GLfloat,GLfloat,GLfloat)

spaceToGL :: (PolySpace, PolySpace) -> GLVertex
spaceToGL (x,y) = (fromRational (x-200)/200,fromRational (y-200)/200,0)

myPoints :: [GLVertex]
myPoints = concat $ map (map spaceToGL) terrain

pushVertex :: GLVertex -> IO ()
pushVertex (x,y,z) = GLUT.vertex $ GL.Vertex3 x y z

display :: Maybe (Solution PolyAction PolyState) -> IO ()
display sol = do
  GL.clear [GL.ColorBuffer]
  GLUT.color $ (GL.Color3 (1.0::GLfloat) 1 1)
  GL.renderPrimitive GL.Triangles $ mapM_ pushVertex myPoints
  GL.renderPrimitive GL.LineLoop $ mapM_ pushVertex [(-1,-1,0),(1,-1,0),(1,1,0),(-1,1,0)]
  case sol of
    Just solution -> GL.renderPrimitive GL.LineStrip $ do
                       GLUT.color $ (GL.Color3 (1.0::GLfloat) 0 0)
                       mapM_ (pushVertex . spaceToGL) (getPath prob solution)
    Nothing -> return ()
  GL.flush
