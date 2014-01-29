import Geometry
import Problem (Problem (..), Solution (..))
import Data.Ratio (Rational)
import System.Random (RandomGen, randomR, mkStdGen)

-- import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (($=), GLfloat)

import qualified BreadthFirst
import qualified UniformCost
import qualified AStar

-----  Polygons world state space -----

type PolySpace = Rational
type PolyState = Vertex PolySpace
type PolyAction = (PolyState, PolyState)

triangle :: (Num a) => a -> a -> a -> Polygon a
triangle x y w = [(x+w,y-w),(x,y+w),(x-w,y-w)]

randomTriangle :: (RandomGen g, Num a) => g -> (Polygon a, g)
randomTriangle gen = (triangle (fromInteger x) (fromInteger y) 20, gen'')
    where (x, gen') = randomR (100,300) gen
          (y, gen'') = randomR (0,400) gen'

terrain :: [Polygon PolySpace]
terrain = fst $ foldl (\(xs, gen) _ -> let (t,g) = randomTriangle gen in (t:xs, g)) ([], mkStdGen 6) [1..100]

polystart :: PolyState
polystart = (0,200)
polygoal :: PolyState
polygoal = (400,200)

polycost :: PolyState -> PolyAction -> Double
polycost _ ((x0,y0),(x1,y1)) = sqrt . fromRational $ (x1-x0)^2 + (y1-y0)^2

polyActions :: PolyState -> [PolyAction]
polyActions st = filter legal [(st,next) | next <- polygoal:(concat terrain), st /= next]
    where legal action = not $ or (map (p_intersects action) terrain)

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
  print terrain
  let sol = AStar.search prob heuristic
  case sol of
    Just solution -> print (getPath prob solution)
    Nothing -> putStrLn "No solutions found"

  (progname, _) <- GLUT.getArgsAndInitialize
  GLUT.createWindow "Hello World"
  GLUT.displayCallback $= display sol
  GLUT.mainLoop

type GLVertex = (GLfloat,GLfloat,GLfloat)

spaceToGL :: (PolySpace, PolySpace) -> GLVertex
spaceToGL (x,y) = (fromRational (x-200)/200,fromRational (y-200)/200,0)

myPoints :: [GLVertex]
myPoints = concat $ map (map spaceToGL) terrain

pushVertex :: GLVertex -> IO ()
pushVertex (x,y,z) = GLUT.vertex $ GL.Vertex3 x y z

display sol = do
  GL.clear [GL.ColorBuffer]
  GL.renderPrimitive GL.Triangles $ mapM_ pushVertex myPoints
  GL.renderPrimitive GL.LineLoop $ mapM_ pushVertex [(-1,-1,0),(1,-1,0),(1,1,0),(-1,1,0)]
  case sol of
    Just solution -> GL.renderPrimitive GL.LineStrip $ do
                       GLUT.color $ (GL.Color3 (1.0::GLfloat) 0 0)
                       mapM_ (pushVertex . spaceToGL) (getPath prob solution)
    Nothing -> return ()
  GL.flush
