module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Main
main :: IO ()
main = do
  sg <- getStdGen
  let world = World (Player (0,0) 0 50 D D) (redApple (15,15)) [] sg
  play (InWindow "Mine Runner" (width,height) (10,10))
    white 60 world draw event step

-- Model
width :: Num a => a
width = 600
height :: Num a => a
height = 400
appleSize :: Num a => a
appleSize = 10
playerSize :: Num a => a
playerSize  = 10
mineSize :: Num a => a
mineSize = playerSize

data World = World { player :: Player
                   , apple :: Apple
                   , mines :: [Point]
                   , stdgen  :: StdGen
                   }

data Direction = U | D | L | R deriving (Eq)

data Player = Player { pPos :: Point
                     , score :: Int
                     , speed :: Float
                     , oldDir :: Direction
                     , newDir :: Direction
                     }

data Apple = Apple { aPos :: Point
                   , fun :: Player -> Player
                   , aColor :: Color
                   }

-- Display
draw :: World -> Picture
draw (World { player = (Player { pPos = (px,py), newDir = dir, score = sco })
            , apple = (Apple { aPos = (a,b), aColor = c })
            , mines = m })
  = pictures [ translate (-50 * (fromIntegral . length $ show sco)) (-30) . text $ show sco
             , pictures $ zipWith
               (\n (x,y) -> color (greyN n) $ rect x y mineSize mineSize)
               ([0.75, 0.5, 0.25] ++ repeat 0) m -- colour the first three mines grey
             , translate a b $ drawApple c
             , translate px py $ drawPlayer dir
             ]

drawPlayer :: Direction -> Picture
drawPlayer dir = pictures [circleSolid (playerSize/2), eyes]
  where s = playerSize/5
        base =  pictures [ translate (-s) s . color white $ circle 1.75
                         , translate   s  s . color white $ circle 1.75 ]
        eyes = (flip rotate) base $ case dir of U -> 0; D -> 180; L -> 270; R -> 90

drawApple :: Color -> Picture
drawApple c = pictures [ color c $ circleSolid (appleSize/2)
                       , translate 1 (appleSize/3) . color green $ circleSolid 2.5 ]

rect :: Float -> Float -> Float -> Float -> Picture
rect x y w h = translate x y $ rectangleSolid w h

-- Update
event :: Event -> World -> World
event (EventKey (SpecialKey x) Down _ _) w
  = w { player = (player w) { newDir = d } }
  where d = case x of
          KeyUp -> U
          KeyDown -> D
          KeyLeft -> L
          KeyRight -> R
          _ -> oldDir $ player w
event _ w = w

step :: Float -> World -> World
step dt = hit . eat . move dt

hit :: World -> World
hit w@(World { player = p@(Player { pPos = pos }) , mines = m })
  = if any (\mp -> pointInCircle mp pos playerSize) $ drop 3 m
    then w { player = p { score = 0, speed = 50 } , mines = [] } else w

eat :: World -> World
eat w@(World { apple = (Apple { aPos = ap, fun = f })
             , player = p@(Player { pPos = pp })
             , stdgen = sg }) =
     if pointInCircle ap pp playerSize
     then let (a,sg')  = randomApple sg (mines w)
          in w { apple = a, stdgen = sg', player = f p }
     else w

move :: Float -> World -> World
move dt w@(World { player = p@(Player { pPos = pos, oldDir = od, newDir = nd, speed = v }) , mines = m })
  = let newMines = if nd /= od then pos : m else m
        np = (wrapScreen $ pos `add` dirToPoint nd (v*dt))
    in w { player = p { pPos = np, oldDir = nd } , mines = newMines }

-- Utility
-- apples
redApple :: Point -> Apple
redApple p = Apple p (\s -> s { score = succ $ score s, speed = 2 + speed s }) red

greenApple :: Point -> Apple
greenApple p = Apple p (\s -> s { score = 3 + score s, speed = 2 + speed s }) $ dark green

randomApple :: StdGen -> [Point] -> (Apple, StdGen)
randomApple sg mins
  = let (x,sg') = randomR (-width/2 + appleSize/2, width/2 - appleSize/2) sg :: (Float,StdGen)
        (y,sg'') = randomR (-height/2 + appleSize/2 ,height/2 - appleSize/2) sg' :: (Float,StdGen)
        (which,sg''') = randomR (0,10) sg'' :: (Float,StdGen)
    in if any (\m -> pInBox (x,y) (boxAround m mineSize)) mins
       then randomApple sg''' mins
       else (if which >= 7 then greenApple (x,y) else redApple (x,y), sg''')

-- points
add :: Point -> Point -> Point
add (a,b) (x,y) = (a+x,b+y)

wrap :: (Num a, Ord a) => a -> a -> a -> a
wrap mi ma n
  | n > ma = mi
  | n < mi = ma
  | otherwise = n

wrapScreen :: Point -> Point
wrapScreen (x,y) = ((wrap (-width /2) (width /2) x), (wrap (-height/2) (height/2) y))

dirToPoint :: Direction -> Float -> Point
dirToPoint d x = case d of
  U -> (0,x)
  D -> (0,-x)
  L -> (-x,0)
  R -> (x,0)

-- collision
dist :: Point -> Point -> Float
dist (x,y) (a,b) = sqrt $ (x-a)**2 + (y-b)**2

pointInCircle :: Point -> Point -> Float -> Bool
pointInCircle p c r = dist p c <= r

boxAround :: Point -> Float -> (Point,Point)
boxAround (x,y) w = ((x-w, y-w), (x+w, y+w))

pInBox :: Point -> (Point,Point) -> Bool
pInBox p (u,l) = pointInBox p u l
