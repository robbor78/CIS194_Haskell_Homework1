{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

-- import Data.Char
import Data.Bits
-- import Data.List

main :: IO ()
--main = exercise1
--main = exercise2
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0   0.0  (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: Int -> Picture
trafficLight 1  = botCircle green & midCircle black & topCircle black & frame
trafficLight 2 = botCircle black & midCircle orange & topCircle black   & frame
trafficLight 3 = botCircle black & midCircle black & topCircle red   & frame
trafficLight 4 = botCircle black & midCircle orange & topCircle red   & frame


trafficController :: Double -> Picture
trafficController t
  | (.|.) (round t `mod` 6 == 1) (round t `mod` 6 == 2) = trafficLight 1
  | round t `mod` 6 == 3 = trafficLight 2
  | (.|.) (round t `mod` 6 == 4) (round t `mod` 6 == 5) = trafficLight 3
  | otherwise                = trafficLight 4

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

blossom :: Double -> Picture
blossom size = colored yellow (solidCircle size)

tree :: Integer -> Picture -> Picture
tree 0 blossom = blossom
tree n blossom = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) blossom ) & rotated (- pi/10) (tree (n-1) blossom))



treeController :: Double -> Picture
treeController t = tree 8 (blossom ((min t 10) / 50.0 ))
  
exercise2 :: IO ()
exercise2 =  animationOf treeController
--  drawingOf (tree 8)

-- Exercise 3

wall, ground, storage, box :: Picture
wall =   colored grey (solidRectangle 1.0 1.0)
ground =  colored yellow (solidRectangle 1.0 1.0)
storage = colored black (solidCircle 0.4) & ground 
box =     colored brown (solidRectangle 1.0 1.0)

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile n = blank




===> CONTINUE HERE drawTile maze 





       
drawRow :: Integer -> Integer -> Picture
drawRow 11 row = blank
drawRow n row = translated (-10.0) (fromIntegral row) wall & translated 1 0 (drawRow (n+1) row) 
         
pictureOfMaze :: Integer -> Picture
pictureOfMaze 11 = blank
pictureOfMaze n = drawRow (-10) n & pictureOfMaze (n+1)

exercise3 :: IO ()
exercise3 = drawingOf (pictureOfMaze (-10))
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 