module MyProj (runMyProj) where

import Graphics.Gloss.Interface.Pure.Game
import System.Environment
import System.IO
import System.Random
import Control.Monad

type Position = (Float, Float) 

type Maze = [Position]

data GameState = GameState
  { gamerPosition :: Position
  , mazeStructure :: Maze
  , gamePicture :: Picture
  , genator :: StdGen
  , size ::Float
  , time ::(Float,Float)
  }


makeWall :: Position -> Position -> Maze
makeWall (a,b) (c,d) 
           | or [(a > c), (b > d)] = []
           | and [(a == c), (b == d)] = [(a, b)]
           | a == c = (a,b) : makeWall (a, b+1) (c,d)
           | b == d = (a,b) : makeWall (a+1, b) (c,d)

startWalls :: Position -> Position ->StdGen -> Maze
startWalls (a,b) (c,d) gen = makeWall (a, b) (a, d) 
                          ++ makeWall (c, b) (c, d) 
                          ++ makeWall (a+2, b) (c-1, b) 
                          ++ makeWall (a+1, d) (c-2,d)
                          ++ mazeGen (a,b) (c,d) gen

generation :: Position -> Position -> Int -> StdGen -> Maze
generation (a,b) (c,d) n gen = do 
        let (h11, gen1) =randomR (0,(c-a+2)/4-1) gen 
        let (h22, gen2) =randomR (0,(c-a+2)/4-1) gen1
        let (h33, gen3) =randomR (0,(c-a+2)/4-1) gen2
        let h1 = fromIntegral(round h11)
        let h2 = fromIntegral(round h22)
        let h3 = fromIntegral(round h33)
        case n of 
            1 -> makeWall (a, (d+b)/2) ((c+a)/2+h1*2, (d+b)/2) 
              ++ makeWall ((c+a)/2+h1*2+2, (d+b)/2) (c, (d+b)/2) 
              ++ makeWall ((c+a)/2, b) ((c+a)/2, b+h2*2) 
              ++ makeWall ((c+a)/2, b+h2*2+2) ((c+a)/2, (b+d)/2+h3*2) 
              ++ makeWall ((c+a)/2, (b+d)/2+h3*2+2) ((c+a)/2, d)

            2 -> makeWall (a, (d+b)/2) (a+h1*2, (d+b)/2) 
              ++ makeWall (a+h1*2+2, (d+b)/2) ((a+c)/2+h2*2, (d+b)/2) 
              ++ makeWall ((a+c)/2+h2*2+2, (d+b)/2) (c, (d+b)/2) 
              ++ makeWall ((c+a)/2, b) ((c+a)/2, (b+d)/2+h3*2) 
              ++ makeWall ((c+a)/2, (b+d)/2+h3*2+2) ((c+a)/2, d)

            3 -> makeWall (a, (d+b)/2) (a+h1*2, (d+b)/2) 
              ++ makeWall (a+h1*2+2, (d+b)/2) (c, (d+b)/2) 
              ++ makeWall ((c+a)/2, b) ((c+a)/2, b+h2*2) 
              ++ makeWall ((c+a)/2, b+h2*2+2) ((c+a)/2, (b+d)/2+h3*2) 
              ++ makeWall ((c+a)/2, (b+d)/2+h3*2+2) ((c+a)/2, d)

            4 -> makeWall (a, (d+b)/2) (a+h1*2, (d+b)/2) 
              ++ makeWall (a+h1*2+2, (d+b)/2) ((a+c)/2+h2*2, (d+b)/2) 
              ++ makeWall ((a+c)/2+h2*2+2, (d+b)/2) (c, (d+b)/2) 
              ++ makeWall ((c+a)/2, b) ((c+a)/2, b+h3*2) 
              ++ makeWall ((c+a)/2, b+h3*2+2) ((c+a)/2, d)


mazeGen :: Position -> Position -> StdGen -> Maze
mazeGen (a,b) (c,d) gen
         | c-a <4 = []
         | otherwise = do
            let (n, newGen) = randomR (4,4) gen
            let (n1, newGen1) = randomR (n,4) newGen
            let (n2, newGen2) = randomR (n1,4) newGen1
            let (n3, newGen3) = randomR (1,n2) newGen2
            generation (a,b) (c,d) n3 newGen ++ mazeGen (a,b) ((a+c)/2,(b+d)/2) newGen ++ mazeGen ((a+c)/2,b) (c,(d+b)/2) newGen1 ++ mazeGen ((a+c)/2,(b+d)/2) (c,d) newGen2 ++ mazeGen (a,(b+d)/2) ((c+a)/2,d) newGen3


mazeToString :: Maze -> String
mazeToString [] = ""
mazeToString (x:xs) = show (fst x) ++ " " ++ show (snd x) ++ " " ++ mazeToString xs
--mazeToString (x:xs) = "(" ++ show (fst x) ++ "," ++ show (snd x) ++ ")" ++ mazeToString xs

saveMazeToFile :: FilePath -> Maze -> IO ()
saveMazeToFile path ma = writeFile path (mazeToString ma)

stringToMaze :: [Float] -> Maze
stringToMaze [] = []
stringToMaze (a:b:xs) = (a,b):stringToMaze xs


--loadMazeFromFile :: FilePath -> Maze
--loadMazeFromFile a = do
  --      contents <-readFile a
    --    stringToMaze . map read .words $contents


window :: Display
window = FullScreen
--window = InWindow "Window for Maze" (800, 800) (0, 0)

background :: Color
background = blue

posToPict :: Position -> Picture
posToPict (a, b) = polygon [(a*20-400, b*20-400), (a*20+20-400, b*20-400), (a*20+20-400, b*20+20-400), (a*20-400, b*20+20-400)]

drawing :: GameState -> Picture
drawing (GameState (a, b) mas pict _ s (t1,t2)) = pictures
    [ pict
    , Color red (posToPict (a,b))
    , Translate 380 100 $ Color white (Text (show (round t1)))
    , Translate 450 100 $ Color white (Text ":")
    , Translate 470 100 $ Color white (Text (show (round t2)))
    ]

drowLab :: Maze -> Picture
drowLab mas = pictures (map posToPict mas)


solveMaze :: Position -> Maze ->Float-> Maze
solveMaze (1,-1) _ s= []
solveMaze (a,b) m s
               | and[(a == s-1),(b==s)]= [(s-1,s)]
               |(elem (a,b) m) = []
               | otherwise = do 
                             let x = solveMaze (a+1,b) (m++[(a,b)]) s
                             let y = solveMaze (a-1,b) (m++[(a,b)]) s
                             let z = solveMaze (a,b+1) (m++[(a,b)]) s
                             let w = solveMaze (a,b-1) (m++[(a,b)]) s
                             if x /= [] then x ++ [(a,b)]
                              else if y /= [] then y ++ [(a,b)]
                               else if z /= [] then z ++ [(a,b)]
                                else if w /= [] then w ++ [(a,b)]
                                  else []

drowSolving :: Position -> Maze -> Picture ->Float-> Picture
drowSolving pos m pict s = pictures 
            [ pict
            , Color yellow (drowLab (solveMaze pos m s))
            ]


handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (GameState c d z gen s t) = do
                                                                       let (a, b) = c
                                                                       if and [(a == s-1), (b == s)] then do
                                                                                                let (n, newGen) =randomR (0,0) gen 
                                                                                                let labir = startWalls (n,0) (s,s) newGen
                                                                                                GameState (1,0) labir (drowLab labir) newGen s (0,0)
                                                                       else if (elem (a,b+1) d) then GameState (a,b) d z gen s t
                                                                                                else GameState (a,b+1) d z gen s t
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (GameState c d z gen s t) = do
                                                                       let (a, b) = c
                                                                       if or [(elem (a,b-1) d), and [(a == 1), (b == 0)]] then GameState (a,b) d z gen s t
                                                                                           else GameState (a,b-1) d z gen s t
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) (GameState c d z gen s t) = do
                                                                       let (a, b) = c
                                                                       if (elem (a-1,b) d) then GameState (a,b) d z gen s t
                                                                                           else GameState (a-1,b) d z gen s t
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (GameState c d z gen s t) = do
                                                                       let (a, b) = c
                                                                       if (elem (a+1,b) d) then GameState (a,b) d z gen s t
                                                                                           else GameState (a+1,b) d z gen s t
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) (GameState c d z gen s t) = GameState c d (drowSolving c d z s) gen s t

handleEvent _ g = g


update :: Float -> GameState -> GameState
update sec (GameState c d z gen s (m,se))
                   |(se==59) = GameState c d z gen s (m+1,0)
                   |otherwise = GameState c d z gen s (m,se+1)

chooseSize::String -> Float
chooseSize s
         |(s=="s") = 8
         |(s=="m") = 16
         |(s=="l") = 32
chooseSize _ = 16         
-- chooseSize::Float-> Float
-- chooseSize s = s*8

runMyProj :: IO ()
runMyProj = do
    gen <- newStdGen

    putStrLn "Press 1,if you want generate new labirint and play"
    putStrLn "Press 2,if you want generate new labirint and save it in file"
    putStrLn "Press 3,if you want load labirint from file and play"
    n <- getLine
    let x = (read n :: Int)
    putStrLn "Choose size of labirint: press S for small, M for medium ,L for large"
    s <- getLine
    let size = chooseSize s
    case x of
      1 -> do
       -- putStrLn "Choose size of labirint: press S for small, M for medium ,L for large"
       -- s <- getLine
       --let size = chooseSize s
       --let size = chooseSize (read s :: Float)
       let labir = startWalls (0,0) (size,size) gen
       let initState = GameState (1,0) labir (drowLab labir) gen size (0,0)
       play window background 1 initState drawing handleEvent update

      2 -> do
        -- putStrLn "Choose size of labirint: press S for small, M for medium ,L for large"
        -- s <- getLine
        -- let size = chooseSize s
        --let size = chooseSize (read s :: Float)
        let labir = startWalls (0,0) (size,size) gen
        saveMazeToFile "lab.txt" labir
      3 -> do
        putStrLn "Input name of file"
        a <- getLine
        contents <- readFile a
        --"lab.txt"
        -- putStrLn "Choose size of labirint: press S for small, M for medium ,L for large"
        -- s <- getLine
        -- let size = chooseSize s
        --let size = chooseSize (read s :: Float)
        let labir = stringToMaze.map read.words $contents  
        let initState = GameState (1,0) labir (drowLab labir) gen size (0,0)
        play window background 1 initState drawing handleEvent update
