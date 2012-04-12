import Data.List
import Control.Monad

type KnightPos = (Int,Int)
type Path = [KnightPos]

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
    ]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

movePath :: Path -> [Path]
movePath path = map (\x -> path ++ [x]) $ moveKnight (last path)

inManyPath :: Int -> KnightPos -> [Path]
inManyPath x start = return [start] >>= foldr (<=<) return (replicate x movePath)

canReachInPath :: Int -> KnightPos -> KnightPos -> Maybe [Path]
canReachInPath x start end =
    if paths == []
        then Nothing
        else Just paths
    where paths = filter (\x -> end == last x) $ inManyPath x start
