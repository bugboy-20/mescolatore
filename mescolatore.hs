import System.IO
import System.Random

getFileLines path = lines <$> readFile path

randomizeList :: [a] -> IO [a]
randomizeList [] = return []
randomizeList l = do
    randNum <- randomRIO (0::Int, length l -1)
    tail <- randomizeList $ rm l randNum
    return (l !! randNum : tail)

rm :: [a] -> Int -> [a]
rm [] _ = []
rm (n:l) i 
  | i >= 0 = if i/=0 then n:rm l (i-1) else l
  | otherwise = error ("per qualche motivo sono arrivato a i=" ++ show i)

main = hSetBuffering stdin LineBuffering >> getFileLines "Nomi.txt" >>= randomizeList >>= printLines >> getChar 

printLines = foldr ((>>) . putStrLn) (return ())
