import System.IO
import System.Random.Shuffle

getFileLines path = lines <$> readFile path

randomizeList :: [a] -> IO [a]
randomizeList = shuffleM

main = hSetBuffering stdin LineBuffering >> getFileLines "Nomi.txt" >>= randomizeList >>= printLines >> getChar 

printLines = foldr ((>>) . putStrLn) (return ())
