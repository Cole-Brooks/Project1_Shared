import Control.Monad
import System.IO
import System.Environment
import Data.Char
    {-
    User will use command 
    runhaskell Main.hs

    which will then prompt for a command. The list of commands and program responses are noted below

    -----------------------------------------------------------------------------------------------------------------------------
    Read <filename>: 
    filename is the name of a file expected to contain a sorting network. Read will read
    in the network and then print it out again to a file called network.txt in a format where each comparison
    has its own line like this:

    1 -- 2
    3 -- 4
    1 -- 3
    2 -- 4
    2 -- 3

    -----------------------------------------------------------------------------------------------------------------------------
    Run <filename> <sequence>: 
    filename is the name of a file containing a comparator network, and sequence is a list like [5,1,3,0]
    You may assume that the list is in the correct format, that all numbers in the list are distinct, and that
    its length is greater than or equal to the biggest wire number in the comparator network that will be used.

    Run should print out the result of applying the network to the sequence. 

    -----------------------------------------------------------------------------------------------------------------------------
    Parallel <filename>:
    filename is the name of a file containing a comparator network. This command should print the parallel form to 
    a file called parallel.txt in this format (same example as Read)

    1 -- 2 , 3 -- 4
    1 -- 3 , 2 -- 4
    2 -- 3

    -----------------------------------------------------------------------------------------------------------------------------
    Sorting <filename>:
    Print out 'True' if the comparator network in the given file is a sorting network, else False.

    -----------------------------------------------------------------------------------------------------------------------------
    Create <n>:
    Creates a sorting network with n wires.

    -}
read_f :: String -> IO ()
read_f file_name = do
    (parsePairs file_name) >>= part2

run_f :: String -> [Int] -> IO ()
run_f str_in sequence = do
    -- note that the string massaging will need to be done here
        pairs <- parsePairs str_in
        -- print pairs
        -- netPrint pairs
        putStrLn $ show (sortSeq sequence pairs)

parallel_f :: String -> IO ()
parallel_f file_name = do
    putStrLn  file_name

sorting_f :: String -> IO ()
sorting_f file_name = do
    putStrLn  file_name

create_f :: String -> IO ()
create_f str_in = do
    putStrLn  str_in

handle_command = do
    -- putStrLn "Enter Command: "
    cmd <- getArgs
    putStrLn $ show cmd

    case cmd of
        ("Read":xs) -> read_f $ head xs
        ("Run":xs) -> run_f (head xs)  (read (head (tail xs)))
        ("Parallel":xs) -> parallel_f $ head xs
        ("Sorting":xs) -> sorting_f $ head xs
        ("Create":xs) -> create_f $ head xs
        otherwise -> putStrLn "Invalid Command"
    -- handle_command

main :: IO ()
main = do
    handle_command


parsePairs :: String -> IO [(Int,Int)]
parsePairs fn = do
        handle <- openFile fn ReadMode
        contents <- hGetContents handle
        let contentData = read contents :: [(Int,Int)]
        print(contentData)
        hClose handle
        return contentData

pairToBar :: (Int,Int) -> String
pairToBar (a,b) = (show a) ++ " -- " ++ (show b)

netPrint :: [(Int,Int)] -> IO ()
netPrint (x:xs) = do
        putStrLn(pairToBar x)
        netPrint xs
netPrint [] = return ()

-- Writes the list
part2 :: [(Int,Int)] -> IO ()
part2 x = writeFile "network.txt" (foldr (\i a -> i ++ "\n" ++ a) [] (fmap (pairToBar) x))

main2 = do
        pairs <- parsePairs "sort1.txt"
        print pairs
        netPrint pairs
        part2 pairs


swap :: [a] -> (Int,Int) -> [a]
swap list (a,b) = [at_index item index | (item, index) <- zip list[0..length list - 1]]
    where at_index x index | index == (a-1) = list !! (b-1)
                           | index == (b-1) = list !! (a-1)
                           | otherwise = x

doSwap :: Ord a => [a] -> (Int, Int) -> Bool
doSwap list (a,b) = (list !! (b-1))<(list !! (a-1))

sortSeq :: Ord a => [a] -> [(Int,Int)] -> [a]
sortSeq x [] = x
sortSeq x (y:ys) = if (doSwap x y) then sortSeq (swap x y) ys else sortSeq x ys

main3 = do
        pairs <- parsePairs "sort1.txt"
        print pairs
        netPrint pairs
        putStrLn $ show (sortSeq [5,1,3,0] pairs)

createNetwork :: Int -> [(Int,Int)]
createNetwork x = foldr (++) [] [([(w-1,w) | w <- reverse [2..v]]) | v <- [1..(x)]]
