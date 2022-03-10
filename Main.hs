-- import Control.Monad
import System.IO
    ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import System.Environment ( getArgs )
import Language.Haskell.TH.Lib (listP)
import Data.List ()
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
    putStrLn "---------------------"
    (parsePairs file_name) >>= part4 

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
        -- ("Test":xs) -> printList_f $ [ head xs ]
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

pairToBarWSpace :: (Int,Int) -> String 
pairToBarWSpace (a,b) = " " ++ (show a) ++ " -- " ++ (show b) ++ " "

listPairToBar :: [(Int, Int)] -> String -> String 
listPairToBar [] str = str ++ "\n"
listPairToBar (x:xs) str = listPairToBar xs (str ++ pairToBarWSpace x)

loLPairToBar :: [[(Int, Int)]] -> String -> String
loLPairToBar [] str = str
loLPairToBar (x:xs) str = loLPairToBar xs (str ++ listPairToBar x str)

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



-- Writes the list
part4 :: [(Int,Int)] -> IO ()
part4 x = do
    writeFile "parallel.txt" (cLoLoT (p_LoT x 0 [] [] []) "")
    -- (mapM_ print  (fmap (fmap pairToBarWSpace) (p_LoT x [] [])))

-- part4 x = do
--     outh <- openFile "parallel.txt" WriteMode
--     part4_print (fmap (fmap pairToBarWSpace) (p_LoT x 0 [] [] [])) outh

-- part4_print :: [[String]] -> Handle -> IO ()
-- part4_print [] outh = hClose outh
-- part4_print (x:xs) outh = do
--     hPutStrLn outh (head x ++ tail x)
--     part4_print xs outh


--------------- part4 helpers --------------------
-- Convert List of List of Tuples to String
cLoLoT :: [[(Int,Int)]] -> String -> String
cLoLoT [] tmpStr = tmpStr
cLoLoT (input:input_end) tmpStr = do
    cLoLoT input_end (tmpStr ++ cLoT input "")

-- Convert List of Tuples to String
cLoT :: [(Int, Int)] -> String -> String
cLoT [] tmpStr = tmpStr ++ "\n"
cLoT (input:input_end) tmpStr = do
    cLoT input_end (tmpStr ++ (pairToBarWSpace input))

-- Parallelize List of Tuples
p_LoT :: [(Int, Int)] -> Int -> [Int] -> [(Int,Int)] -> [[(Int,Int)]] -> [[(Int,Int)]]
p_LoT input i u_wires p_steps out | ((length input) == i) = out ++ [p_steps]
p_LoT input i u_wires p_steps out = do
    -- check if next tuple uses wires in u_wires. If not, add them to c_used_wires
    if not ((fst (input!!i) `elem` u_wires) || (snd (input!!i) `elem` u_wires))
        then p_LoT input (i+1) (aTtL u_wires (input!!i)) (a_TtLoT p_steps (input!!i)) out
    else p_LoT input (i) [] [] (out ++ [p_steps])

-- p_LoT :: [(Int, Int)] -> [(Int,Int)] -> [[(Int,Int)]]-> [[(Int,Int)]]
-- p_LoT [] u_tuples out = out
-- p_LoT input u_tuples out = p_LoT (tail input) u_tuples (out ++ [(fParals input [] (a_TtLoT u_tuples (head input)) [])])

-- -- find parallels
-- fParals :: [(Int, Int)] -> [Int] -> [(Int, Int)] -> [(Int,Int)] -> [(Int,Int)]
-- fParals [] u_wires u_tuples out = out
-- fParals (input:input_end) u_wires u_tuples out = do
--     if not (((fst input) `elem` u_wires || (snd input) `elem` u_wires) || input `elem` u_tuples)
--         then fParals input_end (aTtL u_wires input) (a_TtLoT u_tuples input) (a_TtLoT out input)
--     else fParals input_end u_wires u_tuples out

-- Add Tuple to List of Tuples
a_TtLoT :: [(Int, Int)] -> (Int,Int) -> [(Int,Int)]
a_TtLoT [] pair = [pair]
a_TtLoT (x:xs) pair = x : [pair]

-- Add Tuple to List
aTtL :: [Int] -> (Int,Int) -> [Int]
aTtL [] pair = [(fst pair), (snd pair)]
aTtL (x:xs) pair = x : [(fst pair), (snd pair)]


part6 :: Int -> IO()
part6 x = writeFile "parallel.txt" (cLoLoT (p_LoT (createNetwork x) 0 [] [] []) "")

createNetwork :: Int -> [(Int,Int)]
createNetwork x = foldr (++) [] [([(w-1,w) | w <- reverse [2..v]]) | v <- [1..(x)]]
