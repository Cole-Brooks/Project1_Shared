import Control.Monad
import System.IO
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
    putStrLn  file_name

run_f :: String -> IO ()
run_f str_in = do
    -- note that the string massaging will need to be done here
    putStrLn  str_in

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
    putStrLn "Enter Command: "
    cmd <- getLine

    case words cmd of
        ("Read":_) -> read_f (drop 5 cmd)
        ("Run":_) -> run_f (drop 4 cmd)
        ("Parallel":_) -> parallel_f (drop 9 cmd)
        ("Sorting":_) -> sorting_f (drop 8 cmd)
        ("Create":_) -> create_f (drop 7 cmd)
        otherwise -> putStrLn "Invalid Command"
    handle_command

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

main2 = do
        pairs <- parsePairs "sort1.txt"
        print pairs
        netPrint pairs
