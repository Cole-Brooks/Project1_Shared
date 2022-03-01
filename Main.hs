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
read_f = do
    putStrLn  "Read initiated"

run_f = do
    putStrLn  "Run initiated"

parallel_f = do
    putStrLn  "Parallel initiated"

sorting_f = do
    putStrLn  "Sorting initiated"

create_f = do
    putStrLn  "Create initiated"

handle_command = do
    putStrLn "Enter Command: "
    cmd <- getLine

    case words cmd of
        ("Read":_) -> read_f
        ("Run":_) -> run_f
        ("Parallel":_) -> parallel_f
        ("Sorting":_) -> sorting_f
        ("Create":_) -> create_f
        otherwise -> putStrLn "Invalid Command"
    handle_command

main :: IO ()
main = do
    handle_command