module Render where

main :: IO ()
main = 
    do input <- readFile "/tmp/test.hlog" >>= (\x -> return $ lines x)
       let from  = input !! 0
           cat   = input !! 1
           sub   = input !! 2
           text  = drop 3 input
       putStrLn from
       putStrLn cat
       putStrLn sub
       return ()
       
