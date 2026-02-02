module Main (main) where

import Data.Tree
import Employee
import Party

readCompany :: String -> Tree Employee
readCompany = read

main :: IO ()
main = do
  company <- fmap readCompany getContents
  let (GL guests fun) = maxFun company
  putStrLn $ "Total fun: " ++ show fun
  mapM_ (putStrLn . empName) guests
