{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Control.Monad
import System.Exit
import System.Random
import Database.LevelDB
import Data.ByteString.Char8 hiding (take, putStrLn, getLine, map)
import Control.Monad.IO.Class (liftIO)

data RawAction = String String | Number Int

data Action = Rock | Paper | Scissors | Quit | Invalid
            deriving Show
                     
data Result = Win | Lose | Tie

main :: IO ()
main = do
  runResourceT $ readValue
  runResourceT $ saveValue "bar"
  forever $ getLine >>= process
          
  where randomAction = randomRIO (1, 3) >>= return . parseAction . Number
        process s = case parseAction (String s) of
          Quit -> putStrLn "exiting" >> exitWith ExitSuccess
          Invalid -> putStrLn "Unknown action. <rock|paper|scissors|quit>"
          c -> do r <- randomAction
                  case evaluate (c, r) of
                    Win  -> putStrLn $ "Your " ++ show c ++ " won against " ++ show r ++ "!"
                    Lose -> putStrLn $ "Your " ++ show c ++ " lost against " ++ show r ++ "."
                    Tie  -> putStrLn $ show c ++ " tied."
                    
saveValue :: MonadResource m => ByteString -> m ()
saveValue value = do
  db <- open "/tmp/zachtestldb" [CreateIfMissing, CacheSize 2048]
  put db [] "foo" value
  
readValue :: MonadResource m => m ()
readValue = do
  db <- open "/tmp/zachtestldb" [CreateIfMissing, CacheSize 2048]
  get db [] "foo" >>= liftIO . print
            
parseAction :: RawAction -> Action  
parseAction a = case formatted a of
  String ('r':_) -> Rock
  Number 1       -> Rock
  
  String ('p':_) -> Paper
  Number 2       -> Paper
  
  String ('s':_) -> Scissors
  Number 3       -> Scissors
  
  String "quit"  -> Quit
  _               -> Invalid
  
  where formatted (String s) = String $ map toLower s
        formatted x = x
        
evaluate :: (Action, Action) -> Result
evaluate (Rock, Scissors)  = Win
evaluate (Rock, Paper)     = Lose

evaluate (Scissors, Paper) = Win
evaluate (Scissors, Rock)  = Lose

evaluate (Paper, Rock)     = Win
evaluate (Paper, Scissors) = Lose
evaluate _                 = Tie
