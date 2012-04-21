{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Control.Monad
import System.Exit
import System.Random
import Database.LevelDB
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad.IO.Class (liftIO)

data RawAction = String String | Number Int

data Action = Rock | Paper | Scissors | Quit | Invalid
            deriving Show

data Result = Win | Lose | Tie

main :: IO ()
main = do forever $ getLine >>= process
    
process :: String -> IO ()
process input = case parseAction (String input) of
  Quit -> putStrLn "exiting" >> exitWith ExitSuccess
  Invalid -> putStrLn "Unknown action. <rock|paper|scissors|quit>"
  c -> do r <- randomAction
          case evaluate (c, r) of
            Win  -> putStrLn $ "Your " ++ show c ++ " won against " ++ show r ++ "!"
            Lose -> putStrLn $ "Your " ++ show c ++ " lost against " ++ show r ++ "."
            Tie  -> putStrLn $ show c ++ " tied."
  where randomAction = randomRIO (1, 3) >>= return . parseAction . Number

databasePath :: FilePath
databasePath = "./db/turns.ldb"

saveValue :: MonadResource m => Text -> m ()
saveValue value = do
  db <- open databasePath [CreateIfMissing, CacheSize 2048]
  put db [] (encodeUtf8 "foo") (encodeUtf8 value)

readValue :: MonadResource m => m ()
readValue = do
  db <- open databasePath [CreateIfMissing, CacheSize 2048]
  get db [] (encodeUtf8 "foo") >>= liftIO . print

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
