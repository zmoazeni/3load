{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Control.Monad
import System.Exit
import System.Random
import Database.LevelDB
import Data.Text.Encoding
import Control.Monad.IO.Class (liftIO)
import Conversions
import qualified Data.Binary as Bin (get, put)
import Data.Binary (Binary, Get)
import Data.Maybe

data RawAction = String String | Number Int

data Action = Rock | Paper | Scissors | Quit | Invalid
            deriving Show

type Turn = (Action, Action)

data Result = Win {turn :: Turn} | Lose {turn :: Turn} | Tie {turn :: Turn}
            deriving Show
                     
data Processed = PResult Result | PQuit | PHelp                     

instance Binary Action where
  put a = Bin.put (intForAction a)
    where intForAction :: Action -> Int
          intForAction Rock     = 1
          intForAction Paper    = 2
          intForAction Scissors = 3
          intForAction _        = -1
          
  get = do a <- liftM (Number) $ (Bin.get :: Get Int)
           return (parseAction a)

instance Binary Result where
  put r = do Bin.put intForResult
             Bin.put (turn r)
                 
    where intForResult :: Int
          intForResult = case r of
            Win _  -> 0
            Lose _ -> 1
            Tie _  -> 2
            
  get = do i <- Bin.get :: Get Int
           t <- Bin.get :: Get Turn
           return $ case i of
             0 -> Win t
             1 -> Lose t
             2 -> Tie t
             e -> error $ "Unknown result " ++ (show e)

main :: IO ()             
main = runResourceT $ do
  db <- open databasePath [CreateIfMissing, CacheSize 2048]
  forever $ processLine db
  
  where processLine db = do 
          line <- liftIO $ getLine
          (report, processed) <- process line
          liftIO $ putStrLn report
          case processed of
            PQuit     -> liftIO $ exitWith ExitSuccess
            PHelp     -> return ()
            PResult r -> do saveTurn db r
                            turns <- getTurns db
                            liftIO $ print turns
    
process :: MonadResource m => String -> m (String, Processed)
process input = case parseAction (String input) of
  Quit    -> return ("exiting", PQuit)
  Invalid -> return ("Unknown action. <rock|paper|scissors|quit>", PHelp)
  c       -> do r <- randomAction
                let result = evaluate (c, r)
                return (report(result), PResult result)
          
  where randomAction = liftIO $ randomRIO (1, 3) >>= return . parseAction . Number
        report (Win (h, c))  = "Your " ++ show h ++ " won against " ++ show c ++ "!"
        report (Lose (h, c)) = "Your " ++ show h ++ " lost against " ++ show c ++ "."
        report (Tie (h, _))  = show h ++ " tied."

databasePath :: FilePath
databasePath = "./db/turns.ldb"

saveTurn :: MonadResource m => DB -> Result -> m ()
saveTurn db result = do
  v <- get db [] (encodeUtf8 "results")
  let results = case v of
        Just x -> result:(decode' x :: [Result])
        Nothing -> [result]
                  
  put db [] (encodeUtf8 "results") (encode' results)

getTurns :: MonadResource m => DB -> m [Result]
getTurns db = do
  v <- get db [] (encodeUtf8 "results")
  return $ decode' (fromJust v)

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
evaluate a@(Rock, Scissors)  = Win a
evaluate a@(Rock, Paper)     = Lose a

evaluate a@(Scissors, Paper) = Win a
evaluate a@(Scissors, Rock)  = Lose a

evaluate a@(Paper, Rock)     = Win a
evaluate a@(Paper, Scissors) = Lose a
evaluate a                   = Tie a
