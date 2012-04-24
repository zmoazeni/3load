import Control.Monad
import System.Exit
import Database.LevelDB
import Control.Monad.IO.Class (liftIO)

import Actions
import Strategies

main :: IO ()
main = runResourceT $ do
  db <- open databasePath [CreateIfMissing, CacheSize 2048]
--  let strategy = randomStrategy
  let strategy = historyStrategy db
  forever $ processLine strategy db

  where processLine strategy db = do
          line <- liftIO $ getLine
          (report, processed) <- process strategy line
          liftIO $ putStrLn report
          case processed of
            PQuit     -> liftIO $ exitWith ExitSuccess
            PHelp     -> return ()
            PResult r -> do saveTurn db r
                            notify strategy r
                            reportRecord db

reportRecord :: MonadResource m => DB -> m ()
reportRecord db = do
  turns <- getTurns db
  let wins = foldr (\r xs -> case r of Win _ -> r:xs; _ -> xs) [] turns
      losses = foldr (\r xs -> case r of Lose _ -> r:xs; _ -> xs) [] turns
      ties = foldr (\r xs -> case r of Tie _ -> r:xs; _ -> xs) [] turns
  liftIO $ putStrLn ("W: " ++ show (length wins) ++ " L: " ++ show (length losses) ++ " T: " ++ show (length ties))
  return ()

process :: MonadResource m => Strategy -> String -> m (String, Processed)
process strategy input = case parseAction (String input) of
  Quit        -> return ("exiting", PQuit)
  Invalid     -> return ("Unknown action. <rock|paper|scissors|quit>", PHelp)
  userAction -> do aiAction <- choose strategy
                   let result = evaluate (userAction, aiAction)
                   return (report(result), PResult result)

  where report (Win (h, c))  = "Your " ++ show h ++ " won against " ++ show c ++ "!"
        report (Lose (h, c)) = "Your " ++ show h ++ " lost against " ++ show c ++ "."
        report (Tie (h, _))  = show h ++ " tied."

databasePath :: FilePath
databasePath = "./db/turns.ldb"

