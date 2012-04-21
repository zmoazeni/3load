import Data.Char
import Control.Monad
import System.Exit
import System.Random

data Action = Rock | Paper | Scissors | Quit | Invalid
            deriving Show
                     
data Result = Win | Lose | Tie

main :: IO ()
main = forever $ getLine >>= process
  where randomAction = (randomRIO (1, 3) :: IO Int) >>= return . parseAction . show
        process s = case parseAction s of
          Quit -> putStrLn "exiting" >> exitWith ExitSuccess
          Invalid -> putStrLn "Unknown action. <rock|paper|scissors|quit>"
          c -> do r <- randomAction
                  case evaluate (c, r) of
                    Win  -> putStrLn $ "Your " ++ show c ++ " won against " ++ show r ++ "!"
                    Lose -> putStrLn $ "Your " ++ show c ++ " lost against " ++ show r ++ "."
                    Tie  -> putStrLn $ show c ++ " tied."
            
parseAction :: String -> Action  
parseAction a = case a' of
  ('r':_) -> Rock
  "1"     -> Rock
  
  ('p':_) -> Paper
  "2"     -> Paper
  
  ('s':_) -> Scissors
  "3"     -> Scissors
  
  "quit"  -> Quit
  _       -> Invalid
  
  where a' = map toLower a
        
evaluate :: (Action, Action) -> Result
evaluate (Rock, Scissors)  = Win
evaluate (Rock, Paper)     = Lose

evaluate (Scissors, Paper) = Win
evaluate (Scissors, Rock)  = Lose

evaluate (Paper, Rock)     = Win
evaluate (Paper, Scissors) = Lose
evaluate _                   = Tie

        