import Data.Char
import Control.Monad
import System.Exit

data Action = Rock | Paper | Scissors | Quit | Invalid

main :: IO ()
main = forever $ getLine >>= evaluateTurn
  where evaluateTurn s = case parseAction s of
          Rock -> putStrLn "stubborn as a rock"
          Paper -> putStrLn "uh paper?"
          Scissors -> putStrLn "dont cut yourself"
          
          Quit -> putStrLn "exiting" >> exitWith ExitSuccess
          Invalid -> putStrLn "Unknown action. <rock|paper|scissors|quit>"
    
parseAction :: String -> Action  
parseAction a = case a' of
  ('r':_) -> Rock
  ('p':_) -> Paper
  ('s':_) -> Scissors
  "quit"  -> Quit
  _       -> Invalid
  
  where a' = map toLower a