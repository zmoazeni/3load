module Actions (
  evaluate
  ,parseAction
  ,counterAction
  ,RawAction (..)
  ,Action (..)
  ,Result (..)
  ,Processed (..)
) where

import Data.Binary (Binary, Get, get, put)
import Control.Monad
import Data.Char

data RawAction = String String | Number Int

data Action = Rock | Paper | Scissors | Quit | Invalid
            deriving (Show, Ord, Eq)

type Turn = (Action, Action)

data Result = Win {turn :: Turn} | Lose {turn :: Turn} | Tie {turn :: Turn}
            deriving Show

data Processed = PResult Result | PQuit | PHelp

instance Binary Action where
  put a = put (intForAction a)
    where intForAction :: Action -> Int
          intForAction Rock     = 1
          intForAction Paper    = 2
          intForAction Scissors = 3
          intForAction _        = -1

  get = do a <- liftM (Number) $ (get :: Get Int)
           return (parseAction a)

instance Binary Result where
  put r = do put intForResult
             put (turn r)

    where intForResult :: Int
          intForResult = case r of
            Win _  -> 0
            Lose _ -> 1
            Tie _  -> 2

  get = do i <- get :: Get Int
           t <- get :: Get Turn
           return $ case i of
             0 -> Win t
             1 -> Lose t
             2 -> Tie t
             e -> error $ "Unknown result " ++ (show e)

counterAction :: Action -> Action
counterAction a = case a of
  Rock     -> Paper
  Paper    -> Scissors
  _        -> Rock

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
