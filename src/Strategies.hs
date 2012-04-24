{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Strategies (
  randomStrategy
  ,historyStrategy
  ,getTurns
  ,saveTurn
  ,Strategy (..)
) where

import Database.LevelDB
import Control.Monad.IO.Class (liftIO)
import System.Random
import Data.Text.Encoding
import Data.List

import Conversions
import Actions

data Strategy = Strategy { choose :: MonadResource m => m Action,
                           notify :: MonadResource m => Result -> m () }

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
  case v of
    Just x  -> return (decode' x)
    Nothing -> return []

randomStrategy :: Strategy
randomStrategy = Strategy {choose=chooser, notify=notifier}
  where chooser :: MonadResource m => m Action
        chooser = liftIO $ randomRIO (1, 3) >>= return . parseAction . Number
        notifier _ = return ()

historyStrategy :: DB -> Strategy
historyStrategy db = Strategy {choose=chooser, notify=notifier}
  where chooser :: MonadResource m => m Action
        chooser = do
          (_, actions) <- findActionPattern
          case actions of
            [] -> do
              liftIO $ putStrLn "Couldn't find hisory"
              choose randomStrategy
            xs -> return . counterAction $ mostUsed xs

        notifier :: MonadResource m => Result -> m ()
        notifier result = do
          (pattern, actions) <- findActionPattern
          let (u, _) = turn result
          put db [] (encode' pattern) (encode' (u:actions))
          return ()
          
        findActionPattern :: MonadResource m => m ([Action], [Action])
        findActionPattern = do
          last4' <- last4
          let patterns = actionPatterns [] last4'
          actionGroups <- mapM previousActions patterns
          case filter (\(_, a) -> null a) actionGroups of
            []    -> return ([], [])
            (x:_) -> return x
          where actionPatterns :: [[Action]] -> [Action] -> [[Action]]
                actionPatterns acc [] = acc
                actionPatterns acc (x:xs) = actionPatterns ([x]:acc) xs

        previousActions :: MonadResource m => [Action] -> m ([Action], [Action])
        previousActions pattern = do
          v <- get db [] (encode' pattern)
          case v of
            Just x  -> return (pattern, (decode' x))
            Nothing -> return (pattern, [])

        last4 :: MonadResource m => m [Action]
        last4 = do
          results <- getTurns db
          let userActions = [u | r <- results, let (u, _) = turn r]
              last4' = take 4 userActions
          return last4'
          
        mostUsed :: [Action] -> Action
        mostUsed = head . head . sortLength . group . sort 
          where sortLength = sortBy (\a b -> length b `compare` length a)

