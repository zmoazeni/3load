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
          last2' <- last2
          actions <- previousActions last2'
          case actions of
            [] -> do
              liftIO $ putStrLn "Couldn't find hisory"
              choose randomStrategy
            xs -> return . counterAction $ mostUsed xs

        notifier :: MonadResource m => Result -> m ()
        notifier result = do
          last2' <- last2
          actions <- previousActions last2'
          let (u, _) = turn result
          put db [] (encode' last2') (encode' (u:actions))
          return ()

        previousActions history = do
          v <- get db [] (encode' history)
          case v of
            Just x -> return (decode' x)
            Nothing -> return []

        last2 :: MonadResource m => m [Action]
        last2 = do
          results <- getTurns db
          let userActions = [u | r <- results, let (u, _) = turn r]
              last2' = take 2 userActions
          return last2'
          
        mostUsed :: [Action] -> Action
        mostUsed = head . head . sortLength . group . sort 
          where sortLength = sortBy (\a b -> length b `compare` length a)

