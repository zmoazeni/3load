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

data Strategy = Strategy { choose :: MonadResource m => [Result] -> m Action,
                           notify :: MonadResource m => [Result] -> Result -> m () }

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
  where chooser :: MonadResource m => [Result] -> m Action
        chooser _ = liftIO $ randomRIO (1, 3) >>= return . parseAction . Number
        notifier _ _ = return ()

historyStrategy :: DB -> Strategy
historyStrategy db = Strategy {choose=chooser, notify=notifier}
  where chooser :: MonadResource m => [Result] -> m Action
        chooser results = do
          patternActionGroups <- mapM previousActionsForPattern (patterns results)
          let (_, actions) = firstPatternAndAction patternActionGroups

          case actions of
            [] -> do
              liftIO $ putStrLn "Couldn't find hisory"
              choose randomStrategy results
            xs -> return . counterAction $ mostUsedAction xs

        notifier :: MonadResource m => [Result] -> Result -> m ()
        notifier results newResult = do
          patternActionGroups <- mapM previousActionsForPattern (patterns results)
          let (pattern, actions) = firstPatternAndAction patternActionGroups
              (currentUserAction, _) = turn newResult
          case pattern of
            [] -> case results of
              []    -> return ()
              (r:_) -> put db [] (encode' [fst $ turn r]) (encode' [currentUserAction]) >> return ()
            _  -> put db [] (encode' pattern) (encode' (currentUserAction:actions)) >> return ()

        previousActionsForPattern :: MonadResource m => [Action] -> m ([Action], [Action])
        previousActionsForPattern pattern = do
          v <- get db [] (encode' pattern)
          case v of
            Just x  -> return (pattern, (decode' x))
            Nothing -> return (pattern, [])

        patterns :: [Result] -> [[Action]]
        patterns = splitVariations . recentActionsForResults
          where splitVariations actions = splitVariations' [] actions
                splitVariations' acc [] = acc
                splitVariations' acc (x:xs) = splitVariations' ([x]:acc) xs

        firstPatternAndAction :: [([Action], [Action])] -> ([Action], [Action])
        firstPatternAndAction actionGroups = do
          case filter (\(_, a) -> not(null a)) actionGroups of
            []    -> ([], [])
            (x:_) -> x

        recentActionsForResults :: [Result] -> [Action]
        recentActionsForResults results = let userActions = [u | r <- results, let (u, _) = turn r]
                                          in take 4 userActions

        mostUsedAction :: [Action] -> Action
        mostUsedAction = head . head . sortLength . group . sort
          where sortLength = sortBy (\a b -> length b `compare` length a)

