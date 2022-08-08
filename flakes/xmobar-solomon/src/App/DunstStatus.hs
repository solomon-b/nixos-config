{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
module App.DunstStatus where

import App.Icons
import Control.Applicative
import Data.Maybe
import System.Process
import Text.ParserCombinators.ReadP
import Xmobar (Exec(..))

data Status = Active | Paused

instance Render Status where
  render Active = ""
  render Paused = render DoNotDisturb

parseStatus :: ReadP Status
parseStatus =
  let active = Active <$ string "false"
      paused = Paused <$ string "true"
  in (active <|> paused) <* skipSpaces <* eof

runParser :: String -> [(Status, String)]
runParser = readP_to_S parseStatus

readDunstProcess :: IO String
readDunstProcess = readCreateProcess (shell "dunstctl is-paused") ""

runDunstStatus :: IO String
runDunstStatus =  do
  res <- listToMaybe . runParser <$> readDunstProcess
  case fst <$> res of
    Just Active -> pure $ render Active
    Just Paused -> pure $ render Paused
    Nothing -> error "failed to capture Dunst status"

newtype DunstStatus = DunstStatus String
  deriving stock (Show, Read)

instance Exec DunstStatus where
  run :: DunstStatus -> IO String
  run _ = runDunstStatus

  alias :: DunstStatus -> String
  alias (DunstStatus a) = a
