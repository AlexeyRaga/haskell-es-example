module Main where

import Aggregate
import Data.Text (pack)
import Database.EventStore as ES
import Model.Card
import Repository
import Text.Read

cardId :: CardId
cardId = CardId $ pack "test-card-1"

main :: IO ()
main = do
  conn <- ES.connect defaultSettings "localhost" 1113
  card <- readAllEvents conn cardId
  putStrLn "Your current card is:"
  putStrLn $ show card
  putStrLn ""
  putStrLn "Enter your commands here:"
  go conn card

  where

    execLine conn card line =
      case readMaybe line of
        Just cmd -> execCmd conn card cmd
        Nothing  -> return card

    go conn card = do
      line <- getLine
      case line of
        "exit"   -> putStrLn "Bye."
        "expose" -> (putStrLn $ show card)  >>  go conn card
        _        -> execLine conn card line >>= go conn

execCmd :: Connection -> Card -> Command Card -> IO Card
execCmd conn card cmd =
  case handle card cmd of
    Left err           -> (putStrLn $ "--! " ++ show err) >> return card
    Right (newCard, e) -> do
      putStrLn $ "--> " ++ show e
      _ <- writeAllEvents conn (aggregateId card) [e]
      return newCard
