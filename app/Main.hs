module Main where

import Aggregate
import Data.Text (pack)
import Database.EventStore as ES
import Model.Card
import Repository
import Text.Read

(|>) :: a -> (a -> b) -> b
infixl 5 |>; (|>) a f = f a

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
    go conn card = do
      line <- getLine
      case line of
        "exit"   -> putStrLn "Bye."
        "expose" -> (putStrLn $ show card) >> go conn card
        _        -> do
          nextCard <- case readMaybe line of
                        Just cmd -> exec conn card cmd
                        Nothing  -> return card
          go conn nextCard

exec :: Connection -> Card -> Command Card -> IO Card
exec conn card cmd =
  case handle card cmd of
    Left err           -> (putStrLn $ "--! " ++ show err) >> return card
    Right (newCard, e) -> do
      putStrLn $ "--> " ++ show e
      _ <- writeAllEvents conn (aggregateId card) [e]
      return newCard
