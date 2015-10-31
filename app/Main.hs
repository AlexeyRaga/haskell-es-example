module Main where

import           Aggregate
import           Data.Text (pack)
import           Database.EventStore as ES
import           Model.Card
import           Text.Read

(|>) :: a -> (a -> b) -> b
infixl 5 |>; (|>) a f = f a

cardId :: CardId
cardId = CardId $ pack "test-card-1"

emptyCard :: Card
emptyCard = new cardId :: Card

main :: IO ()
main = do
  --conn <- ES.connect defaultSettings "localhost" 1113
  print "Enter your commands here:"
  go emptyCard
  where
    go :: Card -> IO ()
    go card = do
      line <- getLine
      if line == "exit" then print "Bye." 
      else do
        nextCard <- case readMaybe line of
                      Just cmd -> exec card cmd
                      Nothing  -> return card
        go nextCard

exec :: Card -> Command Card -> IO Card
exec card cmd =
  case handle card cmd of
    Left err -> const card <$> (print $ "Error: " ++ show err)
    Right (c, e) -> const c <$> (print $ show e)
