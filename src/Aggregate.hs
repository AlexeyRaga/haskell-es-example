{-# LANGUAGE TypeFamilies  #-}

module Aggregate where

import Data.Text (Text)

class AggregateId aid where
    textAggregateId :: aid -> Text

class Aggregate s where
  type Id s      :: *
  data Error s   :: *
  data Command s :: *
  data Event s   :: *

  aggregateId :: s -> Id s

  execute :: s -> Command s -> Either (Error s) (Event s)
  apply :: s -> Event s -> s
  new :: Id s -> s

handle :: (Aggregate a) => a -> Command a -> Either (Error a) ((a, Event a))
handle a cmd = (\e -> (apply a e, e)) <$> execute a cmd

load :: (Aggregate a) => Id a -> [Event a] -> a
load aid = foldl apply (new aid)

validate :: (a -> Bool) -> e -> a -> Either e a
validate f err x
  | f x = Right x
  | otherwise = Left err

