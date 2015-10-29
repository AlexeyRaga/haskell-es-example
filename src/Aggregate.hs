{-# LANGUAGE TypeFamilies #-}

module Aggregate where

class Aggregate s where
    data Error s   :: *
    data Command s :: *
    data Event s   :: *

    execute :: s -> Command s -> Either (Error s) (Event s)
    apply :: s -> Event s -> s
    new :: s

handle :: (Aggregate a) => a -> Command a -> Either (Error a) ((a, Event a))
handle a cmd = (\e -> (apply a e, e)) <$> execute a cmd

load :: (Aggregate a) => [Event a] -> a
load = foldl apply new

validate :: (a -> Bool) -> e -> a -> Either e a
validate f err x
    | f x = Right x
    | otherwise = Left err

