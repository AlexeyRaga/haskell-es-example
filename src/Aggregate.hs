{-# LANGUAGE TypeFamilies #-}

module Aggregate where

data EventData e = EventData {
    eventId :: Int,
    body :: Event e
}

class Aggregate s where
    data Error s   :: *
    data Command s :: *
    data Event s   :: *

    execute :: s -> Command s -> Either (Error s) (Event s)
    apply :: s -> Event s -> s
    new :: s

handle :: (Aggregate a) => a -> Command a -> Either (Error a) ((a, Event a))
handle a cmd = (\e -> (apply a e, e)) <$> execute a cmd

load :: (Aggregate a) => [EventData a] -> a
load = foldl folder new
    where
        folder state = apply state . body

validate :: (a -> Bool) -> e -> a -> Either e a
validate f err x
    | f x = Right x
    | otherwise = Left err

