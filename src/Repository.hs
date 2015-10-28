{-# LANGUAGE FlexibleContexts #-}
module Repository where

import Control.Monad
import Data.ByteString.Lazy (fromStrict)
import Database.EventStore
import Data.Aeson
import Data.Text hiding (head)
import Aggregate

readAllEvents :: (Aggregate a, FromJSON (Aggregate.Event a)) => Connection -> Text -> IO a
readAllEvents conn stream =
  readPortion new
  where
    readPortion a = do
      res <- readStreamEventsForward conn stream 0 500 False >>= wait
      let isEOS = streamEventsSliceIsEOS res
      case applyEvents a res of
        Just a' | isEOS -> return a'
        Just a'         -> readPortion a'
        Nothing         -> return a

    applyEvents a s = foldM decodeEvent a (streamEventsSliceEvents s)

    decodeEvent a e =
      apply a <$> (resolvedEventOriginal e >>= decode . fromStrict . recordedEventData)
