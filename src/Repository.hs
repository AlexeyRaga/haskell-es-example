{-# LANGUAGE FlexibleContexts #-}
module Repository where

import Control.Monad
import Data.ByteString.Lazy (fromStrict)
import Database.EventStore as ES
import Data.Aeson
import Data.Text hiding (head)
import Aggregate as A
import Data.Data

readAllEvents :: (Aggregate a, FromJSON (A.Event a), AggregateId (A.Id a)) =>
  Connection
  -> A.Id a
  -> IO a
readAllEvents conn aid  =
  readPortion (new aid)
  where
    stream = textAggregateId aid
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

writeAllEvents :: (Data (A.Event a), ToJSON (A.Event a), AggregateId (A.Id a)) =>
  Connection
  -> A.Id a
  -> [A.Event a]
  -> IO WriteResult
writeAllEvents conn aid es =
  let packEvent e = createEvent (pack . show $ toConstr e) Nothing (withJson $ toJSON e)
      stream = textAggregateId aid
  in  sendEvents conn stream anyStream (packEvent <$> es) >>= wait
