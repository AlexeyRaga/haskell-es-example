{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}

module Main where

--import Lib
import           Control.Monad
import           Data.Data
import           Data.Aeson
import           Data.Text hiding (head)
import           Database.EventStore as ES
import           GHC.Generics
import           Streaming (Stream, Of)
import qualified Streaming as S
import qualified Streaming.Prelude as S

(|>) :: a -> (a -> b) -> b
infixl 5 |>; (|>) a f = f a

newtype IP = IP Text deriving (Eq, Show, Data)
newtype DomainName = DomainName Text deriving (Eq, Show, Data)

type StreamName = Text

data IpEvent
    = IpResolved { ip :: IP, domain :: DomainName}
    deriving (Generic, Show, Data)

instance FromJSON IP where
    parseJSON (String value) = return $ IP value
    parseJSON _ = mzero

instance FromJSON DomainName where
    parseJSON (String value) = return $ DomainName value
    parseJSON _ = mzero

instance ToJSON IP where
    toJSON (IP addr) = String addr

instance ToJSON DomainName where
    toJSON (DomainName name) = String name

instance ToJSON IpEvent

main :: IO ()
main = do
    conn <- ES.connect defaultSettings "localhost" 1113
    S.stdinLn
      |> S.map (makeEvent . pack)
      |> chunksBy 2000 (\e1 e2 -> (ip e1) == (ip e2))
      |> S.mapsM S.toList
      |> S.mapM_ (saveEvents conn)

saveEvents :: Connection -> [IpEvent] -> IO WriteResult
saveEvents conn es =
    let IpResolved (IP addr) _ = head es
        stream = append "ip-" addr
        events = packEvent <$> es
    in sendEvents conn stream anyStream events >>= wait

packEvent :: (Data a, ToJSON a) => a -> Event
packEvent e =
    createEvent (pack . show $ toConstr e) Nothing (withJson $ toJSON e)

makeEvent :: Text -> IpEvent
makeEvent s =
    let (_:addr:name:_) = splitOn "|" s
    in IpResolved (IP addr) (DomainName name)

testEvt :: IpEvent
testEvt = makeEvent "1|0.0.0.1|localtest.me"

chunksBy :: Monad m => Int -> (a -> a -> Bool) -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
chunksBy num f s = s
  |> S.zip (S.iterate (+1) 0)
  |> S.groupBy (\(_, a1) (i, a2) -> i `mod` num /= 0 && f a1 a2)
  |> S.maps (S.map snd)
