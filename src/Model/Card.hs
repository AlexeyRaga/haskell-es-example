{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}

module Model.Card (
  CardId (..), Card, CardItem, ProductId(..), Quantity,
  Error, Command(..), Event(..),
) where

import Aggregate
import Data.Aeson
import Data.Data
import Data.List
import Data.Text (Text)
import GHC.Generics

---------------------------- model ------------------------------------
newtype CardId    = CardId Text    deriving (Generic, Eq, Show, Data, ToJSON, FromJSON, Read)
newtype ProductId = ProductId Text deriving (Generic, Eq, Show, Data, ToJSON, FromJSON, Read)
type    Quantity  = Int

data Card =
  Card { cardId :: CardId, products :: [CardItem] }
  deriving (Show, Data)

data CardItem =
  CardItem { productId :: ProductId, quantity :: Quantity }
  deriving (Generic, Show, Data)

instance AggregateId CardId where
  textAggregateId (CardId cid) = cid

--------------------------- aggregate ---------------------------------
instance Aggregate Card where
  type Id Card = CardId

  data Command Card = AddProduct ProductId
                    | RemoveProduct ProductId
                    | ClearCard
                    deriving (Show, Read)

  data Event Card = ProductAdded { addedProductId :: ProductId }
                  | ProductRemoved { removedProductId :: ProductId }
                  | CardCleared
                  deriving (Generic, Show, Data, ToJSON, FromJSON)

  data Error Card = NoProductInCard ProductId
                  | QuantityExceedsLimit ProductId Quantity
                  deriving (Show)

  aggregateId = cardId

  s `execute` AddProduct p = ProductAdded
    <$> validate (withinLimit s 2) (QuantityExceedsLimit p 2) p

  s `execute` RemoveProduct p = ProductRemoved
    <$> validate (existsInCard s) (NoProductInCard p) p

  _ `execute` ClearCard = Right CardCleared

  s `apply` ProductAdded p = addProduct s p
  s `apply` ProductRemoved p = removeProduct s p
  s `apply` CardCleared = s { products = [] }

  new aid = Card aid []

-------------------------- validation ---------------------------------
existsInCard :: Card -> ProductId -> Bool
existsInCard c p = any (hasId p) (products c)

withinLimit :: Card -> Quantity -> ProductId -> Bool
withinLimit c num p =
  not $ any (\x -> productId x == p && quantity x >= num) (products c)

---------------------------- helpers ----------------------------------
updateQuantity :: (Int -> Int) -> CardItem -> CardItem
updateQuantity f item = item { quantity = max 0 (f $ quantity item) }

addProduct :: Card -> ProductId -> Card
addProduct c p =
  let newProducts = addOrUpdate (hasId p) (updateQuantity succ) (CardItem p 1) (products c)
  in c { products = newProducts }

removeProduct :: Card -> ProductId -> Card
removeProduct c p =
  let newProducts = updateWhere (hasId p) (updateQuantity pred) (products c)
  in c { products = filter ((>0) . quantity) newProducts }

hasId :: ProductId -> CardItem -> Bool
hasId p i = productId i == p

addOrUpdate :: (a -> Bool) -> (a -> a) -> a -> [a] -> [a]
addOrUpdate f update create lst =
  let (ok, nok) = partition f lst
  in if null ok
        then create : nok
        else (update <$> ok) ++ nok

updateWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
updateWhere f upd lst =
  (\x -> if f x then upd x else x) <$> lst
