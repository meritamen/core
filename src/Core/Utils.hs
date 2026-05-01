{-# LANGUAGE OverloadedStrings #-}

module Core.Utils
  ( Name
  , Addr
  , showAddr
  , NameSupply
  , initialNameSupply
  , getName
  , getNames
  , makeName
  )where

import Data.Text (Text)
import TextShow  (TextShow (..))

type Name = Text

type Addr = Int

showAddr :: Int -> Text
showAddr addr = "#" <> showt addr

type NameSupply = Int

initialNameSupply :: NameSupply
initialNameSupply = 0

getName :: (Num a, TextShow a) => a -> Text -> (a, Text)
getName name_supply prefix = (name_supply+1, makeName prefix name_supply)

getNames :: Int -> [Text] -> (Int, [Text])
getNames name_supply prefixes
 = (name_supply + length prefixes, zipWith makeName prefixes [name_supply..])

makeName :: TextShow a => Text -> a -> Text
makeName prefix ns = prefix <> "_" <> showt ns
