{-# LANGUAGE OverloadedStrings #-}

module Core.Utils (Name, Addr, showAddr) where

import Data.Text (Text)
import TextShow (showt)

type Name = Text

type Addr = Int                  
                                 
showAddr :: Int -> Text          
showAddr addr = "#" <> showt addr
