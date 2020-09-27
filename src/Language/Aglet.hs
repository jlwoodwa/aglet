{-# LANGUAGE LambdaCase, EmptyCase #-}

module Language.Aglet where

import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)

newtype Context = Context (Seq Frame)

newtype Frame = Frame (Map Text Ag)

data Ag

data Cedar a = Twig a | Bough (Seq (Cedar a))

-- I saw the silver trees burning from the twigs down, and when it was over
-- only a small pool of molten silver was left where the trees had once stood.
burn :: Cedar Ag -> Ag
burn = \case
