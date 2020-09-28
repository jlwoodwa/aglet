{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Aglet where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Map.Strict (Map, empty, fromList, insert, member, (!?))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), scanl, singleton)
import Data.Text (Text, intercalate, pack, unpack)
import Prelude hiding (scanl)

newtype Context = Context (Seq Frame) deriving (Show)

lookContext :: Context -> Ident -> Maybe Pneuma
lookContext (Context ctx) ident = go ctx
  where
    go :: Seq Frame -> Maybe Pneuma
    go Empty = Nothing
    go (frame :<| rest) = lookFrame frame ident <|> go rest

---- TODO: builtin this and mutContext
-- addContext :: Context -> Ident -> Pneuma -> Context
-- addContext em@(Context Empty) _ _ = em
-- addContext (Context (frame :<| rest)) var pne =
--   Context $ insertFrame frame var pne :<| rest
--
-- mutContext :: Context -> Ident -> Pneuma -> Context
-- mutContext (Context ctx) var pne =
--   Context $ go ctx
--   where
--     go :: Seq Frame -> Seq Frame
--     go Empty = Empty
--     go (tip@(Frame frame) :<| rest) =
--       if member var frame
--         then Frame (insert var pne frame) :<| rest
--         else tip :<| go rest

newtype Frame = Frame (Map Ident Pneuma) deriving (Show)

lookFrame :: Frame -> Ident -> Maybe Pneuma
lookFrame (Frame frame) ident = frame !? ident

insertFrame :: Frame -> Ident -> Pneuma -> Frame
insertFrame (Frame frame) ident pne = Frame $ insert ident pne frame

type Ident = Text

data Soma
  = SLit Int
  | SNil
  | SVar Ident
  | SQuot Soma
  | SLambda [Ident] Soma
  | SApp (Seq Soma)
  deriving (Show)

data Pneuma
  = PLit Int
  | PNil
  | PSymb Ident
  | PQuot Soma
  | PFun Ident ([Pneuma] -> Context -> (Pneuma, Context))

instance Show Pneuma where
  show (PLit x) = "PLit " <> show x
  show (PNil) = "PNil"
  show (PFun name _) = "[function: " <> unpack name <> "]"

breathe :: Soma -> Context -> (Pneuma, Context)
breathe (SLit x) ctx = (PLit x, ctx)
breathe SNil ctx = (PNil, ctx)
breathe (SVar var) ctx =
  (,ctx)
    . fromMaybe PNil -- correct signalling of exception
    $ lookContext ctx var
breathe (SQuot s) ctx = (PQuot s, ctx)
breathe (SLambda idents body) ctx =
  (,ctx) -- suspicious
    . PFun "lambda"
    $ \pneums (Context ctx') ->
      case breathe body
        . Context
        $ Frame (fromList $ zip idents pneums)
          :<| ctx' of
        b@(_, Context Empty) -> b
        (pne, Context (trash :<| old)) -> (pne, Context old)
breathe (SApp Empty) ctx = (PNil, ctx) -- should be `error`?
breathe (SApp somas) ctx =
  case ($ ctx)
    . foldr dyeLift (Empty,)
    . fmap breathe
    $ somas of
    (Empty, _) -> error "Guard above prevents this case"
    (f :<| xs, ctx') -> case f of
      PFun _ fun -> fun (toList xs) ctx'
      _ -> (PNil, ctx') -- should be `error`?
  where
    dyeLift comp below c =
      let (pne, c') = comp c
       in let (pnes, c'') = below c
           in (pne :<| pnes, c'')

--- debugging ---
a :: Soma -> Context -> IO Context
a s c =
  let (res, c') = breathe s c
   in do
        print res
        return c'

initCtx :: Context
initCtx =
  Context . singleton . Frame $
    fromList
      [ ( "+",
          PFun "builtin +" \args ctx ->
            (,ctx) . PLit . sum . map (\(PLit x) -> x) $ args
        )
      ]
