{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- TODO: nested lambdas don't work!
-- d <- flip a initCtx $ sApp [sApp [SLambda ["x"] $ SLambda ["y"] $ SVar "x", SLit 1], SLit 2]
-- probably fixed now??
--
-- try c <- flip a initCtx $ sApp [SVar "def", SQuot $ SVar "realFact", sApp [SVar "Y", SVar "alFact"]]

module Language.Aglet where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Map.Strict (Map, empty, fromList, insert, member, (!?))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), scanl, singleton, (><))
import Data.Sequence qualified as S (fromList)
import Data.Text (Text, intercalate, pack, unpack)
import Debug.Trace (trace, traceShow, traceShowId)
import Prelude hiding (scanl)

newtype Context = Context (Seq Frame) deriving (Show)

lookContext :: Context -> Ident -> Maybe Pneuma
lookContext (Context ctx) ident = let res = go ctx in trace ("looking up " <> unpack ident <> " in " <> show ctx <> " results in " <> show res) $ res
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
  | PQuot Soma
  | PFun Ident ([Pneuma] -> Context -> (Pneuma, Context))

instance Show Pneuma where
  show (PLit x) = "PLit " <> show x
  show (PNil) = "PNil"
  show (PFun name _) = "[function: " <> unpack name <> "]"

rebreathe :: Pneuma -> Context -> (Pneuma, Context)
rebreathe (PQuot s) ctx = breathe s ctx
rebreathe els ctx = (els, ctx)

breathe :: Soma -> Context -> (Pneuma, Context)
breathe (SLit x) ctx = (PLit x, ctx)
breathe SNil ctx = (PNil, ctx)
breathe (SVar var) ctx =
  (,ctx)
    . fromMaybe (trace ("LOOKUP FAILED OF VARIABLE " <> unpack var) $ PNil) -- correct signalling of exception
    $ lookContext ctx var
breathe (SQuot s) ctx = (PQuot s, ctx)
breathe (SLambda idents body) ctx@(Context c) =
  (,ctx) -- suspicious
    . PFun "lambda"
    $ \pneums (Context ctx') ->
      case breathe body
        . Context
        $ Frame (traceShowId $ fromList $ zip idents pneums)
          :<| (c >< ctx') of -- THIS MUST BE CTX, NOT CTX'!!!! -- should we really be appending the two contexts????
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
      _ -> trace "TRIED TO APPLY NON-FUNCTION" (PNil, ctx') -- should be `error`?
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
        ),
        ("-", PFun "builtin -" \[PLit x1, PLit x2] ctx -> (,ctx) . PLit $ x1 - x2),
        ( "*",
          PFun "builtin *" \args ctx ->
            traceShow args $ (,ctx) . PLit . product . map (\(PLit x) -> x) $ args
        ),
        ( "if",
          PFun "builtin if" \[bool, arm1, arm2] ctx -> flip rebreathe ctx case bool of
            PNil -> arm2
            _ -> arm1
        ),
        ( "=",
          PFun "builtin =" \args ctx ->
            (,ctx) $
              if let allEqual :: Eq a => [a] -> Bool
                     allEqual [] = True
                     allEqual (x : xs) = all (== x) xs
                  in allEqual (map (\(PLit x) -> x) args)
                then PLit 0
                else PNil
        ),
        ( "Y",
          evalInEmptyCtx $
            SLambda ["f"] $
              SApp $
                S.fromList
                  [ SLambda ["x"] $ SApp $ S.fromList [SVar "x", SVar "x"],
                    SLambda ["x"] $
                      SApp $
                        S.fromList
                          [ SVar "f",
                            SLambda ["y"] $
                              SApp $
                                S.fromList
                                  [ SApp $ S.fromList $ [SVar "x", SVar "x"],
                                    SVar "y"
                                  ]
                          ]
                  ]
        ),
        ( "def",
          PFun "builtin def" $ flip \case
            Context Empty -> const (PNil, Context Empty)
            Context (Frame skim :<| rest) -> \[PQuot (SVar x), defn] ->
              (PNil, Context $ Frame (insert x defn skim) :<| rest)
        ),
        ( "alFact",
          evalInEmptyCtx $
            SLambda ["f"] $
              SLambda ["n"] $
                sApp
                  [ SVar "if",
                    sApp [SVar "=", SVar "n", SLit 0],
                    SLit 1,
                    sApp
                      [ SVar "*",
                        SVar "n",
                        sApp
                          [ SVar "f",
                            sApp
                              [ SVar "-",
                                SVar "n",
                                SLit 1
                              ]
                          ]
                      ]
                  ]
        )
      ]

emptyCtx :: Context
emptyCtx = Context Empty

evalInEmptyCtx :: Soma -> Pneuma
evalInEmptyCtx s = fst $ breathe s emptyCtx

sApp = SApp . S.fromList
