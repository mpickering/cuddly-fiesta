{-# OPTIONS_GHC -fno-worker-wrapper #-}
-- | Postgres SQL DML
--
-- Provide types and combinators for defining Postgres SQL queries and mutations.
module Repro
where

import Data.String (fromString)
import Data.Text
--import Hasura.SQL.Types (ToSQL(..))
import Prelude
import Text.Builder qualified as TB

newtype FromExp
  = FromExp [FromItem]

instance (ToSQL a) => ToSQL (Maybe a) where
  toSQL (Just a) = toSQL a
  toSQL Nothing = mempty

instance ToSQL FromExp where
  toSQL (FromExp items) =
    "FROM" <> (", " <+> items)

class ToSQL a where
  toSQL :: a -> TB.Builder


newtype WhereFrag = WhereFrag {getWFBoolExp :: ()}

instance ToSQL WhereFrag where
  toSQL (WhereFrag be) =
    "WHERE"

data SQLExp
  = SEPrep Int


instance ToSQL SQLExp where
  toSQL (SEPrep argNumber) =
    fromString (show argNumber)

-- | Extractor can be used to apply Postgres alias to a column
data Extractor = Extractor SQLExp

instance ToSQL Extractor where
  toSQL (Extractor ce ) = undefined

data FromItem

instance ToSQL FromItem

data SQLUpdate = SQLUpdate
  { upTable :: (),
    upSet :: SetExp,
    upFrom :: Maybe FromExp,
    upWhere :: Maybe WhereFrag,
    upRet :: Maybe RetExp
  }

newtype SetExp = SetExp [SetExpItem]


newtype SetExpItem = SetExpItem (SQLExp)


newtype RetExp = RetExp [Extractor]

instance ToSQL RetExp where
  toSQL (RetExp exps) =
    "RETURNING" <> (", " <+> exps)


instance ToSQL SQLUpdate where
  toSQL a =
    "UPDATE"
      <> toSQL (upSet a)
      <> toSQL (upFrom a)
      <> toSQL (upWhere a)
      <> toSQL (upRet a)

instance ToSQL SetExp where
  toSQL (SetExp cvs) =
    "SET" <> ("," <+> cvs)

instance ToSQL SetExpItem where
  toSQL (SetExpItem (val)) =
    toSQL val

infixr 6 <+>

(<+>) :: (ToSQL a) => Text -> [a] -> TB.Builder
(<+>) _ [] = mempty
(<+>) kat (x : xs) =
  toSQL x <> mconcat [toSQL x' | x' <- xs]
{-# INLINE (<+>) #-}
