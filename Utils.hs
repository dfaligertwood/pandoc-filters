--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------

module Utils
  ( addMeta
  , setMetaWith
  , unionMeta
  , unionMetaInline
  , latexInline
  , latexBlock
  , toInline
  ) where

import           Text.Pandoc.Builder
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- This function extends the definition of 'adding' to MetaValues by using the
-- relevant adding function in the types MetaValues are based around.

addMeta :: MetaValue -> MetaValue -> MetaValue
addMeta (MetaMap a)     (MetaMap b)     = MetaMap     (M.unionWith addMeta a b)
addMeta (MetaList a)    (MetaList b)    = MetaList    (a ++ b)
addMeta (MetaBool a)    (MetaBool b)    = MetaBool    (a && b)
addMeta (MetaString a)  (MetaString b)  = MetaString  (a ++ b)
addMeta (MetaInlines a) (MetaInlines b) = MetaInlines (a ++ b)
addMeta (MetaBlocks a)  (MetaBlocks b)  = MetaBlocks  (a ++ b)

--------------------------------------------------------------------------------
-- Equivalent of Text.Pandoc.Builder.setMeta, but adds a function to permit the
-- merging of two values with equivalent keys.

class SetMeta a where
  setMetaWith :: (ToMetaValue b) => (b -> MetaValue -> b) -> String -> b -> a -> a

  unionMeta :: ToMetaValue b => String -> b -> a -> a
  unionMeta k v m = setMetaWith addMeta k (toMetaValue v) m

  unionMetaInline :: String -> Inline -> a -> a
  unionMetaInline k v m = unionMeta k (MetaInlines [v]) m

instance SetMeta Pandoc where
  setMetaWith f k v (Pandoc m b) = Pandoc (setMetaWith f k v m) b

instance SetMeta Meta where
  setMetaWith f k v m
    = setMeta k (maybe v (f v) (lookupMeta k m)) m

--------------------------------------------------------------------------------
-- builders for LaTeX inlines and blocks

latexInline :: String -> Inline
latexInline = RawInline "latex"
latexBlock :: String -> Block
latexBlock = RawBlock "latex"

--------------------------------------------------------------------------------
-- convert block to array of inlines

toInline :: Block -> [Inline]
toInline (Plain is)       = is ++ doubleBreak
toInline (Para is)        = is ++ doubleBreak
toInline (CodeBlock a s)  = [Code a s]
toInline (RawBlock f s)   = [RawInline f s]
toInline (BlockQuote bs)  = [Quoted SingleQuote (concatMap toInline bs)]
toInline (Header _ _ is)  = is ++ doubleBreak
toInline (Div a bs)       = [Span a (concatMap toInline bs)]
toInline _                = [Str ""]

doubleBreak :: [Inline]
doubleBreak = LineBreak : [LineBreak]
