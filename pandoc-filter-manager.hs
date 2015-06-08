--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk
import           Control.Arrow
  ( (&&&) )
import           Control.Monad
  ( foldM )
import           Data.Aeson
import           Data.ByteString.Lazy
  ( ByteString )
import qualified Data.ByteString.Lazy as BL
import qualified HSH
import           System.Environment
  ( getArgs )

--------------------------------------------------------------------------------

main :: IO ()
main
    = BL.getContents
  >>= runFilters
    . ( ( getFilters . either error id . eitherDecode' ) &&& id )
  >>= BL.putStr

runFilters :: ([String], ByteString) -> IO ByteString
runFilters (filters, doc)
    = foldM run doc filters
  where
    run :: ByteString -> String -> IO ByteString
    run doc' f = do
        args <- getArgs
        HSH.run $ (const doc' :: () -> ByteString) HSH.-|- (f, args)

getFilters :: Pandoc -> [String]
getFilters (Pandoc m _)
    = maybe [] (query getStr)
    $ lookupMeta "filters" m
  where
    getStr (Str x) = [x]
    getStr _       = []

--------------------------------------------------------------------------------
