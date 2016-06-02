{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Data.List (sortBy)
import Data.Monoid
import Data.String

import Data.ByteString (ByteString)
import Data.MultiSet (MultiSet, union, intersection, size)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Options.Applicative

import qualified Data.MultiSet as MultiSet
import qualified Data.Text as Text

data Arguments = Arguments {
    dbConnString :: ByteString,
    nGramOrder   :: Int,
    threshold    :: Double,
    schemaName   :: Text,
    searchTerm   :: Text
}

txtOption :: IsString a => Mod OptionFields String -> Parser a
txtOption = fmap fromString . strOption

txtArgument :: IsString a => Mod ArgumentFields String -> Parser a
txtArgument = fmap fromString . strArgument

parseArguments :: Parser Arguments
parseArguments = Arguments
    <$> txtOption (long "db"
                <> metavar "DB"
                <> help "Postgres database connection string")
    <*> option auto (long "ngramorder"
                  <> metavar "NGRAMORDER"
                  <> help "The order of the ngrams used in the search"
                  <> value 2)
    <*> option auto (long "threshold"
                  <> metavar "THRESHOLD"
                  <> help "The minimum threshold score for matches"
                  <> value 0.3)
    <*> txtOption (long "schema"
                <> metavar "SCHEMA"
                <> help "The postgres schema(s) to search through (includes wildcards using %)"
                <> value "%")
    <*> txtArgument (metavar "SEARCHTERM")

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArguments) (fullDesc)


-- | Extracts the set of n order n-grams from the input Text string in the form
-- of a MultiSet (essentially a map). We need to account for terminal cases like
-- 'lolololololol' in the string similarity measure, so we need the counts for
-- each n-gram in the set.
ngrams :: Int -> Text -> MultiSet Text
ngrams n string = go MultiSet.empty padded len
    where canonical     = Text.toLower string
          padded        = Text.center len ' ' canonical
          len           = Text.length canonical + 2*(n-1)
          go set xs cnt = if cnt < n then set else go set' (Text.tail xs) (cnt-1)
              where set' = MultiSet.insert (Text.take n xs) set


-- | Calculates the Jaccard index for two sets of n-grams. Note that the union
-- size is halved as the MultiSet union function adds singleton counts together.
jaccardIndex :: MultiSet Text -> MultiSet Text -> Double
jaccardIndex searchSet searchSet' = magIntersection / magUnion
    where magIntersection =  fromIntegral $ size $ intersection searchSet searchSet'
          magUnion        = (fromIntegral $ size $ union        searchSet searchSet') / 2 -- MultiSet


-- | Given a function for decomposing string into a MultiSet (eg. ngrams 2) and
-- search set to compare it to, prepends the resulting Jaccard index to a given
-- meta-data row
rowDistance :: (Text -> MultiSet Text) ->  MultiSet Text -> (Text, Text, Text) -> (Double, Text, Text, Text)
rowDistance fngrams searchSet (attribute, table, schema) =
    (jaccardIndex searchSet (fngrams attribute), attribute, table, schema)


-- | Given a function for decomposing string into a MultiSet (eg. ngrams 2), a
-- minimum cutoff threshold and an attribute search term, ranks and sorts the
-- input list of meta-data rows by the Jaccard index distance between the
-- search term and each row.
rankAttributes :: (Text -> MultiSet Text) -> Double -> Text -> [(Text, Text, Text)] -> [(Double, Text, Text, Text)]
rankAttributes fngrams thresh searchAttribute =
      (takeWhile (\(a, _, _, _) -> a >= thresh))
    . (sortBy (flip compare))
    . (fmap (rowDistance fngrams searchSet))
    where searchSet = fngrams searchAttribute


main :: IO ()
main = do
    args <- execParser argParser

    let metaQuery = [sql| SELECT attr.attname,
                                 class.relname,
                                 namespace.nspname
                            FROM pg_attribute AS attr
                            JOIN pg_class AS class
                              ON attr.attrelid = class.oid
                            JOIN pg_namespace AS namespace
                              ON class.relnamespace = namespace.oid
                           WHERE attr.attnum > 0    -- exclude system columns
                             AND class.reltype > 0  -- exclude indexes
                             AND namespace.nspname LIKE ? |]

    conn <- connectPostgreSQL $ dbConnString args
    metaData <- query conn metaQuery [schemaName args]

    putStrLn $ unlines $ fmap show $
        rankAttributes (ngrams $ nGramOrder args) (threshold args) (searchTerm args) metaData

