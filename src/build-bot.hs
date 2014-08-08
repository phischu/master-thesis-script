{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process (rawSystem)

import Data.Aeson (FromJSON(parseJSON),withObject,(.:),decode)
import qualified Data.ByteString.Lazy as ByteString (readFile)
import Control.Applicative ((<$>),(<*>))
import Control.Monad (forM_)

data Update = Update {
    packagename :: String,
    packageversion :: String,
    dependencyname1 :: String,
    dependencyversion1 :: String,
    dependencyname2 :: String,
    dependencyversion2 :: String,
    symbolchanged :: Bool,
    legal :: Bool
}

instance FromJSON Update where
    parseJSON = withObject "Update" (\o ->
        Update <$>
        o .: "packagename" <*>
        o .: "packageversion" <*>
        o .: "dependencyname1" <*>
        o .: "dependencyversion1" <*>
        o .: "dependencyname2" <*>
        o .: "dependencyversion2" <*>
        o .: "symbolchanged" <*>
        o .: "legal")

main :: IO ()
main = do
    Just updates <- ByteString.readFile "updates.json" >>= return . decode :: IO (Maybe [Update])
    forM_ updates (\update -> do
        rawSystem "cabal" ["sandbox","delete"]
        rawSystem "cabal" ["sandbox","init"]
        exitcode <- rawSystem "cabal" ["install",packagename update ++ "-" ++ packageversion update]
        print exitcode
        rawSystem "cabal" ["sandbox","delete"]
        return exitcode)


