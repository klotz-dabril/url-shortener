--
-- Short.hs
--
--

{-# LANGUAGE OverloadedStrings #-}


module Short ( Short
             , getUrl
             , findShortById
             , createShort
             ) where


import Control.Monad.Trans.Maybe
import Data.Int
import Database.SQLite.Simple



data Short = Short { getId  :: Int
                   , getUrl :: String
                   } deriving (Show)


instance FromRow Short where
  fromRow = Short <$> field <*> field



findShortById :: Int -> MaybeT IO Short
findShortById shortId = MaybeT $ do conn   <- open "test.db"
                                    shorts <- query conn "SELECT * FROM shorts where id = ?" (Only shortId) :: IO [Short]
                                    close conn

                                    return $ case shorts of (short:_) -> Just short
                                                            _         -> Nothing



createShort :: String -> IO Int64
createShort url = do conn <- open "test.db"
                     execute conn "INSERT INTO shorts (url) VALUES (?)" (Only url)
                     rowId <- lastInsertRowId conn
                     close conn

                     return rowId
