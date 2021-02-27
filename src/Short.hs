--
-- Short.hs
--
--

{-# LANGUAGE OverloadedStrings #-}


module Short (Short (Short), findShortById) where



import Control.Monad.Trans.Maybe
import Database.SQLite.Simple







data Short = Short Int String deriving (Show)

instance FromRow Short where
  fromRow = Short <$> field <*> field



findShortById :: Int -> MaybeT IO Short
findShortById shortId = MaybeT $ do conn   <- open "test.db"
                                    shorts <- query conn "SELECT * FROM shorts where id = ?" (Only shortId) :: IO [Short]
                                    close conn

                                    return $ case shorts of (short:_) -> Just short
                                                            _         -> Nothing
