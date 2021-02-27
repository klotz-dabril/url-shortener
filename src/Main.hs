--
-- Main.hs
--
--

{-# LANGUAGE OverloadedStrings #-}


module Main (main) where

import           Control.Concurrent.MVar
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Builder    as Builder
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map                   as Map
import           Database.SQLite.Simple
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp  (run)


import Extra
import RequestLogger
import RequestCounter
import Router
import Short




shortGetAction :: Action
shortGetAction urlParams request respond = do putStrLn $ "urlParams: " ++ show urlParams

                                              maybeUrl <- runMaybeT $ do idText      <- MaybeT . return $ Map.lookup "id" urlParams
                                                                         shortId     <- MaybeT . return $ textToIntegral idText
                                                                         Short _ url <- findShortById shortId

                                                                         return url

                                              case maybeUrl of Just url -> do putStrLn url
                                                                              respond $ responseLBS status200
                                                                                                    [("Content-Type", "text/plain")]
                                                                                                    (C.pack $ url)

                                                               Nothing  -> not_found_action request respond



rootPostAction :: Action
rootPostAction _ request respond = do maybeParams <- extractBodyParams request ["url"]

                                      case maybeParams of Just [url] -> do conn <- open "test.db"
                                                                           execute conn "INSERT INTO shorts (url) VALUES (?)" (Only url)
                                                                           rowId <- lastInsertRowId conn
                                                                           close conn

                                                                           respond $ responseLBS status200
                                                                                                 [("Content-Type", "text/plain")]
                                                                                                 (Builder.toLazyByteString $ Builder.int64Dec rowId)

                                                          _          -> not_found_action request respond



not_found_action :: Application
not_found_action _ respond = respond $ responseLBS status400
                                                   [("Content-Type", "text/plain")]
                                                   "not_found"



applicationRoutes :: [Route]
applicationRoutes = [ makeRoute methodGet  "/<id>" shortGetAction
                    , makeRoute methodPost "/"     rootPostAction
                    ]



application :: Application
application = router applicationRoutes not_found_action




main :: IO ()
main = do counter <- newMVar 0
          run 3000 . requestCounter counter
                   . routerMatchesLogger applicationRoutes
                   . requestLogger
                   $ application
