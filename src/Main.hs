--
-- Main.hs
--
--

{-# LANGUAGE OverloadedStrings #-}


module Main (main) where

import Control.Concurrent.MVar
import Database.SQLite.Simple
-- import Database.SQLite.Simple.FromRow
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp       (run)


import RequestLogger
import RequestCounter
import Router
import Extra          (extractBodyParams)




data Short = Short Int String String deriving (Show)

instance FromRow Short where
  fromRow = Short <$> field <*> field <*> field



shortGetAction :: Action
shortGetAction urlParams _ respond = do putStrLn $ "urlParams: " ++ show urlParams
                                        respond $ responseLBS status200
                                                              [("Content-Type", "text/plain")]
                                                              "shortGet"



-- extractBodyParams :: Request -> [LB.ByteString] -> IO (Maybe [LB.ByteString])
-- extractBodyParams request paramNames = let tupleFromStrict (x_0, x_1) = (LB.fromStrict x_0, LB.fromStrict x_1)
                                        -- in do (strictRequestParamsList, _) <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd request
                                              -- let lazyRequestParamList = tupleFromStrict <$> strictRequestParamsList
                                                  -- requestParams = Map.fromList lazyRequestParamList

                                              -- return . for paramNames $ (\k -> Map.lookup k requestParams)



rootPostAction :: Action
rootPostAction _ request respond = do maybeParams <- extractBodyParams request ["url"]

                                      case maybeParams of Just [url] -> -- conn <- open "test.db"
                                                                        -- execute conn "INSERT INTO shorts (full) VALUES (?)" (Only ("atest string 2 3" :: String))
                                                                        -- r <- query_ conn "SELECT * from shorts" :: IO [Short]
                                                                        -- mapM_ print r
                                                                        -- close conn

                                                                        respond $ responseLBS status200
                                                                                              [("Content-Type", "text/plain")]
                                                                                              url

                                                          _          -> not_found_action request respond


not_found_action :: Application
not_found_action _ respond = respond $ responseLBS status400
                                                   [("Content-Type", "text/plain")]
                                                   "not_found"



applicationRoutes :: [Route]
applicationRoutes = [ makeRoute methodGet  "/<short>" shortGetAction
                    , makeRoute methodPost "/"        rootPostAction
                    ]



application :: Application
application = router applicationRoutes not_found_action


main :: IO ()
main = do counter <- newMVar 0
          run 3000 . requestCounter counter
                   . routerMatchesLogger applicationRoutes
                   . requestLogger
                   $ application
