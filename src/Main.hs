{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.MVar
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)


import Route


-- import Debug.Trace
-- import Data.Typeable



increment :: MVar Int -> IO Int
increment counterVar = modifyMVar counterVar go
        where go count = do let count' = count + 1
                            return (count', count')



requestCounter :: MVar Int -> Middleware
requestCounter countRef app request respond = do c <- increment countRef
                                                 putStrLn $ "COUNTER: " ++ (show c)
                                                 app request respond



requestLogger :: Middleware
requestLogger app request respond = do putStrLn $ "LOGGER: " ++ (show request)
                                       app request respond




shortGetAction :: Action
shortGetAction _ _ respond = respond $ responseLBS status200
                                                   [("Content-Type", "text/plain")]
                                                   "shortGet"


rootPostAction :: Action
rootPostAction _ _ respond = respond $ responseLBS status200
                                                   [("Content-Type", "text/plain")]
                                                   "rootPost"


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
