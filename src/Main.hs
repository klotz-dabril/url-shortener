{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString          (ByteString)
import Data.Foldable            (find)
import Data.Maybe               (isJust)
import Control.Concurrent.MVar
import Control.Monad            (join)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

-- import Debug.Trace
-- import Data.Typeable



increment :: MVar Int -> IO Int
increment counterVar = modifyMVar counterVar go
        where go count = do let count' = count + 1
                            return (count', count')



request_counter :: MVar Int -> Middleware
request_counter countRef app request respond = do c <- increment countRef
                                                  putStrLn (show c)
                                                  app request respond



request_logger :: Middleware
request_logger app request respond = do putStrLn $ show request
                                        app request respond



data Route = Route { routeMethod   :: Method
                   , routeBasePath :: ByteString
                   , routeAction   :: Application
                   }



requestMatchesRoute :: Request -> Route -> Maybe Application
requestMatchesRoute request route | sameRoute && sameBase = Just action
                                  | otherwise             = Nothing
                                  where sameRoute = requestMethod request == routeMethod route
                                        sameBase  = rawPathInfo request   == routeBasePath route
                                        action    = routeAction route



rootGetAction :: Application
rootGetAction _ respond = respond $ responseLBS status200
                                                [("Content-Type", "text/plain")]
                                                "rootGet"



rootPostAction :: Application
rootPostAction _ respond = respond $ responseLBS status200
                                                 [("Content-Type", "text/plain")]
                                                 "rootPost"



applicationRoutes :: [Route]
applicationRoutes = [ Route methodGet  "/" rootGetAction
                    , Route methodPost "/" rootPostAction
                    ]



router :: [Route] -> Middleware
router routes = \app request respond -> let maybeAction = join
                                                        . find isJust
                                                        $ map (requestMatchesRoute request) routes

                                        in case maybeAction of Just action -> action request respond
                                                               Nothing     -> app request respond



not_found_action :: Application
not_found_action _ respond = respond $ responseLBS status400
                                                   [("Content-Type", "text/plain")]
                                                   "not_found"



application :: Application
application = router applicationRoutes not_found_action



main :: IO ()
main = do counter <- newMVar 0
          run 3000 . request_counter counter
                   . request_logger
                   $ application
