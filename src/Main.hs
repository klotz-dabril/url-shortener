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

import qualified Data.Map  as Map
import qualified Data.Text as Text
import Text.Regex.TDFA



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


type UrlParams = Map.Map String String
type Action    = UrlParams -> Application

data Route = Route { routeMethod    :: Method
                   , routePathSteps :: Text.Text
                   , routeAction    :: Action
                   }


data RouteStepMatch = RouteStepMatched
                    | RouteStepParam Text.Text Text.Text
                    deriving(Show)


matchRouteStep :: Text.Text -> Text.Text -> Maybe RouteStepMatch
matchRouteStep requestStep routeStep | isParamMatch       = Just $ RouteStepParam (Text.pack $ head xs) requestStep
                                     | isSimplerouteMatch = Just $ RouteStepMatched
                                     | otherwise          = Nothing
                                     where isParamMatch           = before == "" && length xs == 1 && after == "" :: Bool
                                           (before, _, after, xs) = (Text.unpack routeStep) =~ ("<(.*)>" :: String) :: (String, String, String, [String])
                                           isSimplerouteMatch     = simpleRouteMatch /= ""
                                           simpleRouteMatch       = (Text.unpack requestStep) =~ ("^" ++ (Text.unpack routeStep) ++ "$") :: String


matchRoute :: [Text.Text] -> [Text.Text] -> Maybe [RouteStepMatch]
matchRoute requestSteps routeSteps | nRequestSteps /= nRouteSteps = Nothing
                                   | isRoot                       = Just [RouteStepMatched]
                                   | otherwise = sequenceA $ zipWith matchRouteStep requestSteps routeSteps
                                   where nRequestSteps = length requestSteps
                                         nRouteSteps   = length routeSteps
                                         isRoot        = nRequestSteps == 0 && nRouteSteps == 0


actionFromRequestAndRoute :: Request -> Route -> Maybe Action
actionFromRequestAndRoute request route = do matchRoute requestPathSteps routeSteps
                                             return action

                                          where action           = routeAction route
                                                routeSteps       = getStepsFromRoute route
                                                requestPathSteps = pathInfo request



getStepsFromRoute :: Route -> [Text.Text]
getStepsFromRoute x = filter (/= "") $ Text.splitOn "/" $ routePathSteps x



routerMatchesLogger :: Middleware
routerMatchesLogger app request respond = do putStrLn "## ROUTER MATCHES ##"
                                             sequenceA . map go $ applicationRoutes
                                             putStrLn "#####################"
                                             app request respond

                                         where go route = putStrLn $ (show . routePathSteps $ route) ++ ": " ++ (show $ matchRoute (pathInfo request) (getStepsFromRoute route))



shortGetAction :: Action
shortGetAction _ _ respond = respond $ responseLBS status200
                                                   [("Content-Type", "text/plain")]
                                                   "shortGet"


rootPostAction :: Action
rootPostAction _ _ respond = respond $ responseLBS status200
                                                   [("Content-Type", "text/plain")]
                                                   "rootPost"



applicationRoutes :: [Route]
applicationRoutes = [ Route methodGet  "/<short>" shortGetAction
                    , Route methodPost "/"        rootPostAction
                    ]



router :: [Route] -> Middleware
router routes = \app request respond -> let maybeAction = join
                                                        . find isJust
                                                        $ map (actionFromRequestAndRoute request) routes

                                        in case maybeAction of Just action -> action Map.empty request respond
                                                               Nothing     -> app request respond



not_found_action :: Application
not_found_action _ respond = respond $ responseLBS status400
                                                   [("Content-Type", "text/plain")]
                                                   "not_found"



application :: Application
application = router applicationRoutes not_found_action



main :: IO ()
main = do counter <- newMVar 0
          run 3000 . requestCounter counter
                   . routerMatchesLogger
                   . requestLogger
                   $ application
