--
-- Route.hs
--
--

{-# LANGUAGE OverloadedStrings #-}


module Router ( router
              , makeRoute
              , Route
              , Action
              , UrlParams
              , routerMatchesLogger
        ) where


import Data.Foldable            (find)
import Data.Maybe               (isJust)
import Data.Traversable         (for)
import Control.Monad            (join)
import Network.HTTP.Types
import Network.Wai

import qualified Data.Map  as Map
import qualified Data.Text as Text
import Text.Regex.TDFA



type UrlParams = Map.Map Text.Text Text.Text
type Action    = UrlParams -> Application


data Route = Route { routeMethod :: Method
                   , routeSteps  :: [Text.Text]
                   , routeAction :: Action
                   }



makeRoute :: Method -> Text.Text -> Action -> Route
makeRoute method endpoint = let steps = filter (/= "") $ Text.splitOn "/" endpoint
                             in Route method steps



data RouteStepMatch = RouteStepMatched
                    | RouteStepParam Text.Text Text.Text
                    deriving(Show)



maybeAppFromRequestAndRoute :: Request -> Route -> Maybe Application
maybeAppFromRequestAndRoute request route = do routeStepMatches <- matchRouteWithRequest route request

                                               let maybeKeyValuesList = map extractMaybeKeyValue routeStepMatches
                                                   justKeyValueList   = filter isJust maybeKeyValuesList

                                               keyValueList <- sequenceA justKeyValueList

                                               let urlParams = Map.fromList keyValueList

                                               return . routeAction route $ urlParams

                                               where extractMaybeKeyValue (RouteStepParam key value) = Just (key,value)
                                                     extractMaybeKeyValue _                          = Nothing




matchStep :: Text.Text -> Text.Text -> Maybe RouteStepMatch
matchStep requestStep routeStep | isParamMatch       = Just $ RouteStepParam (Text.pack $ head params) requestStep
                                | isSimplerouteMatch = Just $ RouteStepMatched
                                | otherwise          = Nothing

                                where isParamMatch                = prefix == "" && length params == 1 && suffix == "" :: Bool
                                      (prefix, _, suffix, params) = (Text.unpack routeStep) =~ ("<(.*)>" :: String) :: (String, String, String, [String])

                                      isSimplerouteMatch    = simpleRouteRegexMatch /= ""
                                      simpleRouteRegexMatch = (Text.unpack requestStep) =~ ("^" ++ (Text.unpack routeStep) ++ "$") :: String



matchRouteWithRequest :: Route -> Request -> Maybe [RouteStepMatch]
matchRouteWithRequest route request | not isSameMethod             = Nothing
                                    | nRequestSteps /= nRouteSteps = Nothing
                                    | isRoot                       = Just [RouteStepMatched]
                                    | otherwise = sequenceA $ zipWith matchStep requestSteps (routeSteps route)

                                    where nRequestSteps = length requestSteps
                                          nRouteSteps   = length (routeSteps route)

                                          requestSteps = pathInfo request

                                          isRoot       = nRequestSteps == 0 && nRouteSteps == 0
                                          isSameMethod = requestMethod request == routeMethod route



router :: [Route] -> Middleware
router routes = \alternativeApp request respond -> let maybeApp = join
                                                                . find isJust
                                                                $ map (maybeAppFromRequestAndRoute request) routes

                                                    in case maybeApp of Just routeApp -> routeApp       request respond
                                                                        Nothing       -> alternativeApp request respond



routerMatchesLogger :: [Route] -> Middleware
routerMatchesLogger routes app request respond = do putStrLn "## ROUTER MATCHES ##"
                                                    _ <- for routes go
                                                    -- sequenceA . map go $ applicationRoutes
                                                    putStrLn "#####################"
                                                    app request respond

                                                where go route = putStrLn $ (show . routeSteps $ route) ++ ": " ++ (show $ matchRouteWithRequest route request)
