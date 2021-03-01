--
-- Main.hs
--
--

{-# LANGUAGE OverloadedStrings #-}


module Main (main) where

import           Control.Concurrent.MVar
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Builder    as Builder
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map                   as Map
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp   (run)


import Extra
import RequestLogger
import RequestCounter
import Router
import Short


import Data.Typeable
import Debug.Trace



retrieveAction :: Action
retrieveAction urlParams request respond = do maybeUrl <- runMaybeT $ do idText  <- MaybeT . return $ Map.lookup "id" urlParams
                                                                         shortId <- MaybeT . return $ textToIntegral idText

                                                                         findShortById shortId >>= return . getUrl

                                              case maybeUrl of Just url -> respond $ responseLBS status200
                                                                                                 [("Content-Type", "text/plain")]
                                                                                                 (C.pack url)

                                                               Nothing  -> not_found_app request respond



createAction :: Action
createAction _ request respond = do maybeId <- runMaybeT $ do [url] <- extractBodyParams request ["url"]
                                                              lift $ createShort url

                                    case maybeId of Just rowId -> respond $ responseLBS status200
                                                                                        [("Content-Type", "text/plain")]
                                                                                        (Builder.toLazyByteString $ Builder.int64Dec rowId)

                                                    _          -> error_app request respond



not_found_app :: Application
not_found_app _ respond = respond $ responseLBS status404
                                                [("Content-Type", "text/plain")]
                                                "not_found"



error_app :: Application
error_app _ respond = respond $ responseLBS status500
                                            [("Content-Type", "text/plain")]
                                            "error"



applicationRoutes :: [Route]
applicationRoutes = [ makeRoute methodPost "/"     createAction
                    , makeRoute methodGet  "/<id>" retrieveAction
                    ]



application :: Application
application = router applicationRoutes not_found_app




main :: IO ()
main = do counter <- newMVar 0
          run 3000 . requestCounter counter
                   . routerMatchesLogger applicationRoutes
                   . requestLogger
                   $ application
