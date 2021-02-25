--
-- RequestLogger.hs
--
--

module RequestLogger (requestLogger) where

import Network.Wai


requestLogger :: Middleware
requestLogger app request respond = do putStrLn $ "LOGGER: " ++ (show request)
                                       app request respond
