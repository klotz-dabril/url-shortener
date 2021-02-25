--
-- RequestCounter.hs
--
--

module RequestCounter (requestCounter) where

import Control.Concurrent.MVar
import Network.Wai


increment :: MVar Int -> IO Int
increment counterVar = modifyMVar counterVar go
        where go count = do let count' = count + 1
                            return (count', count')



requestCounter :: MVar Int -> Middleware
requestCounter countRef app request respond = do c <- increment countRef
                                                 putStrLn $ "COUNTER: " ++ (show c)
                                                 app request respond
