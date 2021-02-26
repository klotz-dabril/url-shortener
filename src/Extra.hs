--
-- Extra.hs
--
--


module Extra ( extractBodyParams
             , tupleFromStrict
             ) where


import Data.Traversable               (for)
import Network.Wai
import Network.Wai.Parse

import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Internal as SB
import qualified Data.Map                 as Map



extractBodyParams :: Request -> [LB.ByteString] -> IO (Maybe [LB.ByteString])
extractBodyParams request paramNames = do (strictRequestParamsList, _) <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd request
                                          let lazyRequestParamList = tupleFromStrict <$> strictRequestParamsList
                                              requestParams = Map.fromList lazyRequestParamList

                                          return . for paramNames $ (\k -> Map.lookup k requestParams)



tupleFromStrict :: (SB.ByteString, SB.ByteString) -> (LB.ByteString, LB.ByteString)
tupleFromStrict (x_0, x_1) = (LB.fromStrict x_0, LB.fromStrict x_1)
