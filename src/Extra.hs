--
-- Extra.hs
--
--


module Extra ( extractBodyParams
             , tupleFromStrict
             , textToIntegral
             ) where


import           Data.Traversable           (for)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Internal   as SB
import qualified Data.Map                   as Map
import qualified Data.Text                  as Text
import qualified Data.Text.Read             as TextRead
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Network.Wai
import           Network.Wai.Parse





extractBodyParams :: Request -> [LB.ByteString] -> MaybeT IO [String]
extractBodyParams request paramNames = do (strictRequestParamsList, _) <- lift $ parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd request
                                          let lazyRequestParamList = tupleFromStrict <$> strictRequestParamsList
                                              requestParams = Map.fromList lazyRequestParamList

                                          MaybeT . return . for paramNames $ (\k -> C.unpack <$> Map.lookup k requestParams)



textToIntegral :: Integral a => Text.Text -> Maybe a
textToIntegral text = case TextRead.decimal text of (Right (n,_)) -> Just n
                                                    _             -> Nothing



tupleFromStrict :: (SB.ByteString, SB.ByteString) -> (LB.ByteString, LB.ByteString)
tupleFromStrict (x_0, x_1) = (LB.fromStrict x_0, LB.fromStrict x_1)
