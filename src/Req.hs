module Req where


import           Control.Concurrent.Async
import           Control.Lens
import qualified Data.ByteString.Lazy     as B
import           Data.String              (fromString)
import           Lib
import           Network.URI
import           Network.Wreq
import qualified Network.Wreq.Session     as S


getTile :: S.Session -> Tile -> IO Tile
getTile sess tile = do
    u <- return $ show (url tile)
    putStrLn $ "GET Url: " ++ u
    r <- S.get sess u
    putStrLn $ "Status: " ++ show (r ^. responseStatus . statusCode) ++ " " ++ show (r ^. responseStatus . statusMessage)
    return $ tile { bytestring = B.toStrict $ r ^. responseBody }


getTiles :: Model -> IO Model
getTiles model = S.withSession $ \sess -> do
    ts <- return $ tiles model
    ts' <- mapConcurrently (getTile sess) ts
    return model {tiles = ts'}

