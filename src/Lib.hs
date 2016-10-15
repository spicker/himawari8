{-# LANGUAGE RecordWildCards #-}

module Lib where


import           Codec.Picture
import           Control.Concurrent.Async
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.List
import           Data.Text                (pack, unpack)
import           Data.Time
import           Lens.Micro
import           Network.URI
import           Network.Wreq
import qualified Network.Wreq.Session     as S
import           Prelude                  hiding (FilePath)
import           Turtle
import qualified Vision.Image             as F
import qualified Vision.Image.JuicyPixels as JF
import           Vision.Primitive         (ix2)


data Tile = Tile
    { position   :: (Int, Int)
    , bytestring :: B.ByteString
    , url        :: URI
    , img        :: Image PixelRGB8
    -- , path :: FilePath
    }


data Model = Model
    { tiles     :: [Tile]
    , scale     :: Int
    , tzOffset  :: Int
    , baseUrl   :: String
    , imgfolder :: FilePath
    , imgfile   :: FilePath
    , atime     :: LocalTime
    }



--URLs
curTime :: Int -> IO LocalTime
curTime tz = do
    currentTime <- getCurrentTime
    return $ utcToLocalTime (hoursToTimeZone tz) currentTime


nullTime = LocalTime { localDay = fromGregorian 0 0 0, localTimeOfDay = midnight }


timePath :: LocalTime -> FilePath
timePath t =
    let
        getMinute = (\a -> div a 10) . toInteger . read $ formatTime defaultTimeLocale "%M" t
    in
        fromString $ (formatTime defaultTimeLocale "%Y-%m-%d-%H-" t) ++ show getMinute ++ ".png"


tileURLs :: Model -> Model
tileURLs model =
    let
        newTile = (\a -> Tile { position = a, bytestring = B.empty, url = nullURI, img = generateImage pixelRenderer 550 550})
        pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
        postupels = [ (x,y) | x <- [0..(scale model)-1], y <- [0..(scale model)-1] ]
        tilePos = map newTile postupels
        tilePaths = map (getTileURL model) tilePos
    in
        model { tiles = tilePaths }


getTileURL :: Model -> Tile -> Tile
getTileURL model tile =
    let
        getTimeStr = formatTime defaultTimeLocale "%Y/%m/%d/%H" (atime model)
        getMinute = (\a -> div a 10) . toInteger . read $ formatTime defaultTimeLocale "%M" (atime model)
        -- textStr = format (s%"/"%d%"d/550/"%s%d%"000_"%d%"_"%d%".png") baseUrl scl getTimeStr getMinute x y
        urlStr = ( baseUrl model ) ++ "/"  ++ show (scale model) ++ "d/550/" ++ getTimeStr ++ show getMinute ++ "000_" ++ show ((fst . position) tile) ++ "_" ++ show ((snd . position) tile) ++ ".png"
    in
        case (parseURI urlStr) of
            Just uri -> tile { url = uri }
            Nothing  -> tile


-- Load tiles
getTile :: S.Session -> Tile -> IO Tile
getTile sess tile = do
    u <- return $ show (url tile)
    putStrLn $ "GET Url: " ++ u
    r <- S.get sess u
    putStrLn $ "Status: " ++ show (r ^. responseStatus . statusCode) ++ " " ++ show (r ^. responseStatus . statusMessage)
    return $ tile { bytestring = BL.toStrict $ r ^. responseBody }


getTiles :: Model -> IO Model
getTiles model = S.withSession $ \sess -> do
    ts <- return $ tiles model
    ts' <- mapConcurrently (getTile sess) ts
    return model {tiles = ts'}


decodeTile :: Tile -> IO Tile
decodeTile tile =
    case decodePng (bytestring tile) of
        Left err -> do
            putStrLn ("Error decoding Png:" ++ err)
            return tile
        Right (ImageRGB8 i) -> return tile { img = i }
        Right _ -> do
            putStrLn "Not a ImageRGB8"
            return tile


decodeTiles :: Model -> IO Model
decodeTiles model = do
    t <- sequence $ map decodeTile (tiles model)
    return model { tiles = t }


--STICH TILES
findTile :: [Tile] -> (Int, Int) -> Maybe Tile
findTile tl pos =
    Data.List.find (\t -> (position t) == pos) tl


stitchTiles :: Model -> Image PixelRGB8
stitchTiles model =
    let
        stitcher x y =
            case findTile (tiles model) (xD, yD) of
                Just t  -> pixelAt (img t) xR yR
                Nothing -> PixelRGB8 0 0 0
            where
                (xD, xR) = x `quotRem` 550
                (yD, yR) = y `quotRem` 550
    in
        generateImage stitcher (550 * scale model) (550 * scale model)


--RESIZE
resizeImage :: Image PixelRGB8 -> Image PixelRGB8
resizeImage img =
    let
        fimg = JF.toFridayRGB img
        size = ix2 1550 1550
    in
        JF.toJuicyRGB $ F.resize F.Bilinear size fimg


--CROP IMAGE
cropImage :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
cropImage screenWidth screenHeight im@Image {..} =
    let
        crop x y
            | y < top = black
            | y >= bottom = black
            | x < left = black
            | x >= right = black
            | otherwise = pixelAt im ( x - left ) ( y - top )
            where
                black = PixelRGB8 0 0 0
                top = screenHeight - imageHeight
                bottom = screenHeight
                left = (screenWidth - imageWidth) `div` 2
                right = imageWidth + ((screenWidth - imageWidth) `div` 2)
    in
        generateImage crop screenWidth screenHeight


--Write Image
writeImage :: PngSavable pixel => FilePath -> Image pixel -> IO ()
writeImage path = writePng (unpack $ format fp path)
