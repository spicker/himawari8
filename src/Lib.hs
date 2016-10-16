{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where


import           Codec.Picture
import           Control.Concurrent.Async
import           Control.Lens
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.List
import           Data.Text                (pack, unpack)
import           Data.Time
import           Network.URI
import           Network.Wreq
import qualified Network.Wreq.Session     as S
import           Prelude                  hiding (FilePath)
import           Turtle
import qualified Vision.Image             as F
import qualified Vision.Image.JuicyPixels as JF
import           Vision.Primitive         (ix2)


data Tile = Tile
    { _position   :: (Int, Int)
    , _bytestring :: B.ByteString
    , _url        :: URI
    , _img        :: Image PixelRGB8
    }


data Model = Model
    { _scale     :: Int
    , _tzOffset  :: Int
    , _imgfolder :: FilePath
    , _width     :: Int
    , _height    :: Int
    , _tiles     :: [Tile]
    , _baseUrl   :: String
    , _imgfile   :: FilePath
    , _atime     :: LocalTime
    }


makeLenses ''Model
makeLenses ''Tile


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
        newTile = (\a -> Tile { _position = a, _bytestring = B.empty, _url = nullURI, _img = generateImage pixelRenderer 550 550})
        pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
        postupels = [ (x,y) | x <- [0..(model^.scale)-1], y <- [0..(model^.scale)-1] ]
        tilePos = map newTile postupels
        tilePaths = map (getTileURL model) tilePos
    in
        set tiles tilePaths model


getTileURL :: Model -> Tile -> Tile
getTileURL model tile =
    let
        getTimeStr = formatTime defaultTimeLocale "%Y/%m/%d/%H" (model^.atime)
        getMinute = (\a -> div a 10) . toInteger . read $ formatTime defaultTimeLocale "%M" (model^.atime)
        -- textStr = format (s%"/"%d%"d/550/"%s%d%"000_"%d%"_"%d%".png") baseUrl scl getTimeStr getMinute x y
        urlStr = (model^.baseUrl) ++ "/"  ++ show (model^.scale) ++ "d/550/" ++ getTimeStr ++ show getMinute ++ "000_" ++ show (tile^.position._1) ++ "_" ++ show (tile^.position._2) ++ ".png"
    in
        case (parseURI urlStr) of
            Just uri -> set url uri tile
            Nothing  -> tile


-- Load tiles
getTile :: S.Session -> Tile -> IO Tile
getTile sess tile = do
    u <- return $ show (tile^.url)
    putStrLn $ "GET Url: " ++ u
    r <- S.get sess u
    putStrLn $ "Status: " ++ show (r ^. responseStatus . statusCode) ++ " " ++ show (r ^. responseStatus . statusMessage)
    -- return $ tile { bytestring = BL.toStrict $ r ^. responseBody }
    return $ set bytestring (BL.toStrict $ r ^. responseBody) tile


getTiles :: Model -> IO Model
getTiles model = S.withSession $ \sess -> do
    ts <- return $ model^.tiles
    ts' <- mapConcurrently (getTile sess) ts
    return $ set tiles ts' model


decodeTile :: Tile -> IO Tile
decodeTile tile =
    case decodePng (tile^.bytestring) of
        Left err -> do
            putStrLn ("Error decoding Png:" ++ err)
            return tile
        Right (ImageRGB8 i) -> return $ set img i tile
        Right _ -> do
            putStrLn "Not a ImageRGB8"
            return tile


decodeTiles :: Model -> IO Model
decodeTiles model = do
    t <- sequence $ map decodeTile (model^.tiles)
    return $ set tiles t model


--STICH TILES
findTile :: [Tile] -> (Int, Int) -> Maybe Tile
findTile tl pos =
    Data.List.find (\t -> (t^.position) == pos) tl


stitchTiles :: Model -> Image PixelRGB8
stitchTiles model =
    let
        stitcher x y =
            case findTile (model^.tiles) (xD, yD) of
                Just t  -> pixelAt (t^.img) xR yR
                Nothing -> PixelRGB8 0 0 0
            where
                (xD, xR) = x `quotRem` 550
                (yD, yR) = y `quotRem` 550
    in
        generateImage stitcher (550 * model^.scale) (550 * model^.scale)


--RESIZE
resizeImage :: Int -> Image PixelRGB8 -> Image PixelRGB8
resizeImage s img =
    let
        fimg = JF.toFridayRGB img
        size = ix2 s s
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
                top = (screenHeight - imageHeight) `div` 2
                bottom = imageHeight + ((screenHeight - imageHeight) `div` 2)
                left = (screenWidth - imageWidth) `div` 2
                right = imageWidth + ((screenWidth - imageWidth) `div` 2)
    in
        generateImage crop screenWidth screenHeight


--Write Image
writeImage :: PngSavable pixel => FilePath -> Image pixel -> IO ()
writeImage path = writePng (unpack $ format fp path)
