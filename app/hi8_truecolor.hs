#!/usr/bin/env stack
{- stack
    --resolver lts-6.20
    --install-ghc runghc 
    --package turtle
    --package base
    --package HTTP
    --package JuicyPixels
    --package friday
    --package async
    --
-}

-- hi8_truecolor.hs
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}


module Main where


import           Data.Text       (pack, unpack)
import           Data.Time
import qualified Data.List
import qualified Data.ByteString as B
-- import           Control.Concurrent.Async
import           Network.Browser
import           Network.HTTP
import           Network.URI
import           Prelude hiding (FilePath)
import           Turtle
import           Codec.Picture
import qualified Vision.Image as F
import           Vision.Primitive (ix2)
import qualified Vision.Image.JuicyPixels as JF


main :: IO ()
main = do
    -- GET ARGS
    (scl,tz) <- options "A script to download the current picture of Himawari8" parser

    -- MAKE URLS
    t <- curTime (negate $ tz)
    pth <- home
    model <- return $ initialModel { atime = t, scale = scl, imgfolder = pth <> ".himawari8", imgfile = timePath t}
    putStrLn ("tz: "++ (show $ tzOffset model)++ "\nscale: " ++ (show $ scale model))
    modelUrls <- return (tileURLs model)

    -- GET TILES
    putStrLn "Loading tiles..."
    modelBs <- getTiles modelUrls
    
    -- REMOVE OLD
    direxists <- testdir (imgfolder modelBs)
    case direxists of
        True -> return ()
        False -> mkdir $ imgfolder modelBs

    putStrLn "Removing old files..."
    stdout $ removeOld (imgfolder modelBs)

    -- SAVE TILES
    putStrLn "Decoding tiles..."
    modelD <- decodeTiles modelBs

    -- STITCH TILES
    putStrLn "Stitching tiles..."
    img <- return $ stitchTiles modelD
    
    -- RESIZE
    putStrLn "Resizing image..."
    imgR <- return $ resizeImage img

    -- CROP IMAGE
    putStrLn "Crop image..."
    imgC <- return $ cropImage 2560 1600 imgR

    -- WRITE IMAGE
    touch (imgfolder modelD <> imgfile modelD)
    pathtoimg <- realpath (imgfolder modelD <> imgfile modelD)
    putStrLn "Saving png..."
    writePng (unpack $ format fp pathtoimg) imgC

    -- SET DESKTOP
    putStrLn "Setting wallpaper..."
    setWallpaper pathtoimg



    putStrLn "Done."



data Tile = Tile 
    { position :: (Int, Int)
    , bytestring :: B.ByteString
    , url :: URI
    , img :: Image PixelRGB8
    -- , path :: FilePath
    } 


data Model = Model 
    { tiles :: [Tile]
    , scale :: Int
    , tzOffset :: Int
    , baseUrl :: String
    , imgfolder :: FilePath
    , imgfile :: FilePath
    , atime :: LocalTime
    } 


parser :: Parser (Int, Int)
parser = 
    (,) <$> argInt "scale" "Scale"
        <*> argInt "tz" "Timezone offset"

initialModel :: Model
initialModel = Model 
    { tiles = []
    , scale = 1
    , tzOffset = 1
    , baseUrl = "http://himawari8-dl.nict.go.jp/himawari8/img/D531106"
    , imgfolder = ""
    , imgfile = ""
    , atime = LocalTime { localDay = fromGregorian 0 0 0, localTimeOfDay = midnight } 
    }


--URLs
curTime :: Int -> IO LocalTime
curTime tz = do
    currentTime <- getCurrentTime
    return $ utcToLocalTime (hoursToTimeZone tz) currentTime


timePath :: LocalTime -> FilePath
timePath t = fromString $ (formatTime defaultTimeLocale "%Y-%m-%d-%H" t) ++ ".png"


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
            Nothing -> tile
    

getTile :: Tile -> BrowserAction (HandleStream B.ByteString) Tile
getTile tile = do
    req <- return (url tile)
    (_,rsp) <- request (mkRequest GET req)
    return tile { bytestring = (rspBody rsp) }


getTiles :: Model -> IO Model
getTiles model = do
    ts <- Network.Browser.browse $ do
        setAllowRedirects True
        sequence (map getTile (tiles model))
    return model { tiles = ts }


-- getTile' :: Tile -> IO Tile
-- getTile' tile = do
--     rsp <- httpLBS 


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
                Just t -> pixelAt (img t) xR yR
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
            | otherwise = pixelAt im (x-left) (y-top)
            where
                black = PixelRGB8 0 0 0
                top = (screenHeight - imageHeight) `div` 2
                bottom = imageHeight + ((screenHeight - imageHeight) `div` 2)
                left = (screenWidth - imageWidth) `div` 2
                right = imageWidth + ((screenWidth - imageWidth) `div` 2)
    in
        generateImage crop screenWidth screenHeight


--SET BACKGROUND
setWallpaper :: FilePath -> IO ()
setWallpaper imgpath = do
    procs 
        "sqlite3" 
        [ "/Users/spicker/Library/Application Support/Dock/desktoppicture.db"
        , format ("update data set value = '"%fp%"'") imgpath ] 
        empty
    procs 
        "killall" 
        [ "Dock" ] 
        empty


removeOld :: FilePath -> Shell Text
removeOld folder = do
    p <- find (ends $ text ".png") folder
    liftIO $ rm p
    return (format (s%fp) "Removed: " p)