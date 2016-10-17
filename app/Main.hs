{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Lens
import           Data.Maybe   (fromMaybe)
import           Lib
import           Prelude      hiding (FilePath)
import           Turtle


main = do
    -- Get args
    model <- settings
    modelUrls <- return (tileURLs model)

    -- Make dir
    direxists <- testdir $ model^.imgfolder
    case direxists of
        False -> mkdir $ model^.imgfolder
        True  -> return ()

    -- GET TILES
    putStrLn "Loading tiles..."
    modelBs <- getTiles modelUrls

    -- REMOVE OLD
    putStrLn "Removing old files..."
    stdout $ removeOld $ modelBs^.imgfolder

    -- SAVE TILES
    putStrLn "Decoding tiles..."
    modelD <- decodeTiles modelBs

    -- STITCH TILES
    putStrLn "Stitching tiles..."
    img <- return $ stitchTiles modelD

    -- RESIZE
    putStrLn "Resizing image..."
    imgR <- return $ resizeImage (modelD^.height) img

    -- CROP IMAGE
    putStrLn "Crop image..."
    imgC <- return $ cropImage (modelD^.width) (modelD^.height) imgR

    -- WRITE IMAGE
    pathtoimg <- return (modelD^.imgfolder <> modelD^.imgfile)
    putStrLn "Saving png..."
    writeImage pathtoimg imgC

    -- SET DESKTOP
    -- putStrLn "Setting wallpaper..."
    -- setWallpaperOSX pathtoimg

    putStrLn "Done."


parser :: Parser (Maybe Int, Maybe Int, Maybe FilePath, Maybe Int, Maybe Int)
parser =
    (,,,,) <$> (optional $ optInt "scale" 's' "Specifies the amount of tiles to be downloaded [default 1]")
        <*> (optional $ optInt "timezone-offset" 't' "Timezone offset (in hours) [default 1]")
        <*> (optional $ optPath "output" 'o' "Output folder [default ~/.himawari8]")
        <*> (optional $ optInt "width" 'w' "Output image width [default 2650]")
        <*> (optional $ optInt "height" 'g' "Output image height [default 1600]")


settings :: IO Model
settings = do
    homepath <- home
    (s,t,o,w,h) <- options "A small program to download images taken by the Himawari-8 satellite" parser
    let sDefault = 1
        tDefault = 1
        oDefault = homepath <> ".himawari8"
        wDefault = 2560
        hDefault = 1600
    ct <- curTime (negate $ fromMaybe tDefault t)
    return $ Model
        { _scale = fromMaybe sDefault s
        , _tzOffset = fromMaybe tDefault t
        , _imgfolder =  fromMaybe oDefault o
        , _width = fromMaybe wDefault w
        , _height = fromMaybe hDefault h
        , _tiles = []
        , _baseUrl = "http://himawari8-dl.nict.go.jp/himawari8/img/D531106"
        , _imgfile = timePath ct
        , _atime = ct}


setWallpaperOSX :: FilePath -> IO ()
setWallpaperOSX imgpath = do
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
