
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Lens
import           Lib
import           Prelude      hiding (FilePath)
import           Turtle


main = do
    -- GET ARGS
    (scl,tz) <- options "A script to download pictures taken by the Himawari-8 satellite" parser
    -- pth <-
    --     case outputFolder of
    --         Nothing -> do; p <- home; return $ p <> ".himawari8"
    --         Just o -> return o
    pth <- home
    pth' <- return $ pth <> ".himawari8"
    -- MAKE URLS
    t <- curTime (negate $ tz)
    model <- return $ initialModel { _atime = t, _scale = scl, _imgfolder = pth', _imgfile = timePath t}
    putStrLn ("tz: "++ (show $ t)++ "\nscale: " ++ (show $ model^.scale))
    modelUrls <- return (tileURLs model)

    -- GET TILES
    putStrLn "Loading tiles..."
    modelBs <- getTiles modelUrls

    -- REMOVE OLD
    direxists <- testdir (modelBs^.imgfolder)
    case direxists of
        True  -> return ()
        False -> mkdir $ modelBs^.imgfolder

    putStrLn "Removing old files..."
    stdout $ removeOld (modelBs^.imgfolder)

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
    pathtoimg <- return (modelD^.imgfolder <> modelD^.imgfile)
    putStrLn "Saving png..."
    writeImage pathtoimg imgC

    -- SET DESKTOP
    -- putStrLn "Setting wallpaper..."
    -- setWallpaper pathtoimg

    putStrLn "Done."


initialModel :: Model
initialModel = Model
    { _tiles = []
    , _scale = 1
    , _tzOffset = 1
    , _baseUrl = "http://himawari8-dl.nict.go.jp/himawari8/img/D531106"
    , _imgfolder = "~/.himawari8"
    , _imgfile = ""
    , _atime = nullTime
    }


parser :: Parser (Int, Int)
parser =
    (,) <$> optInt "scale" 's' "Specifies the amount of tiles to be downloaded [default 1]"
        <*> optInt "timezone-offset" 't' "Timezone offset (in hours) [default 1]"
        -- <*> optional $ optPath "output" 'o' "Output folder [default ~/.himawari8]"


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

