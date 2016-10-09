
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}


module Main where


import Prelude hiding (FilePath)
import Lib
import Turtle


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
    pathtoimg <- return (imgfolder modelD <> imgfile modelD)
    putStrLn "Saving png..."
    writeImage pathtoimg imgC

    -- SET DESKTOP
    putStrLn "Setting wallpaper..."
    setWallpaper pathtoimg

    putStrLn "Done."


parser :: Parser (Int, Int)
parser = 
    (,) <$> argInt "scale" "Scale"
        <*> argInt "tz" "Timezone offset"


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

