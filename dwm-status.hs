{-# OPTIONS_GHC -Wall #-}
-- rewrite of dwm-status.c in the one true programming language
-- but first, install haskell-x11

import Graphics.X11.Xlib.Event (sync)
import Graphics.X11.Xlib.Display (defaultRootWindow, openDisplay, closeDisplay)
-- import Graphics.X11.Xlib.Display 
-- import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Xlib.Window (storeName)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Applicative ((<$>))

import System.Locale (defaultTimeLocale) -- for time
import System.Time
-- import Data.Time.Clock 
-- import Data.Time.LocalTime 
-- import Data.Time.Format 
import Text.Printf

-- see below for ansi codes help
-- colours
red, white, magenta, purple, yellow, blue, brightGreen, deepGreen :: String
whitebg, magentabg, purplebg, bluebg, deepGreenbg :: String
unknown :: String
red = "\x1b[38;5;196m"
white = "\x1b[38;5;255m"
whitebg = "\x1b[48;5;255m"
magenta = "\x1b[38;5;199m"
magentabg = "\x1b[48;5;199m"
yellow = "\x1b[38;5;190m"
blue = "\x1b[38;5;21m"
bluebg = "\x1b[48;5;21m"
brightGreen = "\x1b[38;5;46m" -- 40 is less bright
deepGreen = "\x1b[38;5;34m"
deepGreenbg = "\x1b[48;5;34m"
purple = "\x1b[38;5;57m"
purplebg = "\x1b[48;5;57m"
unknown = yellow

resetAll :: String
resetAll = "\x1b[0m" -- resets all attributes

main :: IO ()
main = do
    dpy <- openDisplay ""
    mainLoop dpy 
    closeDisplay dpy

playWithDpy :: IO ()
playWithDpy = openDisplay "" >>= closeDisplay -- LOL

mainLoop :: Display -> IO ()
mainLoop dpy = forever $ do
    time <- getDateTime "[%a %b %d] %H:%M:%S" 
    (x,y,z) <- getLoadAvg 
    let avgs = printf "[%.2f %.2f %.2f] " x y z
    batt <- getBattery
    temper <- getTemperature
    setStatus dpy $ avgs ++ batt ++ temper ++ time
    threadDelay 1000000 -- sleep one second
--     threadDelay 6000000 -- sleep six seconds
--     threadDelay 10000000 -- sleep ten seconds
--     threadDelay 60000000 -- sleep sixty seconds
   
setStatus :: Display -> String -> IO ()
setStatus dpy str = do
    let window = defaultRootWindow dpy
    storeName dpy window str
    sync dpy False

{-
-- getDateTime could be implemented instead with Data.Time.Calendar
getDateTime :: IO String
getDateTime = do
    time <- getCurrentTime
    timezone <- getCurrentTimeZone
    let localTime = utcToLocalTime timezone time
    return $ formatTime defaultTimeLocale "[%a %b %d] %H:%M:%S" localTime
--     return $ formatTime defaultTimeLocale "[%a %b %d] %H:%M" localTime

-- or like this
-- getDateTime = do
--     localTime <- getClockTime >>= toCalendarTime
--     return $ formatCalendarTime defaultTimeLocale "[%a %b %d] %H:%M:%S" localTime
--     return $ formatCalendarTime defaultTimeLocale "[%a %b %d] %H:%M" localTime

-- or like this:
-- getDateTime = getClockTime >>= toCalendarTime >>= return . 
--               formatCalendarTime defaultTimeLocale "[%a %b %d] %H:%M:%S" 
-}

-- or like this!
-- give it the format you want
getDateTime :: String -> IO String
getDateTime str = getClockTime >>= toCalendarTime >>= return . 
                  formatCalendarTime defaultTimeLocale str


type LoadAvgs = (Double, Double, Double)
getLoadAvg :: IO LoadAvgs
-- getLoadAvg :: IO (Double, Double, Double)
getLoadAvg = parseLoadAvg <$> readFile "/proc/loadavg"
    where
        parseLoadAvg :: String -> LoadAvgs
        parseLoadAvg input = (x,y,z) 
            where (x:y:z:_) = map read (words input)


getFileData :: FilePath -> IO Int
getFileData = fmap read . readFile 

getTemperature :: IO String
getTemperature = do
    temper <- ((/1000) . fromIntegral) <$> getFileData 
        "/sys/class/thermal/thermal_zone0/temp"
    let colour = f temper
    return $ printf "[%s%02.1f°C%s] " colour temper (getColour 0)
        where 
            f :: Double -> String
            f temper 
                | temper > 80 = getColour 1 -- red
                | temper > 75 = getColour 2 -- yellow
                | temper < 70 = getColour 6 -- blue
                | otherwise   = getColour 0 -- no colour

getBattery :: IO String
getBattery = do
    capacity <- getFileData "/sys/class/power_supply/BAT0/capacity"
    status <- readFile "/sys/class/power_supply/BAT0/status"
    let chargin = status /= "Discharging\n"
    {-
    -- TODO: display warning on low battery, using zenity
    if capacity < 95 
        then do
            if not chargin
                then putStrLn "not plugged in! oh no :("
                else putStrLn "plugged in"
        else putStrLn "lotsa battery left!"
    -}
    let colour :: String
        colour = if chargin
        then getColour 4 -- magenta
        else if capacity > 70 
            then getColour 3 -- deep green
            else if capacity > 30 
                then getColour 6 -- blue
                else if capacity > 10 
                    then getColour 2 -- yellow
                    else getColour 1 -- red
    return $ printf "[%s%d%%%s] " colour capacity (getColour 0)
--     return (colour ++ show capacity ++ getColour 0)

-- positive codes return various colours
-- code 0 returns a reset string
-- code -1 returns empty string
-- one way to display all available colours: weechat --colors
getColour :: Int -> String
getColour 0 = "\x1b[0m" -- reset 
getColour 1 = "\x1b[38;5;196m" -- red
getColour 2 = "\x1b[38;5;190m" -- yellow
getColour 3 = "\x1b[38;5;34m" -- deep green
getColour 4 = "\x1b[38;5;199m" -- magenta
getColour 5 = "\x1b[38;5;46m" -- bright green
getColour 6 = "\x1b[38;5;45m" -- bright blue
getColour 7 = "\x1b[38;5;21m" -- blue
getColour _ = "" -- empty string



{-
an ansi escape sequence is in the form:

    \e[<code>m

where:

    \e - escape - ascii 27 / hex 1b / octal 033
    [ - literal bracket
    m - literal 'm'

the code is one of the following:
    0 - reset colors to default
    n;m
        n -
            o - normal color
            1 - 'bright' color
        m -
            30-37 - foreground
            40-47 - background
    n;5;m
        n - 
            38 - foreground
            48 - background
        5 - 
            literal '5'
        m - 
            0-15 - classic ansi color
            16-231 - xterm 256-color rgb color
            232-255 - grayscale
         see all of the colours with weechat --colors
-}
