{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Manga.OCR.Http where

import Manga.OCR.Options (parseOptions, OCROptions (..))

import Web.Spock ( middleware, SpockM, SpockAction, post, body, runSpock, spock, text, setStatus, get, static )
import Network.Wai.Middleware.Cors (simpleCors)
import System.IO (Handle, hIsEOF, hGetLine, SeekMode (SeekFromEnd), hSeek)
import System.Process (withCreateProcess, proc)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import System.Timeout (timeout)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Web.Spock.Config (defaultSpockCfg, PoolOrConn (PCNoDatabase))
import Data.Time (UTCTime(..), getCurrentTime)
import Network.HTTP.Types (serviceUnavailable503)
import Shelly (runHandle, shelly, mkdir_p, which, terror, touchfile)

type ServerState = ()
type Server a = SpockM () () ServerState a
type ApiAction a = SpockAction () () ServerState a

data ServerConfig = ServerConfig
  { ocrHandle  :: Handle
  , tempImgDir :: FilePath
  , routeName  :: String
  }

waitNewLine :: Int -> Handle -> IO (Maybe Text)
waitNewLine iter h = timeout (iter * interval) loop
  where
    interval = 1000000
    loop =  do
      b <- hIsEOF h
      if b
        then threadDelay interval >> loop
        else TIO.hGetLine h

seekEOF :: Handle -> IO ()
seekEOF h = hSeek h SeekFromEnd 0

app :: IO ()
app = do
  opts <- parseOptions
  let tempImgDir = tempDir opts </> "imgs"
      tempOutput = tempDir opts </> "ocr.txt"
      comm = ocrCommand opts
      port = serverPort opts
  shelly do
    which comm >>= maybe (terror (err comm)) (const (pure ()))
    mkdir_p tempImgDir
    touchfile tempOutput
    runHandle comm ["-r", pack tempImgDir, "-w", pack tempOutput] $ \h -> liftIO do
      let s = server (ServerConfig h tempImgDir (routeNameOpt opts))
      c <- defaultSpockCfg () PCNoDatabase ()
      liftIO $ runSpock port (spock c s)
  where
    err c = "could not find command " <> pack c <> "in PATH"

server :: ServerConfig -> Server ()
server c = do
  middleware simpleCors
  post (static (routeName c)) $ do
    reqData <- body
    ml <- liftIO $ do
      seekEOF h
      utc <- show . utctDayTime <$> getCurrentTime
      B.writeFile (tempImgDir c </> (utc ++ ".jpg")) reqData
      waitNewLine 10 h
    maybe
      (setStatus serviceUnavailable503)
      text ml
  where
    h = ocrHandle c
