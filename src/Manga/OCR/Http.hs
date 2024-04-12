{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Manga.OCR.Http where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime (..), getCurrentTime)
import GHC.IO.Buffer (BufferState (ReadBuffer))
import Manga.OCR.Options (OCROptions (..), parseOptions)
import Network.HTTP.Types (serviceUnavailable503)
import Network.Wai.Middleware.Cors (simpleCors)
import Shelly (mkdir_p, runHandle, shelly, terror, touchfile, which)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (ReadMode), SeekMode (SeekFromEnd), hGetLine, hIsEOF, hSeek, withFile)
import System.Process (proc, withCreateProcess)
import System.Timeout (timeout)
import Web.Spock (SpockAction, SpockM, body, get, middleware, post, runSpock, setStatus, spock, static, text)
import Web.Spock.Config (PoolOrConn (PCNoDatabase), defaultSpockCfg)

type ServerState = ()

type Server a = SpockM () () ServerState a

type ApiAction a = SpockAction () () ServerState a

data ServerConfig = ServerConfig
  { ocrHandle :: Handle,
    tempImgDir :: FilePath,
    routeName :: String
  }

waitNewLine :: Int -> Handle -> IO (Maybe Text)
waitNewLine iter h = timeout (iter * interval) loop
  where
    interval = 1000000
    loop = do
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
    runHandle comm ["-r", pack tempImgDir, "-w", pack tempOutput] $ \_ -> liftIO do
      withFile tempOutput ReadMode $ \fh -> do
        let s = server (ServerConfig fh tempImgDir (routeNameOpt opts))
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
      text
      ml
  where
    h = ocrHandle c
