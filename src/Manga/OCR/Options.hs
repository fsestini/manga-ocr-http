module Manga.OCR.Options (OCROptions(..), parseOptions) where

import Options.Applicative
import Data.Semigroup ((<>))
import System.FilePath ((</>))
import System.Directory (getHomeDirectory)

data OCROptions = OCROptions
  { ocrCommand    :: FilePath
  , tempDir       :: FilePath
  , routeNameOpt  :: String
  , serverPort    :: Int
  }

ocrOptionsParser :: FilePath -> Parser OCROptions
ocrOptionsParser homeDir = OCROptions
                <$> strOption
                    ( long "ocr-command"
                   <> short 'c'
                   <> help "command to invoke manga-ocr"
                   <> showDefault
                   <> value "manga_ocr"
                   <> metavar "COMMAND")
                <*> strOption
                    ( long "temporary-dir"
                   <> short 't'
                   <> help "temporary directory to store received images"
                   <> showDefault
                   <> value (homeDir </> ".ocr-temp")
                   <> metavar "DIR")
                <*> strOption
                    ( long "route-name"
                   <> short 'r'
                   <> help "name of the route where to expose the api"
                   <> showDefault
                   <> value "ocr"
                   <> metavar "ROUTE")
                <*> option auto
                    ( long "port"
                   <> short 'p'
                   <> help "Port for the server to listen to"
                   <> showDefault
                   <> value 8080
                   <> metavar "INT")

parseOptions :: IO OCROptions
parseOptions = do
  homeDir <- getHomeDirectory
  execParser $
    info (ocrOptionsParser homeDir <**> helper)
      ( fullDesc
     <> progDesc "Manga OCR functionality as a REST API."
      )
