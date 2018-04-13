{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Controllers
import qualified Data.Default                         as D
import           Data.Maybe                           (catMaybes)
import           Network.HTTP.Types.Method (StdMethod(HEAD))
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           System.Console.CmdArgs
import           System.Directory
import           System.FilePath.Posix
import           System.IO                            (IOMode (..), openFile)
import           Web.Scotty
import Prelude hiding (head)


mkLogger :: FilePath -> IO Middleware
mkLogger logfile = do
        createDirectoryIfMissing True (takeDirectory logfile)
        logH <- openFile logfile AppendMode
        mkRequestLogger  D.def
          { destination = Handle logH
          , outputFormat = Apache FromSocket }

-- apparently not defined in Scotty
head :: RoutePattern -> ActionM () -> ScottyM ()
head = addroute HEAD

main :: IO ()
main = do
    MainOptions{..} <- cmdArgs mainOptions

    -- the request logger
    reqLogger <- if accesslog /= def
                 then Just <$> mkLogger accesslog
                 else return Nothing

    -- in debug mode also print verbose output to stdout
    let devLogger = if debug
                    then Just logStdoutDev
                    else Nothing

    let middlewares =  catMaybes [reqLogger, devLogger]

    scotty port $ do
      -- install the middlewares
      mapM_ middleware middlewares

      -- define routes
      get "/version" getVersion
      get "/status"  (getStatus basedir)
      get "/apk/:hash" (getApk basedir True)
      head "/apk/:hash" (getApk basedir False)
      post "/apk" (postApk basedir)


data MainOptions = MainOptions
  { port      :: Int
  , basedir   :: FilePath
  , accesslog :: FilePath
  , local     :: Bool
  , debug     :: Bool
  } deriving (Show, Data, Typeable)

mainOptions :: MainOptions
mainOptions = MainOptions
  { port        = 3000     &= typ "PORT" &= help "port on which apkstore should be available"
  , basedir     = "srv"    &= typDir     &= help "location of the apk directory"
  , accesslog   = def      &= typFile    &= help "log file in Apache log format"
  , local       = False                  &= help "reject non-local requests"
  , debug       = False                  &= help "show extra debug output"
  }
