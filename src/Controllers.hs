{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.ByteString.Base16    as Base16
import qualified Data.ByteString.Lazy      as LB
import           Data.Char                 (isHexDigit)
import           Data.List                 (intercalate)
import           Data.Maybe                (listToMaybe)
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Lazy            as LT
import Data.Aeson hiding (json)
import           Network.HTTP.Types.Status
import           Network.Wai.Parse
import           System.Directory
import           System.FilePath.Posix
import           Web.Scotty

import qualified Data.ByteString.Char8     as Char8

import qualified Crypto.Hash.SHA1          as SHA1

isHash :: String -> Bool
isHash t = let onlyHex = and $ isHexDigit <$> t
               lenOk   = Prelude.length t == 40
           in onlyHex && lenOk

type BaseDir = String

newtype Apk = Apk
  { hash :: String
  } deriving (Show)

-- git-like file paths. The first two characters are used to identify the
-- directory, the rest becomes the filename
filepath :: BaseDir -> Apk -> FilePath
filepath base (Apk h) = let dir = Prelude.take 2 h
                            fil = Prelude.drop 2 h
                        in  Data.List.intercalate "/" [base, dir, fil]

-- count the total number of files inside the base directory
numApks :: BaseDir -> IO Int
numApks base = do
  folders <- listDirectory base
  let folders' = Prelude.map (\f -> base ++ "/" ++ f) folders
  allFiles <- Prelude.concat <$> mapM listDirectory folders'
  return (Prelude.length allFiles)


getVersion :: ActionM ()
getVersion = text "version 1.0"

getStatus :: BaseDir -> ActionM ()
getStatus base = do
  numFiles <- liftIO $ numApks base
  json $ object [ "apks" .= numFiles
                , "version" .= ("0.1" ::String)
                ]

parseApk :: Text -> ActionM Apk
parseApk p = do
  pHash <- T.unpack <$> param p
  unless (isHash pHash) $ do
      status badRequest400
      raise "parameter hash must be a SHA1 hash in hexadecimal format"
  return $ Apk pHash

getApk :: BaseDir -> Bool -> ActionM ()
getApk basedir sendBody = do
  apk <- parseApk "hash"
  fileExists <- liftIO (doesFileExist $ filepath basedir apk)

  -- TODO: this might not be necessary. `file` should handle the 404ing
  unless fileExists $ do
    status notFound404
    text "cannot find apk"

  setHeader "Content-Type" "application/apk"
  setHeader "Content-Disposition" ("attachment; filename=" <> LT.pack (hash apk) <> ".apk")
  when sendBody $
    file $ filepath basedir apk




aFile :: ActionM Web.Scotty.File
aFile = do
  fs <- files
  let maybeF = listToMaybe fs
  case maybeF of
    Nothing -> raise "I was promised a file"
    Just f  -> return f

postApk :: BaseDir -> ActionM ()
postApk basedir = do
  f <- aFile
  let fc = fileContent $ snd f
  apk <- liftIO  $ do
    let apk = Apk $ Char8.unpack $ Base16.encode $ SHA1.hashlazy fc
        fp  = filepath basedir apk
    fExists <- doesFileExist $ filepath basedir apk
    unless fExists $ do
      createDirectoryIfMissing True (takeDirectory fp)
      LB.writeFile fp fc
    return apk
  text $ LT.pack $ hash apk
