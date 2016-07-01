{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Software where

import           ClassyPrelude

import           Control.Monad.Trans.Either

type Command = EitherT Text IO

type PathAndEnvironment = ([FilePath], [FilePath], [(String, String)])

none :: a -> Command PathAndEnvironment
none _ = return ([], [], [])

data Location = Opt | Local

-- | This type describes program packages. Sometimes the package contents are
--   at the toplevel. In that case it is necessary to create a directory before
--   extracting, and its name has to be specified.
data ArchiveType
  -- | A Zip archive.
  = Zip (Maybe FilePath)
  -- | A tar, tar.gz, tar.bz, and tar.xz archive. `tar` can handle those formats
  --   automatically.
  | TarX (Maybe FilePath)
  -- | An installer which has to be chmod'ed and executed. The command gets
  -- passed the path of the package and where the package shall be installed.
  | Installer (FilePath -> FilePath -> Command ())
  | Plain

-- | This type is for describing how software can be installed.
--   For some of them
data Software
  -- | Represents software which is distributed as a set of `dnf` packages
  = DnfPackages {
      sPackages    :: [String]
    , sPostInstall :: FilePath -> Command PathAndEnvironment
    }
  -- | Represents software which is located in a Git repository.
  --   Afterexecuting the commands it will be installed into ~/opt.
  | Repository {
      sOrigin   :: String
    , sLocation :: Location
    , sInstall  :: FilePath -> Command PathAndEnvironment
    }
  -- | An archive. After the extraction some commands might have to be executed.
  | Archive {
      sOrigin      :: String
    , sLocation    :: Location
    , sArchiveType :: ArchiveType
    , sPostInstall :: FilePath -> Command PathAndEnvironment
    }
  -- | This refers to one of the scripts bundled with the installer.
  --   After linking it to ~/bin some other commands might have to be executed.
  | Script {
      sScriptFile :: FilePath
    , sInstall    :: FilePath -> Command PathAndEnvironment
    }
