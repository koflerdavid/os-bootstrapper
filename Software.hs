{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Software where

import           ClassyPrelude

import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import           System.Exit                (ExitCode (..))
import           System.Process             (showCommandForUser, system)

type Command = EitherT Text IO

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

type PathAndEnvironment = ([FilePath], [FilePath], [(String, String)])

-- | This function installs all software. It returns a list of all directories
-- which shall be added to the PATH and all necessary environment variables
-- which have to be set in `./.bash_profile
installSoftware :: [Software] -> Command ([FilePath], [(String, String)])
installSoftware softwares = do
    (prependedDirectories, directories, vars) <- execStateT installSoftware' ([], [], [])
    return (prependedDirectories ++ directories, vars)
  where
    installSoftware' = void $ traverse install softwares

install :: Software -> StateT PathAndEnvironment Command ()
install (DnfPackages packages postInstall) = do
  let installCommand = showCommandForUser
  exitCode <- sudo $ ["dnf", "--assumeyes", "install"] ++ packages
  case exitCode of
    ExitFailure code | code /= 1 ->
      lift $ left $ asText $ "`dnf` quit with exit code " ++ tshow code
    _ -> do
      -- Absorb the paths and environent variables required by postInstall
      (prePaths', paths', vars') <- lift $ postInstall ""
      modify $ \ (prePaths, paths, vars) ->
        (prePaths' ++ prePaths, paths' ++ paths, vars' ++ vars)

install _ = lift $ left $ "Not yet supported"

sudo :: [String] -> StateT PathAndEnvironment Command ExitCode
sudo command = lift $ lift $ system $ showCommandForUser "sudo" command

addDirectoryToPath path = modify (\ (prepended, paths, vars) -> (prepended, path:paths, vars))
prependDirectoryToPath path = modify (\ (prepended, paths, vars) -> (path:prepended, paths, vars))
