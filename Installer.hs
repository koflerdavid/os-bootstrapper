#!/usr/bin/env stack
{- stack
   --resolver lts-6.5
   --system-ghc
   --install-ghc
   runghc
   --package classy-prelude
   --package either
   --package filepath
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{- OPTIONS_HADDOCK show-extensions -}

{-|
Module      : Installer
Description : Setups a user account
Copyright   : (c) David Kofler, 2016
License     : BSD3
Maintainer  : kofler.david@email.com
Stability   : experimental
Portability : POSIX

This script setups a fresh Linux user account according to the author's preferences.
-}

import           ClassyPrelude

import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import           Data.Either.Combinators    (whenLeft)
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist, getHomeDirectory)
import           System.Exit                (ExitCode (..))
import           System.FilePath            (takeFileName, (</>))
import           System.Posix.Files         (createSymbolicLink, fileExist,
                                             getFileStatus,
                                             getSymbolicLinkStatus,
                                             isSymbolicLink)
import           System.Process             (showCommandForUser, system)


import           Packages
import           Software
import           SymbolicLinkDsl

main :: IO ()
main = putStrLn ">>> Welcome to the account installer" >> setupUserAccount

setupUserAccount :: IO ()
setupUserAccount = do
  result <- runEitherT $ do
    home <- lift getHomeDirectory
    createDirectories [
        home </> "bin" -- Links to binaries of ~/opt apps
      , home </> "opt" -- For apps which ought to have their own directory tree
      , home </> "pasttimes" -- Non-work non-uni related stuff
      , home </> "projects" -- The author's projects involving coding
      , home </> "src" -- 3rd party source code, mostly mirrors of repositories
      , home </> "uni"
      ]
    config <- installSoftware packages
    createSymlinks [
        home </> "Videos" `_in` home `as` "dwhelper"
      ]

  whenLeft result (putStrLn . (">>> Error: " ++))

-- | This function creates the nontrivial directory structure which the author
-- prefers to have available.
createDirectories :: [FilePath] -> EitherT Text IO ()
createDirectories = lift . traverse_ (createDirectoryIfMissing True) -- Also create parent directories

configureBash :: ([FilePath], [(String, String)]) -> EitherT Text IO ()
configureBash (pathDirectories, variables) = return ()

createSymlinks :: [(FilePath, FilePath, Maybe FilePath)] -> EitherT Text IO ()
createSymlinks = lift . traverse_ createSymlink
  where
    createSymlink (destination, location, mName) = do
      -- Use the basename of the destination as name of the link as default
      let name = fromMaybe (takeFileName destination) mName
      let linkPath = location </> takeFileName name
      unlessM (symbolicLinkExists linkPath) $
        createSymbolicLink destination linkPath

symbolicLinkExists :: FilePath -> IO Bool
symbolicLinkExists path = do
  fileExist path <&&> isSymbolicLink <$> getSymbolicLinkStatus path

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
