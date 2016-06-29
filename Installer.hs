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
import           Data.Either.Combinators    (whenLeft)
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist, getHomeDirectory)
import           System.FilePath            (takeFileName, (</>))
import           System.Posix.Files         (createSymbolicLink, fileExist,
                                             getFileStatus,
                                             getSymbolicLinkStatus,
                                             isSymbolicLink)

import           SymbolicLinkDsl
import           Software

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
    config <- installSoftware [
        DnfPackages ["gcc", "make"] none
      ]
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
