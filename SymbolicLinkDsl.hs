module SymbolicLinkDsl (
  as
, _in
) where

_in :: FilePath -> FilePath -> (FilePath, FilePath, Maybe FilePath)
_in destination location = (destination, location, Nothing)

as :: (FilePath, FilePath, Maybe FilePath) -> FilePath -> (FilePath, FilePath, Maybe FilePath)
as (destination, location, _) linkName = (destination, location, Just linkName)

-- Make them less binding than the </> operator.
-- They are intended to be used as dest `in` path `as` name
infixl 4 `_in`
infixl 4 `as`
