module NameSupply (
  NameSupply,
  uniqueName,
  empty,
  insert
) where

import qualified Data.Map as Map



type NameSupply = Map.Map String Int

empty :: NameSupply
empty = Map.empty

uniqueName :: String -> NameSupply -> (String, NameSupply)
uniqueName name names =
  case Map.lookup name names of
    Nothing -> (name, insert name names)
    Just index -> let newIndex = index + 1
                  in (name ++ show newIndex, Map.insert name newIndex names)

insert :: String -> NameSupply -> NameSupply
insert name names | Map.member name names = names
                  | otherwise             = Map.insert name initialIndex names
  where
    initialIndex = 0
