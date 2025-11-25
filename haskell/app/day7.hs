{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import qualified Data.Text as T
import Data.List (foldl')

data FSEntry = File Int T.Text | Dir T.Text [FSEntry] deriving (Show)
data Command = Chdir T.Text | Backup | Ls deriving (Show)

-- | parseCommand
-- >>> parseCommand (T.pack "$ cd ..")
-- Just Backup
-- >>> parseCommand (T.pack "$ cd a")
-- Just (Chdir "a")
parseCommand :: T.Text -> Maybe Command
parseCommand s =
    case T.stripPrefix "$ cd " s of
        Nothing -> if s == "ls" then Just Ls else Nothing
        Just dir -> if dir == ".." then Just Backup else Just $ Chdir dir

-- | parseFSEntry
-- >>> parseFSEntry (T.pack "62596 h.lst")
-- Just (File 62596 "h.lst")
-- >>> parseFSEntry (T.pack "dir a")
-- Just (Dir "a" [])
parseFSEntry :: T.Text -> Maybe FSEntry
parseFSEntry s =
    case T.stripPrefix "dir " s of
        Nothing ->
            if T.isPrefixOf "cd" s then
                Nothing
            else case T.splitOn " " s of
                [numStr, fileName] -> Just $ File (read $ T.unpack numStr) fileName
                _ -> Nothing
        Just dir -> Just $ Dir dir []

isCommand :: T.Text -> Bool
isCommand = T.isPrefixOf "$ "

-- | parseDirectoryContents
-- >>> parseDirectoryContents [T.pack "dir e", T.pack "62596 h.lst", T.pack "cd .."] []
-- (["cd .."],[File 62596 "h.lst",Dir "e" []])
parseDirectoryContents :: [T.Text] -> [FSEntry] -> ([T.Text], [FSEntry])
parseDirectoryContents [] entries = ([], entries)
parseDirectoryContents (s : xs) entries =
    case parseFSEntry s of
        Just fs -> parseDirectoryContents xs (fs : entries)
        Nothing -> (s : xs, entries)

-- parseFileList :: [T.Text] -> FSEntry
-- parseFileList =
--     foldl' (\acc i ->

--         )

part1 :: T.Text -> Int
part1 _ = 1

part2 :: T.Text -> Int
part2 _ = 1
