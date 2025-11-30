{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (traceShow, trace)

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
        Nothing -> if s == "$ ls" then Just Ls else Nothing
        Just dir -> if dir == ".." then Just Backup else Just $ Chdir dir

-- | parseFSEntry
-- >>> parseFSEntry (T.pack "62596 h.lst")
-- Just (File 62596 "h.lst")
-- >>> parseFSEntry (T.pack "dir a")
-- Just (Dir "a" [])
-- >>> parseFSEntry (T.pack "584 i")
-- Just (File 584 "i")
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
-- >>> parseDirectoryContents [T.pack "dir e", T.pack "62596 h.lst", T.pack "$ cd .."] []
-- (["$ cd .."],[File 62596 "h.lst",Dir "e" []])
-- >>> parseDirectoryContents [T.pack "584 i", T.pack "$ cd ..", T.pack "$ cd .."] []
-- (["$ cd ..","$ cd .."],[File 584 "i"])
-- >>> parseDirectoryContents ["584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"] []
-- (["$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"],[File 584 "i"])
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

-- OK so for Clojure I just used atom which explains why this part is so ugly
-- What is the data structure I want in the end?
-- It's just the tree of the file system, which is an object like Dir ['/']
-- Stepping through requires we keep track of and pop context.
-- Is the only context we need directory names?

type DirPath = [T.Text]

updateIn :: FSEntry -> DirPath -> [FSEntry] -> FSEntry
updateIn (Dir dirname _) [] entries = Dir dirname entries
updateIn (Dir dirname contents) xs entries =
    let (xs', x) = (init xs, last xs) in
    Dir dirname (map (\entry ->
        case entry of
            File _ _ -> entry
            Dir dirname' _ ->
                if dirname' == x then
                    updateIn entry xs' entries
                    else entry) contents)
updateIn (File _ _) _ _ = error "can't update file"

-- text to parse
-- entire tree
-- current directory structure
parseInteractions :: FSEntry -> DirPath -> [T.Text] -> FSEntry
parseInteractions currentTree _ [] = currentTree
parseInteractions currentTree path (s : xs) =
    case fromJust $ parseCommand s of
        Chdir dir ->
            parseInteractions currentTree (dir:path) xs
        Ls ->
            let (xs', entries) = parseDirectoryContents xs [] in
                parseInteractions (updateIn currentTree path entries) path xs'
        Backup ->
            case path of
                [] -> error "invalid dirpath"
                _ : path' -> parseInteractions currentTree path' xs


-- | parseCommand
-- >>> s = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
-- >>> parseTree (T.splitOn "\n" s)
-- Dir "/" [Dir "d" [File 7214296 "k",File 5626152 "d.ext",File 8033020 "d.log",File 4060174 "j"],File 8504156 "c.dat",File 14848514 "b.txt",Dir "a" [File 62596 "h.lst",File 2557 "g",File 29116 "f",Dir "e" [File 584 "i"]]]
parseTree :: [T.Text] -> FSEntry
parseTree = parseInteractions (Dir "/" []) [] . tail

type DirectorySizeMap = M.Map T.Text Int

sizeCount :: DirectorySizeMap -> FSEntry -> (Int, DirectorySizeMap)
sizeCount m (File n _) = (n, m)
sizeCount m (Dir a contents) =
    let (n', m') = foldr (\entry (n, acc) -> let (k, j) = sizeCount acc entry in (n + k, j)) (0, m) contents in
        (n', M.insert a n' m')

-- | part1
-- >>> s = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
-- >>> part1 s
-- 95437
part1 :: T.Text -> Int
part1 = M.foldr (\s n -> if s < 100000 then s + n else n) 0 . snd . sizeCount M.empty . parseTree . T.splitOn "\n"

part2 :: T.Text -> Int
part2 _ = 1
