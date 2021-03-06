module Main (main) where

import Prelude hiding (words)
import Anagram
import Control.Monad
import Options.Applicative
import Data.List hiding (words)
import Text.Printf
import Control.DeepSeq (force)
import Control.Exception (evaluate)

-- | Finds anagrams
-- If words are given on the command-line anagrams are found for these
-- words, printed to stdout and then the program exits. Otherwise
-- the program will run in interactive mode continually prompting
-- for new words to find anagrams for until the program is interrupted.
main :: IO ()
main = do
    params <- execParser options
    ana  <- anagrams <$> (getWords . dictFile) params >>= evaluate . force
    case words params of
        [] -> forever $ interactive ana
        xs -> xs `forM_` \ x -> printResult x (ana x)

interactive :: (String -> [String]) -> IO ()
interactive ana = getLine >>= \ x -> printResult x (ana x)

printResult :: String -> [String] -> IO ()
printResult x y = printf "%s -> %s\n" x $ intercalate "," y

getWords :: FilePath -> IO [String]
getWords p = lines <$> readFile p

data Params = Params
    { words    :: [String]
    , dictFile :: FilePath
    }

paramsP :: Parser Params
paramsP = Params
    <$> (many . strArgument)
        (  metavar "WORDS"
        <> help "The words to find anagrams for"
        )
    <*> dictOption where
        dictOption = strOption
            (  long "dict"
            <> metavar "PATH"
            <> help "Path to the dictionary to use"
            )
            <|> pure "/usr/share/dict/words"

options :: ParserInfo Params
options = info (helper <*> paramsP)
    (  fullDesc
    <> progDesc
        (  "Finds anagrams in WORDS. If WORDS are not present the program "
        ++ "runs in interactive mode."
        )
    )
