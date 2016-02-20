import Prelude hiding (words)
import Anagram
import System.Environment
import Control.Monad
import Options.Applicative

main :: IO ()
main = do
    params <- execParser options
    ana  <- fmap anagrams $ getWords (dictFile params)
    case words params of
        [] -> forever $ interactive ana
        xs -> map ana xs `forM_` \as -> do
            as `forM_` (\a -> putStr (a ++ ", "))
            putStrLn ""

interactive :: (String -> [String]) -> IO ()
interactive ana = getLine >>= p . ana where
    p as = do
        as `forM_` \a -> putStr (a ++ ", ")
        putStrLn ""

getWords :: FilePath -> IO [String]
getWords p = lines `fmap` readFile p

data Params = Params
    { words    :: [String]
    , dictFile :: FilePath
    }

params :: Parser Params
params = Params
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

options = info (helper <*> params)
    (  fullDesc
    <> progDesc
        (  "Finds anagrams in WORDS. If WORDS are not present the program "
        ++ "runs in interactive mode."
        )
    )
