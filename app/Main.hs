module Main where

import Core
import qualified Data.Text.IO as TIO
import Options.Applicative

data Option = Option { fileName :: FilePath }

parser :: Parser Option
parser = Option <$> strArgument
  ( metavar "FILE"
   <> help "Path to the input file containing the program"
  )

cliParser :: ParserInfo Option
cliParser = info
  (parser <**> helper)
  ( fullDesc
   <> progDesc "Dump G-machine execution trace from a given file"
   <> header "core - G-machine implementation of lazy programming language Core"
  )

main :: IO ()
main = do
  fileName <- fileName <$> execParser cliParser
  program <- TIO.readFile fileName
  TIO.putStrLn . dumpCoreTrace $ program
