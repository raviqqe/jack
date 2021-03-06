module Args (
  Args(..),
  parseArgs,
) where

import Options.Applicative
import Control.Monad


-- types

data Args = Args { sourceCodeFilename :: Maybe FilePath, debug :: Bool }
            deriving Show


-- functions

args :: Parser Args
args = Args <$> argument (optional str)
                         ( metavar "FILE"
                        <> value Nothing)
            <*> switch ( short 'd'
                      <> long "debug"
                      <> help "switch debug mode" )


parseArgs :: IO Args
parseArgs = do
  let argsWithHelp = info (helper <*> args)
                          ( fullDesc
                         <> progDesc "Compile source code \
                                     \of Jack programming language"
                         <> header "Jack Programming Language Compiler" )
  execParser argsWithHelp
