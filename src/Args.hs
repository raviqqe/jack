module Args (
  Args(..),
  parseArgs,
) where

import Options.Applicative
import Control.Monad


-- types

data Args = Args { sourceCodeFilename :: Maybe FilePath,
                   objectFilename :: Maybe FilePath,
                   debug :: Bool }
            deriving Show


-- functions

args :: Parser Args
args = Args <$> argument (optional str)
                         (metavar "FILE"
                       <> value Nothing)
            <*> optional (strOption (short 'o'
                                  <> long "object-file"
                                  <> metavar "OBJECT_FILE"
                                  <> help "object file of output" ))
            <*> switch (short 'd'
                     <> long "debug"
                     <> help "toggle debug mode" )


parseArgs :: IO Args
parseArgs = execParser argsWithHelp
  where
    argsWithHelp = info (helper <*> args)
                        (fullDesc
                      <> progDesc "Compile source code \
                                  \of Jack programming language"
                      <> header "Jack Programming Language Compiler" )
