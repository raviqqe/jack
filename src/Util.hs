module Util (
  liftExceptT,
  printMessage
) where

import Control.Monad.Except



liftExceptT :: ExceptT String IO a -> IO a
liftExceptT = runExceptT >=> either fail return

printMessage :: Either String () -> IO ()
printMessage (Left message) = putStrLn message
printMessage (Right _) = return ()
