
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import System.Environment hiding (getEnv)
import Network.JobQueue.Backend
import Network.JobQueue
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Class

main :: IO ()
main = do
  args <- getArgs
  case args of
    (loc:name:args') -> do
      b@(Backend { bOpenQueue = oq }) <- openBackend loc
      bq <- oq name
      mtop <- peekQueue bq
      case mtop of
        Just top -> case top of
          (job, _, _, _) -> print job
        Nothing -> return ()
      closeQueue bq
      bClose b
    _ -> return ()

