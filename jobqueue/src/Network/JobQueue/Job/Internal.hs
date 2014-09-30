{-# LANGUAGE TemplateHaskell #-}

module Network.JobQueue.Job.Internal where

import Data.Char
import Data.Time.Clock
import System.Log.Logger
import System.IO
import Data.Aeson.TH

import Network.JobQueue.Class

data JobState = Initialized | Runnable | Running | Aborted | Finished
  deriving (Show, Read, Eq)

$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''JobState)

{- | Job control block
Job consists of /State/, /Unit/, /CTime/, /OnTime/, /Id/, /Group/, and /Priority/.

- State - takes one of 5 states (initialized, runnable, running, aborted and finished)

- Unit - an instance of Unit class, which is specified by type parameter of Job data type

- CTime - creation time

- OnTime - the time at which this job starts

- Id - Identifier of this job

- Group - Group ID of this job

- Priority - the priority of this job

-}
data Job a =
    Job {
      jobState    :: JobState
    , jobUnit     :: a
    , jobCTime    :: UTCTime
    , jobOnTime   :: UTCTime
    , jobId       :: Int
    , jobGroup    :: Int
    , jobPriority :: Int }
  | StopTheWorld
  deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 3, constructorTagModifier = map toLower } ''Job)

createJob :: (Unit a) => JobState -> a -> IO (Job a)
createJob state unit = do
  ctime <- getCurrentTime
  return (Job state unit ctime ctime (defaultId) (defaultGroup) (getPriority unit))

createOnTimeJob :: (Unit a) => JobState -> UTCTime -> a -> IO (Job a)
createOnTimeJob state ontime unit = do
  ctime <- getCurrentTime
  return (Job state unit ctime ontime (defaultId) (defaultGroup) (getPriority unit))

printJob :: (Unit a) => Job a -> IO ()
printJob job = case job of
  Job {} -> do
    noticeM "job" $ show (jobUnit job)
    hPutStrLn stdout $ desc (jobUnit job)
    hFlush stdout
  StopTheWorld -> return ()

---------------------------------------------------------------- PRIVATE

defaultId :: Int
defaultId = -1

defaultGroup :: Int
defaultGroup = -1
