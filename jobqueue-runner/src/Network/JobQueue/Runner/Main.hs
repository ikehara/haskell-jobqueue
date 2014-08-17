
module Network.JobQueue.Runner.Main where

import qualified Language.Haskell.Interpreter as I

import Network.JobQueue
import Control.Monad

defaultMain = do
  boot <- readFile "bootstrap.hs"
  result <- run boot
  case result of
    Right r -> r
    Left er -> case er of
      I.UnknownError e -> putStrLn e
      I.WontCompile es -> forM_ es $ \e -> case e of
        I.GhcError { I.errMsg = msg } -> putStrLn msg
      I.NotAllowed e   -> putStrLn e
      I.GhcException e -> putStrLn e

run :: String -> IO (Either I.InterpreterError (IO ()))
run source = I.runInterpreter $ do
  I.loadModules ["env.hs", "main.hs"]
  I.setImportsQ
    [ ("Prelude", Nothing)
    , ("Network.JobQueue", Nothing)
    , ("Control.Monad", Nothing)
    , ("Data.Default", Nothing)
    , ("Env", Nothing)
    , ("Main", Nothing)
    ]
  I.interpret source (I.as :: IO ())
