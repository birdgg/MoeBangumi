module Web.Tracing where

import Data.Aeson qualified as Aeson
import Data.Text.Display (display)
import Effectful.Exception qualified as E
import Effectful.Log
import Log qualified
import Network.Wai (Request)
import Infra.Environment.Config (DeploymentEnv)

handleExceptions
  :: Logger
  -> DeploymentEnv
  -> Maybe Request
  -> E.SomeException
  -> IO ()
handleExceptions logger environment mRequest e@(E.SomeException exception) = do
  Log.runLogT "moe-production" logger LogAttention $ do
    let context = E.displayExceptionContext $ E.someExceptionContext e
    Log.logAttention "Unhandled exception" $
      Aeson.object
        [ "exception" .= display (show exception)
        , "backtraces" .= context
        ]


