{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests remote verification on the server side (i.e. no GHCJS involvement)
module RemoteVerifySpec (spec) where

import Test.Hspec

import Control.Concurrent
import Control.Exception (finally)
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import Data.Bifunctor (first)
import Data.Either
import Data.Text (Text, unpack)

import NeatInterpolation (text)

import qualified Network.HTTP.Client as HTTP

import Servant.Client

import Pact.Analyze.Remote.Server (runServantServer)
import qualified Pact.Analyze.Remote.Types as Remote
import Pact.Repl
import Pact.Repl.Types
import Pact.Server.API
import Pact.Types.Runtime

#if ! MIN_VERSION_servant_client(0,16,0)
type ClientError = ServantError
#endif

spec :: Spec
spec = do
  describe "single module"                       testSingleModule
  describe "multiple modules, sent out of order" testUnsortedModules

data TestFailure
  = ReplError String
  deriving Show

loadCode :: Text -> IO (Either TestFailure ReplState)
loadCode code = do
  replState0 <- initReplState StringEval (Just "http://localhost:3000")
  (eTerm, replState) <- runStateT (evalRepl' $ unpack code) replState0
  pure $ case eTerm of
    Left err -> Left $ ReplError err
    Right _t -> Right replState

stateModuleData :: ModuleName -> ReplState -> IO (Either String (ModuleData Ref))
stateModuleData nm replState = replLookupModule replState nm

serve :: Int -> IO ThreadId
serve port = forkIO $ runServantServer port

serveAndRequest :: Int -> Remote.Request -> IO (Either ClientError Remote.Response)
serveAndRequest port body = do
  let url = "http://localhost:" ++ show port
  verifyBaseUrl <- parseBaseUrl url
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  let clientEnv = mkClientEnv mgr verifyBaseUrl
  tid <- serve port
  finally
    (runClientM (verifyClient body) clientEnv)
    (killThread tid)

mkNamespaces :: Text
mkNamespaces = [text|
  (module ns MODULE_ADMIN
    (defcap MODULE_ADMIN () true)

    (defun success ()
      true)
    (defun failure ()
      (enforce false "Disabled"))

    (defconst GUARD_SUCCESS (create-user-guard (success)))
    (defconst GUARD_FAILURE (create-user-guard (failure))))

  (define-namespace 'ns1 GUARD_SUCCESS GUARD_FAILURE)
  (define-namespace 'ns2 GUARD_SUCCESS GUARD_FAILURE)
|]

testSingleModule :: Spec
testSingleModule = do
  replState0 <- runIO $ either (error.show) id <$> loadCode
    [text|
      (env-keys ["admin"])
      (env-data { "keyset": { "keys": ["admin"], "pred": "=" } })
      (begin-tx)

      (define-keyset 'ks (read-keyset "keyset"))
      $mkNamespaces
      (namespace 'ns1)
      (module mod1 'ks
        (defun f:integer ()
          @doc   "always returns 1"
          @model [(property (= result 1))]
          1))

      (commit-tx)
    |]

  let mn1 = "ns1.mod1"
  it "loads locally" $ do
    stateModuleData mn1 replState0 >>= (`shouldSatisfy` isRight)

  (ModuleData mod1 _refs) <- runIO $ either error id <$> stateModuleData mn1 replState0

  resp <- runIO $ serveAndRequest 3000 $ Remote.Request [derefDef <$> mod1] mn1

  it "verifies over the network" $
    fmap (view Remote.responseLines) resp `shouldBe`
    (Right ["Property proven valid",""])

testUnsortedModules :: Spec
testUnsortedModules = do
  replState0 <- runIO $ either (error . show) id <$> loadCode code

  let mn1 = "ns1.mod1"
      mn2 = "ns2.mod2"

  it "loads when topologically sorted locally" $ do
    stateModuleData "mod2" replState0 >>= (`shouldSatisfy` isRight)

  resp <- runIO . runExceptT $ do
    ModuleData mod1 _refs <- ExceptT $ stateModuleData mn1 replState0
    ModuleData mod2 _refs <- ExceptT $ stateModuleData mn2 replState0
    ExceptT . fmap (first show) . serveAndRequest 3001 $
      Remote.Request [derefDef <$> mod2, derefDef <$> mod1] mn2

  it "verifies over the network" $
    fmap (view Remote.responseLines) resp `shouldBe`
    (Right ["Property proven valid",""])
  where
    code = [text|
      (env-keys ["admin"])
      (env-data { "keyset": { "keys": ["admin"], "pred": "=" } })
      (begin-tx)
      (define-keyset 'ks (read-keyset "keyset"))
      $mkNamespaces
      (namespace 'ns1)
      (module mod1 'ks
        (defun f:integer ()
          @doc   "always returns 1"
          @model [(property (= result 1))]
          1))
      (commit-tx)
      (begin-tx)
      (define-keyset 'ks2 (read-keyset "keyset"))
      (namespace 'ns2)
      (module mod2 'ks2
        (use ns1.mod1)
        (defun g:integer ()
          @doc   "always returns 2"
          @model [(property (= result 2))]
          2))
      (commit-tx)
    |]
