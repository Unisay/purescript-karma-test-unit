module Test.Unit.Karma 
  ( runKarma 
  ) where

import Prelude

import Control.Monad.Aff (attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Free (resume)
import Control.Monad.State (State, execState, modify)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Test.Unit (Group(Group), TestF(SkipUnit, TestUnit, TestGroup), TestSuite, walkSuite)
import Test.Unit.Main (run, runTestWith)

type Run eff
   = ({total :: Int} -> Eff eff Unit)
  -> ({id :: String, description :: String, suite :: Array String, log :: Array String, success :: Boolean, skipped :: Boolean} -> Eff eff Unit)
  -> Eff eff Unit
  -> Eff eff Unit

foreign import _runKarma
  :: forall eff
   . Run eff
  -> Eff eff Unit

countTests :: forall eff. TestSuite eff -> State Int Unit
countTests tst =
  case resume tst of
    Left (TestGroup (Group label tst') skip only rest) ->
      do
        countTests tst' 
        countTests rest
    Left (TestUnit label _ _ _ rest) -> 
      do
        modify (add 1)
        countTests rest
    Left (SkipUnit rest) -> countTests rest
    Right _ -> pure unit

-- | Run a test suite using karma test runner.
runKarma
  :: forall eff
   . TestSuite (console :: CONSOLE, avar :: AVAR | eff)
  -> Eff (console :: CONSOLE, avar :: AVAR | eff) Unit
runKarma = _runKarma <<< createRunner
  where
  createRunner
    :: TestSuite (console :: CONSOLE, avar :: AVAR | eff)
    -> Run (console :: CONSOLE, avar :: AVAR | eff)
  createRunner suite = \info result complete ->
    let
      total = execState (countTests suite) 0
      karmaRunner tst = walkSuite runSuiteItem tst
        where
          runSuiteItem path (TestGroup (Group label content) _ _ _) = do
            pure unit
          runSuiteItem path t'@(TestUnit label _ _ t rest) = do
            res <- attempt t
            case res of
              (Right _) -> do
                liftEff $ result {id: label, suite: (foldl A.snoc [] path), description: label, log: [], success: true, skipped: false}
              (Left err) -> do
                liftEff $ result {id: label, suite: (foldl A.snoc [] path), description: label, log: [message err], success: false, skipped: false}
            pure unit
          runSuiteItem path (SkipUnit rest) = do
            pure unit
      in do
        info { total }
        run $ runTestWith karmaRunner suite
        complete
