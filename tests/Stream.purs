module Test.Stream (main, test) where

import Control.Monad.Eff.Ref
import qualified Control.Timer as T

import Data.Date

import Test.Assert.Simple
import Test.PSpec hiding (skip)
import Test.PSpec.Mocha

import FRP.Kefir

assertAbout e a = if e * 0.9 < a && a < e * 1.1 then return unit else assertFailure msg
  where
    msg = "expected: about " ++ show e ++ " but got: " ++ show a

main = runMocha test

test = do
  describe "stream" $ do
    describe "emitter" $ do
      itAsync "should take value" $ \done -> do
        e <- emitter
        onValue e $ \v -> do
          v @?= "foo"
          itIs done

        onEnd e $ itIsNot done "end"
        emit e "foo"

      itAsync "should take end" $ \done -> do
        e <- emitter
        onValue e $ \_ -> do
          itIsNot done "value"
        onEnd e $ itIs done
        end e

    describe "never" $
      itAsync "should send end" $ \done -> do
        n <- never
        onEnd n $ itIs done

    describe "later" $ do
      itAsync "should emit 1 value and end, 100ms later." $ \done -> do
        st <- now
        l  <- later 50 "bar"
        onValue l $ \v -> do
          v @?= "bar"
        onEnd l $ do
          ed <- now
          assertAbout 50 (toEpochMilliseconds ed - toEpochMilliseconds st)
          itIs done

    describe "interval" $ do
      itAsync "should emit same values, forever." $ \done -> do
        i <- interval 50 "baz"
        r <- newRef ""
        k <- onValue i $ \v -> modifyRef r (\a -> a ++ v)
        T.timeout 175 $ do
          v <- readRef r
          v @?= "bazbazbaz"
          off k
          itIs done

    describe "sequentially" $ do
      itAsync "should emit values, and end" $ \done -> do
        s <- sequentially 50 ["foo", "bar", "baz"]
        r <- newRef ""
        onValue s $ \v -> modifyRef r (\a -> a ++ v)
        onEnd s $ do
          v <- readRef r
          v @?= "foobarbaz"
          itIs done

    describe "repeatedly" $ do
      itAsync "should emit values forever" $ \done -> do
        p <- repeatedly 50 ["foo", "bar"]
        r <- newRef ""
        k <- onValue p $ \v -> modifyRef r (\a -> a ++ v)
        T.timeout 175 $ do
          v <- readRef r
          v @?= "foobarfoo"
          off k
          itIs done

    describe "fromPoll" $ do
      itAsync "should polling" $ \done -> do
        c <- newRef 7
        p <- fromPoll 50 $ modifyRef' c (\i -> {retVal: i, newState: i * 2})
        r <- newRef 0
        k <- onValue p $ \v -> modifyRef r (\a -> a + v)
        T.timeout 125 $ do
          v <- readRef r
          v @?= 7 * 3
          off k
          itIs done

    describe "withInterval" $
      itAsync "should send 3 msg and end." $ \done -> do
        c <- newRef 0
        i <- withInterval 50 $ \em -> do
          n <- modifyRef' c $ \i -> {retVal: i, newState: i + 1}
          if n < 3 then emit em "foo" else end em

        r <- newRef ""
        onValue i $ \v -> modifyRef r (\a -> a ++ v)
        onEnd i $ do
          v <- readRef r
          v @?= "foofoofoo"
          itIs done

    describe "fromCallback" $
      itAsync "sould send once and end." $ \done -> do
        f <- fromCallback $ \cb -> cb "bar"
        r <- newRef ""
        onValue f $ \v -> modifyRef r (\a -> a ++ v)
        onEnd f $ do
          v <- readRef r
          v @?= "bar"
          itIs done

    describe "fromBinder" $
      itAsync "" $ \done -> do
        r <- newRef ""

        b <- fromBinder $ \e -> do
          emit e "emit"
          T.timeout 50 $ end e
          return $ do
            modifyRef r (\a -> a ++ "end")

        onValue b $ \v -> modifyRef r (\a -> a ++ v)
        onEnd b $ T.timeout 50 $ do
          v <- readRef r
          v @?= "emitend"
          itIs done

  describe "property" $
    describe "constant" $
      itAsync "must send constant and end" $ \done -> do
        c <- constant "foo"

        r <- newRef ""
        onValue c $ \v -> modifyRef r ((++) v)
        onEnd c $ do
          v <- readRef r
          v @?= "foo"
          itIs done
